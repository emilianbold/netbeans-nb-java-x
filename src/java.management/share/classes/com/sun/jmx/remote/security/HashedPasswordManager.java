/*
 * Copyright (c) 2017, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package com.sun.jmx.remote.security;

import com.sun.jmx.remote.util.ClassLogger;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.FileLock;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * HashedPasswordManager loads passwords from the password file and optionally
 * hashes them.
 * <p>
 * <p>
 * This class accepts Unicode UTF-8 encoded file
 * <p>
 * <p>
 * Each entry in the password file contains a username followed by a password.
 * Password can be in clear text or as a hash. Hashed passwords must follow the
 * below format. hashedPassword = base64_encoded_64_byte_salt W
 * base64_encoded_hash W hash_algorithm where, W = spaces,
 * base64_encoded_64_byte_salt = 64 byte random salt, base64_encoded_hash =
 * hash_algorithm(password + salt), hash_algorithm = Algorithm string as
 * specified in
 * <a href="{@docRoot}/../specs/security/standard-names.html#messagedigest-algorithms">
 * </a>
 * hash_algorithm is an optional field. If not specified, SHA3-512 will be
 * assumed.
 * <p>
 * <p>
 * If passwords are in clear, they will be over-written by their hash if hashing
 * is requested by setting com.sun.management.jmxremote.password.toHashes
 * property to true in the management.properties file and if the password file
 * is writable and if the system security policy allows writing into the
 * password file, if a security manager is configured
 * <p>
 * <p>
 * In order to change the password for a role, replace the hashed password entry
 * with a new clear text password or a new hashed password. If the new password
 * is in clear, it will be replaced with its hash when a new login attempt is
 * made.
 * <p>
 * <p>
 * A given role should have at most one entry in this file. If a role has no
 * entry, it has no access. If multiple entries are found for the same role
 * name, then the last one will be used.
 * <p>
 * <p>
 * <p>
 * A user generated hashed password file can also be used instead of a
 * clear-text password file. If generated by the user, hashed passwords must
 * follow the format specified above.
 */
final public class HashedPasswordManager {

    private static final class UserCredentials {

        private final String userName;
        private final String hashAlgorithm;
        private final String b64Salt;
        private final String b64Hash;

        public UserCredentials(String userName, String hashAlgorithm, String b64Salt, String b64Hash) {
            this.userName = userName;
            this.hashAlgorithm = hashAlgorithm;
            this.b64Salt = b64Salt;
            this.b64Hash = b64Hash;
        }

        @Override
        public String toString() {
            return userName + " " + b64Salt + " " + b64Hash + " " + hashAlgorithm;
        }
    }

    private static final String DefaultHashAlgorithm = "SHA3-512";
    private static final int DefaultSaltLength = 64;

    private final SecureRandom random = new SecureRandom();
    private final Map<String, UserCredentials> userCredentialsMap = new HashMap<>();
    private final String passwordFile;
    private final boolean shouldHashPasswords;
    private boolean isLogged = false;

    private static final ClassLogger logger
            = new ClassLogger("javax.management.remote.misc",
                    "HashedPasswordManager");

    /**
     * Creates a new password manager for the input password file
     *
     * @param filename UTF-8 encoded input file to read passwords from
     * @param shouldHashPasswords Request for clear passwords to be hashed
     */
    public HashedPasswordManager(String filename, boolean shouldHashPasswords) {
        this.passwordFile = filename;
        this.shouldHashPasswords = shouldHashPasswords;
    }

    private String[] getHash(String algorithm, String password) {
        try {
            byte[] salt = new byte[DefaultSaltLength];
            random.nextBytes(salt);

            MessageDigest digest = MessageDigest.getInstance(algorithm);
            digest.reset();
            digest.update(salt);
            byte[] hash = digest.digest(password.getBytes(StandardCharsets.UTF_8));
            String saltStr = Base64.getEncoder().encodeToString(salt);
            String hashStr = Base64.getEncoder().encodeToString(hash);

            return new String[]{saltStr, hashStr};
        } catch (NoSuchAlgorithmException ex) {
            if (logger.debugOn()) {
                logger.debug("getHash", "Invalid algorithm : " + algorithm);
            }
            // We should never reach here as default Hash Algorithm
            // must be always present
            return new String[]{"", ""};
        }
    }

    private String[] readPasswordFile() throws IOException {
        synchronized (HashedPasswordManager.class) {
            byte[] data;
            File f = new File(passwordFile);
            try (FileInputStream fin = new FileInputStream(f);
                    FileLock lock = fin.getChannel().lock(0L, Long.MAX_VALUE, true)) {
                data = new byte[(int) f.length()];
                int read = fin.read(data);
                if (read != data.length) {
                    throw new IOException("Failed to read data from the password file");
                }
                lock.release();
            }
            String str = new String(data, StandardCharsets.UTF_8);
            return str.split("\\r?\\n");
        }
    }

    private void writePasswordFile(String input) throws IOException {
        synchronized (HashedPasswordManager.class) {
            try (FileOutputStream fout = new FileOutputStream(passwordFile);
                    OutputStreamWriter out = new OutputStreamWriter(fout, StandardCharsets.UTF_8);
                    FileLock lock = fout.getChannel().lock()) {
                out.write(input);
                lock.release();
            }
        }
    }

    /**
     * Authenticate the supplied credentials against the one present in the file
     *
     * @param userName Input username
     * @param inputPassword Input password
     * @return true if authentication succeeds, false otherwise
     */
    public synchronized boolean authenticate(String userName, char[] inputPassword) {
        if (userCredentialsMap.containsKey(userName)) {
            try {
                UserCredentials us = userCredentialsMap.get(userName);
                byte[] salt = Base64.getDecoder().decode(us.b64Salt);
                byte[] targetHash = Base64.getDecoder().decode(us.b64Hash);
                MessageDigest digest = MessageDigest.getInstance(us.hashAlgorithm);
                digest.reset();
                digest.update(salt);
                ByteBuffer byteBuffer = Charset.forName("UTF-8").encode(CharBuffer.wrap(inputPassword));
                byte[] passwordBytes = new byte[byteBuffer.limit()];
                byteBuffer.get(passwordBytes);
                byte[] hash = digest.digest(passwordBytes);
                return Arrays.equals(hash, targetHash);
            } catch (NoSuchAlgorithmException ex) {
                if (logger.debugOn()) {
                    logger.debug("authenticate", "Unrecognized hash algorithm : "
                            + userCredentialsMap.get(userName).hashAlgorithm
                            + " - for user : " + userName);
                }
                return false;
            }
        } else {
            if (logger.debugOn()) {
                logger.debug("authenticate", "Unknown user : " + userName);
            }
            return false;
        }
    }

    /**
     * Load passwords from the password file.
     * <p>
     * <p>
     * This method should be called for every login attempt to load new/changed
     * credentials, if any.
     * <p>
     * <p>
     * If hashing is requested, clear passwords will be over-written with their
     * SHA3-512 hash
     *
     * @throws IOException If unable to access the file
     * @throws SecurityException If read/write file permissions are not granted
     */
    public synchronized void loadPasswords()
            throws IOException, SecurityException {

        SecurityManager security = System.getSecurityManager();
        if (security != null) {
            security.checkRead(passwordFile);
        }

        AtomicBoolean hasClearPasswords = new AtomicBoolean(false);
        StringBuilder sbuf = new StringBuilder();
        final String header = "# The passwords in this file are hashed.\n"
                + "# In order to change the password for a role, replace the hashed "
                + "password entry\n"
                + "# with a clear text password or a new hashed password. "
                + "If the new password is in clear,\n# it will be replaced with its "
                + "hash when a new login attempt is made.\n\n";

        userCredentialsMap.clear();
        Arrays.stream(readPasswordFile()).forEach(line -> {
            if (line.trim().startsWith("#")) {   // Ignore comments
                sbuf.append(line).append("\n");
                return;
            }
            String[] tokens = line.split("\\s+");
            switch (tokens.length) {
                case 2: {
                    // Password is in clear
                    String[] b64str = getHash(DefaultHashAlgorithm, tokens[1]);
                    UserCredentials us = new UserCredentials(tokens[0], DefaultHashAlgorithm, b64str[0], b64str[1]);
                    sbuf.append(us.userName).append(" ").append(us.b64Salt).
                            append(" ").append(us.b64Hash).append(" ").
                            append(us.hashAlgorithm).append("\n");
                    if (userCredentialsMap.get(tokens[0]) != null) {
                        if (logger.debugOn()) {
                            logger.debug("loadPasswords", "Ignoring entry for role : " + tokens[0]);
                        }
                    }
                    userCredentialsMap.put(tokens[0], us);
                    hasClearPasswords.set(true);
                    if (logger.debugOn()) {
                        logger.debug("loadPasswords",
                                "Found atleast one clear password");
                    }
                    break;
                }
                case 3:
                case 4: {
                    // Passwords are hashed
                    UserCredentials us = new UserCredentials(tokens[0], (tokens.length == 4 ? tokens[3] : DefaultHashAlgorithm),
                            tokens[1], tokens[2]);
                    sbuf.append(line).append("\n");
                    if (userCredentialsMap.get(tokens[0]) != null) {
                        if (logger.debugOn()) {
                            logger.debug("loadPasswords", "Ignoring entry for role : " + tokens[0]);
                        }
                    }
                    userCredentialsMap.put(tokens[0], us);
                    break;
                }
                default:
                    sbuf.append(line).append("\n");
                    break;
            }
        });

        if (!shouldHashPasswords && hasClearPasswords.get()) {
            if (logger.debugOn()) {
                logger.debug("loadPasswords",
                        "Passwords in " + passwordFile + " are in clear but are requested "
                        + "not to be hashed !!!");
            }
        }

        // Check if header needs to be inserted
        if (sbuf.indexOf("# The passwords in this file are hashed") != 0) {
            sbuf.insert(0, header);
        }

        // Even if we are unable to write hashed passwords to password file,
        // passwords will be hashed in memory so that jvm heap dump should not
        // give away clear passwords
        if (shouldHashPasswords && hasClearPasswords.get()) {
            if (new File(passwordFile).canWrite()) {
                writePasswordFile(sbuf.toString());
                if (logger.debugOn()) {
                    logger.debug("loadPasswords",
                            "Wrote hashed passwords to file : " + passwordFile);
                }
            } else if (logger.debugOn() && !isLogged) {
                isLogged = true;
                logger.debug("loadPasswords",
                        "Passwords in " + passwordFile + " are in clear and password file is read-only. "
                        + "Passwords cannot be hashed !!!!");
            }
        }
    }
}
