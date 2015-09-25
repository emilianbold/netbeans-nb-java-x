/*
 * Copyright (c) 2002, 2015, Oracle and/or its affiliates. All rights reserved.
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

package sun.security.ssl;

import java.util.*;
import java.security.CryptoPrimitive;
import sun.security.ssl.CipherSuite.*;

/**
 * Type safe enum for an SSL/TLS protocol version. Instances are obtained
 * using the static factory methods or by referencing the static members
 * in this class. Member variables are final and can be accessed without
 * accessor methods.
 *
 * There is only ever one instance per supported protocol version, this
 * means == can be used for comparision instead of equals() if desired.
 *
 * Checks for a particular version number should generally take this form:
 *
 * <pre>{@code
 * if (protocolVersion.v >= ProtocolVersion.TLS10) {
 *   // TLS 1.0 code goes here
 * } else {
 *   // SSL 3.0 code here
 * }
 * }</pre>
 *
 * @author  Andreas Sterbenz
 * @since   1.4.1
 */
public final class ProtocolVersion implements Comparable<ProtocolVersion> {

    // The limit of maximum protocol version
    static final int LIMIT_MAX_VALUE = 0xFFFF;

    // The limit of minimum protocol version
    static final int LIMIT_MIN_VALUE = 0x0000;

    // Dummy protocol version value for invalid SSLSession
    static final ProtocolVersion NONE = new ProtocolVersion(-1, "NONE");

    // If enabled, send/accept SSLv2 hello messages
    static final ProtocolVersion SSL20Hello =
                                new ProtocolVersion(0x0002, "SSLv2Hello");

    // SSL 3.0
    static final ProtocolVersion SSL30 = new ProtocolVersion(0x0300, "SSLv3");

    // TLS 1.0
    static final ProtocolVersion TLS10 = new ProtocolVersion(0x0301, "TLSv1");

    // TLS 1.1
    static final ProtocolVersion TLS11 = new ProtocolVersion(0x0302, "TLSv1.1");

    // TLS 1.2
    static final ProtocolVersion TLS12 = new ProtocolVersion(0x0303, "TLSv1.2");

    // DTLS 1.0
    // {254, 255}, the version value of DTLS 1.0.
    static final ProtocolVersion DTLS10 =
                                new ProtocolVersion(0xFEFF, "DTLSv1.0");

    // No DTLS 1.1, that version number was skipped in order to harmonize
    // version numbers with TLS.

    // DTLS 1.2
    // {254, 253}, the version value of DTLS 1.2.
    static final ProtocolVersion DTLS12 =
                                new ProtocolVersion(0xFEFD, "DTLSv1.2");

    private static final boolean FIPS = SunJSSE.isFIPS();

    // minimum version we implement (SSL 3.0)
    static final ProtocolVersion MIN = FIPS ? TLS10 : SSL30;

    // maximum version we implement (TLS 1.2)
    static final ProtocolVersion MAX = TLS12;

    // SSL/TLS ProtocolVersion to use by default (TLS 1.2)
    static final ProtocolVersion DEFAULT_TLS = TLS12;

    // DTLS ProtocolVersion to use by default (TLS 1.2)
    static final ProtocolVersion DEFAULT_DTLS = DTLS12;

    // Default version for hello messages (SSLv2Hello)
    static final ProtocolVersion DEFAULT_HELLO = FIPS ? TLS10 : SSL30;

    // Available protocols
    //
    // Including all supported protocols except the disabled ones.
    static final Set<ProtocolVersion> availableProtocols;

    // version in 16 bit MSB format as it appears in records and
    // messages, i.e. 0x0301 for TLS 1.0
    public final int v;

    // major and minor version
    public final byte major, minor;

    // name used in JSSE (e.g. TLSv1 for TLS 1.0)
    final String name;

    // Initialize the available protocols.
    static {
        Set<ProtocolVersion> protocols = new HashSet<>(7);

        ProtocolVersion[] pvs = new ProtocolVersion[] {
                SSL20Hello, SSL30, TLS10, TLS11, TLS12, DTLS10, DTLS12};
        EnumSet<CryptoPrimitive> cryptoPrimitives =
            EnumSet.<CryptoPrimitive>of(CryptoPrimitive.KEY_AGREEMENT);
        for (ProtocolVersion p : pvs) {
            if (SSLAlgorithmConstraints.DEFAULT_SSL_ONLY.permits(
                    cryptoPrimitives, p.name, null)) {
                protocols.add(p);
            }
        }

        availableProtocols =
                Collections.<ProtocolVersion>unmodifiableSet(protocols);
    }

    // private
    private ProtocolVersion(int v, String name) {
        this.v = v;
        this.name = name;
        major = (byte)(v >>> 8);
        minor = (byte)(v & 0xFF);
    }

    // private
    private static ProtocolVersion valueOf(int v) {
        if (v == SSL30.v) {
            return SSL30;
        } else if (v == TLS10.v) {
            return TLS10;
        } else if (v == TLS11.v) {
            return TLS11;
        } else if (v == TLS12.v) {
            return TLS12;
        } else if (v == SSL20Hello.v) {
            return SSL20Hello;
        } else if (v == DTLS10.v) {
            return DTLS10;
        } else if (v == DTLS12.v) {
            return DTLS12;
        } else {
            int major = (v >>> 8) & 0xFF;
            int minor = v & 0xFF;
            return new ProtocolVersion(v, "Unknown-" + major + "." + minor);
        }
    }

    /**
     * Return a ProtocolVersion with the specified major and minor version
     * numbers. Never throws exceptions.
     */
    public static ProtocolVersion valueOf(int major, int minor) {
        return valueOf(((major & 0xFF) << 8) | (minor & 0xFF));
    }

    /**
     * Return a ProtocolVersion for the given name.
     *
     * @exception IllegalArgumentException if name is null or does not
     * identify a supported protocol
     */
    static ProtocolVersion valueOf(String name) {
        if (name == null) {
            throw new IllegalArgumentException("Protocol cannot be null");
        }

        if (FIPS && (name.equals(SSL30.name) || name.equals(SSL20Hello.name))) {
            throw new IllegalArgumentException(
                    "Only TLS 1.0 or later allowed in FIPS mode");
        }

        if (name.equals(SSL30.name)) {
            return SSL30;
        } else if (name.equals(TLS10.name)) {
            return TLS10;
        } else if (name.equals(TLS11.name)) {
            return TLS11;
        } else if (name.equals(TLS12.name)) {
            return TLS12;
        } else if (name.equals(SSL20Hello.name)) {
            return SSL20Hello;
        } else if (name.equals(DTLS10.name)) {
            return DTLS10;
        } else if (name.equals(DTLS12.name)) {
            return DTLS12;
        } else {
            throw new IllegalArgumentException(name);
        }
    }

    @Override
    public String toString() {
        return name;
    }

    /**
     * Compares this object with the specified object for order.
     */
    @Override
    public int compareTo(ProtocolVersion protocolVersion) {
        if (maybeDTLSProtocol()) {
            if (!protocolVersion.maybeDTLSProtocol()) {
                throw new IllegalArgumentException("Not DTLS protocol");
            }

            return protocolVersion.v - this.v;
        } else {
            if (protocolVersion.maybeDTLSProtocol()) {
                throw new IllegalArgumentException("Not TLS protocol");
            }

            return this.v - protocolVersion.v;
        }
    }

    /**
     * Returns true if a ProtocolVersion represents a DTLS protocol.
     */
    boolean isDTLSProtocol() {
        return this.v == DTLS12.v || this.v == DTLS10.v;
    }

    /**
     * Returns true if a ProtocolVersion may represent a DTLS protocol.
     */
    boolean maybeDTLSProtocol() {
        return (this.major & 0x80) != 0;
    }

    boolean useTLS12PlusSpec() {
        return maybeDTLSProtocol() ? (this.v <= DTLS12.v) : (this.v >= TLS12.v);
    }

    boolean useTLS11PlusSpec() {
        return maybeDTLSProtocol() ? true : (this.v >= TLS11.v);
    }

    boolean useTLS10PlusSpec() {
        return maybeDTLSProtocol() ? true : (this.v >= TLS10.v);
    }

    boolean obsoletes(CipherSuite suite) {
        ProtocolVersion proto = this;
        if (proto.isDTLSProtocol()) {
            // DTLS bans stream ciphers.
            if (suite.cipher.cipherType == CipherType.STREAM_CIPHER) {
                return true;
            }

            proto = mapToTLSProtocol(this);
        }

        return (proto.v >= suite.obsoleted);
    }

    boolean supports(CipherSuite suite) {
        ProtocolVersion proto = this;
        if (proto.isDTLSProtocol()) {
            // DTLS bans stream ciphers.
            if (suite.cipher.cipherType == CipherType.STREAM_CIPHER) {
                return false;
            }

            proto = mapToTLSProtocol(this);
        }

        return (proto.v >= suite.supported);
    }

    // Map a specified protocol to the corresponding TLS version.
    //
    // DTLS 1.2 -> TLS 1.2
    // DTLS 1.0 -> TLS 1.1
    private static ProtocolVersion mapToTLSProtocol(
            ProtocolVersion protocolVersion) {

        if (protocolVersion.isDTLSProtocol()) {
            if (protocolVersion.v == DTLS10.v) {
                protocolVersion = TLS11;
            } else {    // DTLS12
                protocolVersion = TLS12;
            }
        }

        return protocolVersion;
    }
}
