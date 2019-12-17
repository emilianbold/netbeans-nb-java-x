/*
 * Copyright (c) 2006, 2019, Oracle and/or its affiliates. All rights reserved.
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

package sun.security.ec;

import java.io.IOException;
import java.math.BigInteger;

import java.security.*;
import java.security.interfaces.*;
import java.security.spec.*;

import sun.security.util.*;
import sun.security.x509.AlgorithmId;
import sun.security.pkcs.PKCS8Key;

/**
 * Key implementation for EC private keys.
 *
 * ASN.1 syntax for EC private keys from SEC 1 v1.5 (draft):
 *
 * <pre>
 * EXPLICIT TAGS
 *
 * ECPrivateKey ::= SEQUENCE {
 *   version INTEGER { ecPrivkeyVer1(1) } (ecPrivkeyVer1),
 *   privateKey OCTET STRING,
 *   parameters [0] ECDomainParameters {{ SECGCurveNames }} OPTIONAL,
 *   publicKey [1] BIT STRING OPTIONAL
 * }
 * </pre>
 *
 * We currently ignore the optional parameters. We require that the
 * parameters are encoded as part of the AlgorithmIdentifier,
 * not in the private key structure.
 *
 * @since   1.6
 * @author  Andreas Sterbenz
 */
public final class ECPrivateKeyImpl extends PKCS8Key implements ECPrivateKey {

    private static final long serialVersionUID = 88695385615075129L;

    private BigInteger s;       // private value
    private byte[] arrayS;      // private value as a little-endian array
    private ECParameterSpec params;
    private ECPoint pub;        // the optional public key

    /**
     * Construct a key from its encoding. Called by the ECKeyFactory.
     */
    ECPrivateKeyImpl(byte[] encoded) throws InvalidKeyException {
        decode(encoded);
    }

    /**
     * Construct a key from its components. Used by the
     * KeyFactory.
     */
    ECPrivateKeyImpl(BigInteger s, ECPoint pub, ECParameterSpec params)
            throws InvalidKeyException {
        this.s = s;
        this.params = params;
        this.pub = pub;
        makeEncoding(s);

    }

    ECPrivateKeyImpl(byte[] s, ECPoint pub, ECParameterSpec params)
            throws InvalidKeyException {
        this.arrayS = s.clone();
        this.params = params;
        this.pub = pub;
        makeEncoding(s);
    }

    private void makeEncoding(byte[] s) throws InvalidKeyException {
        algid = new AlgorithmId
        (AlgorithmId.EC_oid, ECParameters.getAlgorithmParameters(params));
        try {
            DerOutputStream out = new DerOutputStream();
            out.putInteger(1); // version 1
            byte[] privBytes = s.clone();
            ArrayUtil.reverse(privBytes);
            out.putOctetString(privBytes);
            if (pub != null) {
                DerOutputStream pubDer = new DerOutputStream();
                pubDer.putBitString(ECUtil.encodePoint(pub, params.getCurve()));
                out.write(DerValue.createTag(DerValue.TAG_CONTEXT,
                        true, (byte) 0x01), pubDer);
            }
            DerValue val =
                new DerValue(DerValue.tag_Sequence, out.toByteArray());
            key = val.toByteArray();
        } catch (IOException exc) {
            // should never occur
            throw new InvalidKeyException(exc);
        }
    }

    private void makeEncoding(BigInteger s) throws InvalidKeyException {
        algid = new AlgorithmId
        (AlgorithmId.EC_oid, ECParameters.getAlgorithmParameters(params));
        try {
            byte[] sArr = s.toByteArray();
            // convert to fixed-length array
            int numOctets = (params.getOrder().bitLength() + 7) / 8;
            byte[] sOctets = new byte[numOctets];
            int inPos = Math.max(sArr.length - sOctets.length, 0);
            int outPos = Math.max(sOctets.length - sArr.length, 0);
            int length = Math.min(sArr.length, sOctets.length);
            System.arraycopy(sArr, inPos, sOctets, outPos, length);

            DerOutputStream out = new DerOutputStream();
            out.putInteger(1); // version 1
            out.putOctetString(sOctets);
            DerValue val =
                new DerValue(DerValue.tag_Sequence, out.toByteArray());
            key = val.toByteArray();
        } catch (IOException exc) {
            // should never occur
            throw new InvalidKeyException(exc);
        }
    }

    // see JCA doc
    public String getAlgorithm() {
        return "EC";
    }

    // see JCA doc
    public BigInteger getS() {
        if (s == null) {
            byte[] arrCopy = arrayS.clone();
            ArrayUtil.reverse(arrCopy);
            s = new BigInteger(1, arrCopy);
        }
        return s;
    }

    public byte[] getArrayS() {
        if (arrayS == null) {
            byte[] arr = getS().toByteArray();
            ArrayUtil.reverse(arr);
            int byteLength = (params.getOrder().bitLength() + 7) / 8;
            arrayS = new byte[byteLength];
            int length = Math.min(byteLength, arr.length);
            System.arraycopy(arr, 0, arrayS, 0, length);
        }
        return arrayS.clone();
    }

    // see JCA doc
    public ECParameterSpec getParams() {
        return params;
    }

    /**
     * Parse the key. Called by PKCS8Key.
     */
    protected void parseKeyBits() throws InvalidKeyException {
        try {
            DerInputStream in = new DerInputStream(key);
            DerValue derValue = in.getDerValue();
            if (derValue.tag != DerValue.tag_Sequence) {
                throw new IOException("Not a SEQUENCE");
            }
            DerInputStream data = derValue.data;
            int version = data.getInteger();
            if (version != 1) {
                throw new IOException("Version must be 1");
            }
            byte[] privData = data.getOctetString();
            ArrayUtil.reverse(privData);
            arrayS = privData;
            AlgorithmParameters algParams = this.algid.getParameters();
            if (algParams == null) {
                throw new InvalidKeyException("EC domain parameters must be "
                        + "encoded in the algorithm identifier");
            }
            params = algParams.getParameterSpec(ECParameterSpec.class);
            while (data.available() != 0) {
                DerValue value = data.getDerValue();
                if (value.isContextSpecific((byte) 0)) {
                    // ignore for now. Usually not encoded because
                    // pkcs8 already has the params
                } else if (value.isContextSpecific((byte) 1)) {
                    pub = ECUtil.decodePoint(
                            value.data.getUnalignedBitString().toByteArray(),
                            params.getCurve());
                } else {
                    throw new InvalidKeyException("Unexpected value: " + value);
                }
            }
        } catch (IOException e) {
            throw new InvalidKeyException("Invalid EC private key", e);
        } catch (InvalidParameterSpecException e) {
            throw new InvalidKeyException("Invalid EC private key", e);
        }
    }
}
