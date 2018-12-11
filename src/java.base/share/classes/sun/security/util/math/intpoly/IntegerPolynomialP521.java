/*
 * Copyright (c) 2018, Oracle and/or its affiliates. All rights reserved.
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

/*
 * This file is generated by FieldGen.jsh. Do not modify it directly.
 */

package sun.security.util.math.intpoly;

import java.math.BigInteger;
public class IntegerPolynomialP521 extends IntegerPolynomial {
    private static final int BITS_PER_LIMB = 28;
    private static final int NUM_LIMBS = 19;
    private static final int MAX_ADDS = 2;
    public static final BigInteger MODULUS = evaluateModulus();
    private static final long CARRY_ADD = 1 << 27;
    private static final int LIMB_MASK = -1 >>> (64 - BITS_PER_LIMB);
    public IntegerPolynomialP521() {

        super(BITS_PER_LIMB, NUM_LIMBS, MAX_ADDS, MODULUS);

    }
    private static BigInteger evaluateModulus() {
        BigInteger result = BigInteger.valueOf(2).pow(521);
        result = result.subtract(BigInteger.valueOf(1));
        return result;
    }
    @Override
    protected void finalCarryReduceLast(long[] limbs) {
        long c = limbs[18] >> 17;
        limbs[18] -= c << 17;
        limbs[0] += c;
    }
    private void carryReduce(long[] r, long c0, long c1, long c2, long c3, long c4, long c5, long c6, long c7, long c8, long c9, long c10, long c11, long c12, long c13, long c14, long c15, long c16, long c17, long c18, long c19, long c20, long c21, long c22, long c23, long c24, long c25, long c26, long c27, long c28, long c29, long c30, long c31, long c32, long c33, long c34, long c35, long c36) {
        long c37 = 0;
        //reduce from position 36
        c17 += (c36 << 11) & LIMB_MASK;
        c18 += c36 >> 17;
        //reduce from position 35
        c16 += (c35 << 11) & LIMB_MASK;
        c17 += c35 >> 17;
        //reduce from position 34
        c15 += (c34 << 11) & LIMB_MASK;
        c16 += c34 >> 17;
        //reduce from position 33
        c14 += (c33 << 11) & LIMB_MASK;
        c15 += c33 >> 17;
        //reduce from position 32
        c13 += (c32 << 11) & LIMB_MASK;
        c14 += c32 >> 17;
        //reduce from position 31
        c12 += (c31 << 11) & LIMB_MASK;
        c13 += c31 >> 17;
        //reduce from position 30
        c11 += (c30 << 11) & LIMB_MASK;
        c12 += c30 >> 17;
        //reduce from position 29
        c10 += (c29 << 11) & LIMB_MASK;
        c11 += c29 >> 17;
        //reduce from position 28
        c9 += (c28 << 11) & LIMB_MASK;
        c10 += c28 >> 17;
        //reduce from position 27
        c8 += (c27 << 11) & LIMB_MASK;
        c9 += c27 >> 17;
        //reduce from position 26
        c7 += (c26 << 11) & LIMB_MASK;
        c8 += c26 >> 17;
        //reduce from position 25
        c6 += (c25 << 11) & LIMB_MASK;
        c7 += c25 >> 17;
        //reduce from position 24
        c5 += (c24 << 11) & LIMB_MASK;
        c6 += c24 >> 17;
        //reduce from position 23
        c4 += (c23 << 11) & LIMB_MASK;
        c5 += c23 >> 17;
        //reduce from position 22
        c3 += (c22 << 11) & LIMB_MASK;
        c4 += c22 >> 17;
        //reduce from position 21
        c2 += (c21 << 11) & LIMB_MASK;
        c3 += c21 >> 17;
        //reduce from position 20
        c1 += (c20 << 11) & LIMB_MASK;
        c2 += c20 >> 17;
        //reduce from position 19
        c0 += (c19 << 11) & LIMB_MASK;
        c1 += c19 >> 17;
        c19 = 0;

        carryReduce0(r, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30, c31, c32, c33, c34, c35, c36, c37);
    }
    void carryReduce0(long[] r, long c0, long c1, long c2, long c3, long c4, long c5, long c6, long c7, long c8, long c9, long c10, long c11, long c12, long c13, long c14, long c15, long c16, long c17, long c18, long c19, long c20, long c21, long c22, long c23, long c24, long c25, long c26, long c27, long c28, long c29, long c30, long c31, long c32, long c33, long c34, long c35, long c36, long c37) {

        //carry from position 17
        long t0 = (c17 + CARRY_ADD) >> 28;
        c17 -= (t0 << 28);
        c18 += t0;
        //carry from position 18
        t0 = (c18 + CARRY_ADD) >> 28;
        c18 -= (t0 << 28);
        c19 += t0;
        //reduce from position 19
        c0 += (c19 << 11) & LIMB_MASK;
        c1 += c19 >> 17;
        //carry from position 0
        t0 = (c0 + CARRY_ADD) >> 28;
        c0 -= (t0 << 28);
        c1 += t0;
        //carry from position 1
        t0 = (c1 + CARRY_ADD) >> 28;
        c1 -= (t0 << 28);
        c2 += t0;
        //carry from position 2
        t0 = (c2 + CARRY_ADD) >> 28;
        c2 -= (t0 << 28);
        c3 += t0;
        //carry from position 3
        t0 = (c3 + CARRY_ADD) >> 28;
        c3 -= (t0 << 28);
        c4 += t0;
        //carry from position 4
        t0 = (c4 + CARRY_ADD) >> 28;
        c4 -= (t0 << 28);
        c5 += t0;
        //carry from position 5
        t0 = (c5 + CARRY_ADD) >> 28;
        c5 -= (t0 << 28);
        c6 += t0;
        //carry from position 6
        t0 = (c6 + CARRY_ADD) >> 28;
        c6 -= (t0 << 28);
        c7 += t0;
        //carry from position 7
        t0 = (c7 + CARRY_ADD) >> 28;
        c7 -= (t0 << 28);
        c8 += t0;
        //carry from position 8
        t0 = (c8 + CARRY_ADD) >> 28;
        c8 -= (t0 << 28);
        c9 += t0;
        //carry from position 9
        t0 = (c9 + CARRY_ADD) >> 28;
        c9 -= (t0 << 28);
        c10 += t0;
        //carry from position 10
        t0 = (c10 + CARRY_ADD) >> 28;
        c10 -= (t0 << 28);
        c11 += t0;
        //carry from position 11
        t0 = (c11 + CARRY_ADD) >> 28;
        c11 -= (t0 << 28);
        c12 += t0;
        //carry from position 12
        t0 = (c12 + CARRY_ADD) >> 28;
        c12 -= (t0 << 28);
        c13 += t0;
        //carry from position 13
        t0 = (c13 + CARRY_ADD) >> 28;
        c13 -= (t0 << 28);
        c14 += t0;
        //carry from position 14
        t0 = (c14 + CARRY_ADD) >> 28;
        c14 -= (t0 << 28);
        c15 += t0;
        //carry from position 15
        t0 = (c15 + CARRY_ADD) >> 28;
        c15 -= (t0 << 28);
        c16 += t0;
        //carry from position 16
        t0 = (c16 + CARRY_ADD) >> 28;
        c16 -= (t0 << 28);
        c17 += t0;
        //carry from position 17
        t0 = (c17 + CARRY_ADD) >> 28;
        c17 -= (t0 << 28);
        c18 += t0;

        r[0] = c0;
        r[1] = c1;
        r[2] = c2;
        r[3] = c3;
        r[4] = c4;
        r[5] = c5;
        r[6] = c6;
        r[7] = c7;
        r[8] = c8;
        r[9] = c9;
        r[10] = c10;
        r[11] = c11;
        r[12] = c12;
        r[13] = c13;
        r[14] = c14;
        r[15] = c15;
        r[16] = c16;
        r[17] = c17;
        r[18] = c18;
    }
    private void carryReduce(long[] r, long c0, long c1, long c2, long c3, long c4, long c5, long c6, long c7, long c8, long c9, long c10, long c11, long c12, long c13, long c14, long c15, long c16, long c17, long c18) {
        long c19 = 0;
        //carry from position 17
        long t0 = (c17 + CARRY_ADD) >> 28;
        c17 -= (t0 << 28);
        c18 += t0;
        //carry from position 18
        t0 = (c18 + CARRY_ADD) >> 28;
        c18 -= (t0 << 28);
        c19 += t0;
        //reduce from position 19
        c0 += (c19 << 11) & LIMB_MASK;
        c1 += c19 >> 17;
        //carry from position 0
        t0 = (c0 + CARRY_ADD) >> 28;
        c0 -= (t0 << 28);
        c1 += t0;
        //carry from position 1
        t0 = (c1 + CARRY_ADD) >> 28;
        c1 -= (t0 << 28);
        c2 += t0;
        //carry from position 2
        t0 = (c2 + CARRY_ADD) >> 28;
        c2 -= (t0 << 28);
        c3 += t0;
        //carry from position 3
        t0 = (c3 + CARRY_ADD) >> 28;
        c3 -= (t0 << 28);
        c4 += t0;
        //carry from position 4
        t0 = (c4 + CARRY_ADD) >> 28;
        c4 -= (t0 << 28);
        c5 += t0;
        //carry from position 5
        t0 = (c5 + CARRY_ADD) >> 28;
        c5 -= (t0 << 28);
        c6 += t0;
        //carry from position 6
        t0 = (c6 + CARRY_ADD) >> 28;
        c6 -= (t0 << 28);
        c7 += t0;
        //carry from position 7
        t0 = (c7 + CARRY_ADD) >> 28;
        c7 -= (t0 << 28);
        c8 += t0;
        //carry from position 8
        t0 = (c8 + CARRY_ADD) >> 28;
        c8 -= (t0 << 28);
        c9 += t0;
        //carry from position 9
        t0 = (c9 + CARRY_ADD) >> 28;
        c9 -= (t0 << 28);
        c10 += t0;
        //carry from position 10
        t0 = (c10 + CARRY_ADD) >> 28;
        c10 -= (t0 << 28);
        c11 += t0;
        //carry from position 11
        t0 = (c11 + CARRY_ADD) >> 28;
        c11 -= (t0 << 28);
        c12 += t0;
        //carry from position 12
        t0 = (c12 + CARRY_ADD) >> 28;
        c12 -= (t0 << 28);
        c13 += t0;
        //carry from position 13
        t0 = (c13 + CARRY_ADD) >> 28;
        c13 -= (t0 << 28);
        c14 += t0;
        //carry from position 14
        t0 = (c14 + CARRY_ADD) >> 28;
        c14 -= (t0 << 28);
        c15 += t0;
        //carry from position 15
        t0 = (c15 + CARRY_ADD) >> 28;
        c15 -= (t0 << 28);
        c16 += t0;
        //carry from position 16
        t0 = (c16 + CARRY_ADD) >> 28;
        c16 -= (t0 << 28);
        c17 += t0;
        //carry from position 17
        t0 = (c17 + CARRY_ADD) >> 28;
        c17 -= (t0 << 28);
        c18 += t0;

        r[0] = c0;
        r[1] = c1;
        r[2] = c2;
        r[3] = c3;
        r[4] = c4;
        r[5] = c5;
        r[6] = c6;
        r[7] = c7;
        r[8] = c8;
        r[9] = c9;
        r[10] = c10;
        r[11] = c11;
        r[12] = c12;
        r[13] = c13;
        r[14] = c14;
        r[15] = c15;
        r[16] = c16;
        r[17] = c17;
        r[18] = c18;
    }
    @Override
    protected void mult(long[] a, long[] b, long[] r) {
        long c0 = (a[0] * b[0]);
        long c1 = (a[0] * b[1]) + (a[1] * b[0]);
        long c2 = (a[0] * b[2]) + (a[1] * b[1]) + (a[2] * b[0]);
        long c3 = (a[0] * b[3]) + (a[1] * b[2]) + (a[2] * b[1]) + (a[3] * b[0]);
        long c4 = (a[0] * b[4]) + (a[1] * b[3]) + (a[2] * b[2]) + (a[3] * b[1]) + (a[4] * b[0]);
        long c5 = (a[0] * b[5]) + (a[1] * b[4]) + (a[2] * b[3]) + (a[3] * b[2]) + (a[4] * b[1]) + (a[5] * b[0]);
        long c6 = (a[0] * b[6]) + (a[1] * b[5]) + (a[2] * b[4]) + (a[3] * b[3]) + (a[4] * b[2]) + (a[5] * b[1]) + (a[6] * b[0]);
        long c7 = (a[0] * b[7]) + (a[1] * b[6]) + (a[2] * b[5]) + (a[3] * b[4]) + (a[4] * b[3]) + (a[5] * b[2]) + (a[6] * b[1]) + (a[7] * b[0]);
        long c8 = (a[0] * b[8]) + (a[1] * b[7]) + (a[2] * b[6]) + (a[3] * b[5]) + (a[4] * b[4]) + (a[5] * b[3]) + (a[6] * b[2]) + (a[7] * b[1]) + (a[8] * b[0]);
        long c9 = (a[0] * b[9]) + (a[1] * b[8]) + (a[2] * b[7]) + (a[3] * b[6]) + (a[4] * b[5]) + (a[5] * b[4]) + (a[6] * b[3]) + (a[7] * b[2]) + (a[8] * b[1]) + (a[9] * b[0]);
        long c10 = (a[0] * b[10]) + (a[1] * b[9]) + (a[2] * b[8]) + (a[3] * b[7]) + (a[4] * b[6]) + (a[5] * b[5]) + (a[6] * b[4]) + (a[7] * b[3]) + (a[8] * b[2]) + (a[9] * b[1]) + (a[10] * b[0]);
        long c11 = (a[0] * b[11]) + (a[1] * b[10]) + (a[2] * b[9]) + (a[3] * b[8]) + (a[4] * b[7]) + (a[5] * b[6]) + (a[6] * b[5]) + (a[7] * b[4]) + (a[8] * b[3]) + (a[9] * b[2]) + (a[10] * b[1]) + (a[11] * b[0]);
        long c12 = (a[0] * b[12]) + (a[1] * b[11]) + (a[2] * b[10]) + (a[3] * b[9]) + (a[4] * b[8]) + (a[5] * b[7]) + (a[6] * b[6]) + (a[7] * b[5]) + (a[8] * b[4]) + (a[9] * b[3]) + (a[10] * b[2]) + (a[11] * b[1]) + (a[12] * b[0]);
        long c13 = (a[0] * b[13]) + (a[1] * b[12]) + (a[2] * b[11]) + (a[3] * b[10]) + (a[4] * b[9]) + (a[5] * b[8]) + (a[6] * b[7]) + (a[7] * b[6]) + (a[8] * b[5]) + (a[9] * b[4]) + (a[10] * b[3]) + (a[11] * b[2]) + (a[12] * b[1]) + (a[13] * b[0]);
        long c14 = (a[0] * b[14]) + (a[1] * b[13]) + (a[2] * b[12]) + (a[3] * b[11]) + (a[4] * b[10]) + (a[5] * b[9]) + (a[6] * b[8]) + (a[7] * b[7]) + (a[8] * b[6]) + (a[9] * b[5]) + (a[10] * b[4]) + (a[11] * b[3]) + (a[12] * b[2]) + (a[13] * b[1]) + (a[14] * b[0]);
        long c15 = (a[0] * b[15]) + (a[1] * b[14]) + (a[2] * b[13]) + (a[3] * b[12]) + (a[4] * b[11]) + (a[5] * b[10]) + (a[6] * b[9]) + (a[7] * b[8]) + (a[8] * b[7]) + (a[9] * b[6]) + (a[10] * b[5]) + (a[11] * b[4]) + (a[12] * b[3]) + (a[13] * b[2]) + (a[14] * b[1]) + (a[15] * b[0]);
        long c16 = (a[0] * b[16]) + (a[1] * b[15]) + (a[2] * b[14]) + (a[3] * b[13]) + (a[4] * b[12]) + (a[5] * b[11]) + (a[6] * b[10]) + (a[7] * b[9]) + (a[8] * b[8]) + (a[9] * b[7]) + (a[10] * b[6]) + (a[11] * b[5]) + (a[12] * b[4]) + (a[13] * b[3]) + (a[14] * b[2]) + (a[15] * b[1]) + (a[16] * b[0]);
        long c17 = (a[0] * b[17]) + (a[1] * b[16]) + (a[2] * b[15]) + (a[3] * b[14]) + (a[4] * b[13]) + (a[5] * b[12]) + (a[6] * b[11]) + (a[7] * b[10]) + (a[8] * b[9]) + (a[9] * b[8]) + (a[10] * b[7]) + (a[11] * b[6]) + (a[12] * b[5]) + (a[13] * b[4]) + (a[14] * b[3]) + (a[15] * b[2]) + (a[16] * b[1]) + (a[17] * b[0]);
        long c18 = (a[0] * b[18]) + (a[1] * b[17]) + (a[2] * b[16]) + (a[3] * b[15]) + (a[4] * b[14]) + (a[5] * b[13]) + (a[6] * b[12]) + (a[7] * b[11]) + (a[8] * b[10]) + (a[9] * b[9]) + (a[10] * b[8]) + (a[11] * b[7]) + (a[12] * b[6]) + (a[13] * b[5]) + (a[14] * b[4]) + (a[15] * b[3]) + (a[16] * b[2]) + (a[17] * b[1]) + (a[18] * b[0]);
        long c19 = (a[1] * b[18]) + (a[2] * b[17]) + (a[3] * b[16]) + (a[4] * b[15]) + (a[5] * b[14]) + (a[6] * b[13]) + (a[7] * b[12]) + (a[8] * b[11]) + (a[9] * b[10]) + (a[10] * b[9]) + (a[11] * b[8]) + (a[12] * b[7]) + (a[13] * b[6]) + (a[14] * b[5]) + (a[15] * b[4]) + (a[16] * b[3]) + (a[17] * b[2]) + (a[18] * b[1]);
        long c20 = (a[2] * b[18]) + (a[3] * b[17]) + (a[4] * b[16]) + (a[5] * b[15]) + (a[6] * b[14]) + (a[7] * b[13]) + (a[8] * b[12]) + (a[9] * b[11]) + (a[10] * b[10]) + (a[11] * b[9]) + (a[12] * b[8]) + (a[13] * b[7]) + (a[14] * b[6]) + (a[15] * b[5]) + (a[16] * b[4]) + (a[17] * b[3]) + (a[18] * b[2]);
        long c21 = (a[3] * b[18]) + (a[4] * b[17]) + (a[5] * b[16]) + (a[6] * b[15]) + (a[7] * b[14]) + (a[8] * b[13]) + (a[9] * b[12]) + (a[10] * b[11]) + (a[11] * b[10]) + (a[12] * b[9]) + (a[13] * b[8]) + (a[14] * b[7]) + (a[15] * b[6]) + (a[16] * b[5]) + (a[17] * b[4]) + (a[18] * b[3]);
        long c22 = (a[4] * b[18]) + (a[5] * b[17]) + (a[6] * b[16]) + (a[7] * b[15]) + (a[8] * b[14]) + (a[9] * b[13]) + (a[10] * b[12]) + (a[11] * b[11]) + (a[12] * b[10]) + (a[13] * b[9]) + (a[14] * b[8]) + (a[15] * b[7]) + (a[16] * b[6]) + (a[17] * b[5]) + (a[18] * b[4]);
        long c23 = (a[5] * b[18]) + (a[6] * b[17]) + (a[7] * b[16]) + (a[8] * b[15]) + (a[9] * b[14]) + (a[10] * b[13]) + (a[11] * b[12]) + (a[12] * b[11]) + (a[13] * b[10]) + (a[14] * b[9]) + (a[15] * b[8]) + (a[16] * b[7]) + (a[17] * b[6]) + (a[18] * b[5]);
        long c24 = (a[6] * b[18]) + (a[7] * b[17]) + (a[8] * b[16]) + (a[9] * b[15]) + (a[10] * b[14]) + (a[11] * b[13]) + (a[12] * b[12]) + (a[13] * b[11]) + (a[14] * b[10]) + (a[15] * b[9]) + (a[16] * b[8]) + (a[17] * b[7]) + (a[18] * b[6]);
        long c25 = (a[7] * b[18]) + (a[8] * b[17]) + (a[9] * b[16]) + (a[10] * b[15]) + (a[11] * b[14]) + (a[12] * b[13]) + (a[13] * b[12]) + (a[14] * b[11]) + (a[15] * b[10]) + (a[16] * b[9]) + (a[17] * b[8]) + (a[18] * b[7]);
        long c26 = (a[8] * b[18]) + (a[9] * b[17]) + (a[10] * b[16]) + (a[11] * b[15]) + (a[12] * b[14]) + (a[13] * b[13]) + (a[14] * b[12]) + (a[15] * b[11]) + (a[16] * b[10]) + (a[17] * b[9]) + (a[18] * b[8]);
        long c27 = (a[9] * b[18]) + (a[10] * b[17]) + (a[11] * b[16]) + (a[12] * b[15]) + (a[13] * b[14]) + (a[14] * b[13]) + (a[15] * b[12]) + (a[16] * b[11]) + (a[17] * b[10]) + (a[18] * b[9]);
        long c28 = (a[10] * b[18]) + (a[11] * b[17]) + (a[12] * b[16]) + (a[13] * b[15]) + (a[14] * b[14]) + (a[15] * b[13]) + (a[16] * b[12]) + (a[17] * b[11]) + (a[18] * b[10]);
        long c29 = (a[11] * b[18]) + (a[12] * b[17]) + (a[13] * b[16]) + (a[14] * b[15]) + (a[15] * b[14]) + (a[16] * b[13]) + (a[17] * b[12]) + (a[18] * b[11]);
        long c30 = (a[12] * b[18]) + (a[13] * b[17]) + (a[14] * b[16]) + (a[15] * b[15]) + (a[16] * b[14]) + (a[17] * b[13]) + (a[18] * b[12]);
        long c31 = (a[13] * b[18]) + (a[14] * b[17]) + (a[15] * b[16]) + (a[16] * b[15]) + (a[17] * b[14]) + (a[18] * b[13]);
        long c32 = (a[14] * b[18]) + (a[15] * b[17]) + (a[16] * b[16]) + (a[17] * b[15]) + (a[18] * b[14]);
        long c33 = (a[15] * b[18]) + (a[16] * b[17]) + (a[17] * b[16]) + (a[18] * b[15]);
        long c34 = (a[16] * b[18]) + (a[17] * b[17]) + (a[18] * b[16]);
        long c35 = (a[17] * b[18]) + (a[18] * b[17]);
        long c36 = (a[18] * b[18]);

        carryReduce(r, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30, c31, c32, c33, c34, c35, c36);
    }
    @Override
    protected void reduce(long[] a) {
        carryReduce(a, a[0], a[1], a[2], a[3], a[4], a[5], a[6], a[7], a[8], a[9], a[10], a[11], a[12], a[13], a[14], a[15], a[16], a[17], a[18]);
    }
    @Override
    protected void square(long[] a, long[] r) {
        long c0 = (a[0] * a[0]);
        long c1 = 2 * ((a[0] * a[1]));
        long c2 = 2 * ((a[0] * a[2])) + (a[1] * a[1]);
        long c3 = 2 * ((a[0] * a[3]) + (a[1] * a[2]));
        long c4 = 2 * ((a[0] * a[4]) + (a[1] * a[3])) + (a[2] * a[2]);
        long c5 = 2 * ((a[0] * a[5]) + (a[1] * a[4]) + (a[2] * a[3]));
        long c6 = 2 * ((a[0] * a[6]) + (a[1] * a[5]) + (a[2] * a[4])) + (a[3] * a[3]);
        long c7 = 2 * ((a[0] * a[7]) + (a[1] * a[6]) + (a[2] * a[5]) + (a[3] * a[4]));
        long c8 = 2 * ((a[0] * a[8]) + (a[1] * a[7]) + (a[2] * a[6]) + (a[3] * a[5])) + (a[4] * a[4]);
        long c9 = 2 * ((a[0] * a[9]) + (a[1] * a[8]) + (a[2] * a[7]) + (a[3] * a[6]) + (a[4] * a[5]));
        long c10 = 2 * ((a[0] * a[10]) + (a[1] * a[9]) + (a[2] * a[8]) + (a[3] * a[7]) + (a[4] * a[6])) + (a[5] * a[5]);
        long c11 = 2 * ((a[0] * a[11]) + (a[1] * a[10]) + (a[2] * a[9]) + (a[3] * a[8]) + (a[4] * a[7]) + (a[5] * a[6]));
        long c12 = 2 * ((a[0] * a[12]) + (a[1] * a[11]) + (a[2] * a[10]) + (a[3] * a[9]) + (a[4] * a[8]) + (a[5] * a[7])) + (a[6] * a[6]);
        long c13 = 2 * ((a[0] * a[13]) + (a[1] * a[12]) + (a[2] * a[11]) + (a[3] * a[10]) + (a[4] * a[9]) + (a[5] * a[8]) + (a[6] * a[7]));
        long c14 = 2 * ((a[0] * a[14]) + (a[1] * a[13]) + (a[2] * a[12]) + (a[3] * a[11]) + (a[4] * a[10]) + (a[5] * a[9]) + (a[6] * a[8])) + (a[7] * a[7]);
        long c15 = 2 * ((a[0] * a[15]) + (a[1] * a[14]) + (a[2] * a[13]) + (a[3] * a[12]) + (a[4] * a[11]) + (a[5] * a[10]) + (a[6] * a[9]) + (a[7] * a[8]));
        long c16 = 2 * ((a[0] * a[16]) + (a[1] * a[15]) + (a[2] * a[14]) + (a[3] * a[13]) + (a[4] * a[12]) + (a[5] * a[11]) + (a[6] * a[10]) + (a[7] * a[9])) + (a[8] * a[8]);
        long c17 = 2 * ((a[0] * a[17]) + (a[1] * a[16]) + (a[2] * a[15]) + (a[3] * a[14]) + (a[4] * a[13]) + (a[5] * a[12]) + (a[6] * a[11]) + (a[7] * a[10]) + (a[8] * a[9]));
        long c18 = 2 * ((a[0] * a[18]) + (a[1] * a[17]) + (a[2] * a[16]) + (a[3] * a[15]) + (a[4] * a[14]) + (a[5] * a[13]) + (a[6] * a[12]) + (a[7] * a[11]) + (a[8] * a[10])) + (a[9] * a[9]);
        long c19 = 2 * ((a[1] * a[18]) + (a[2] * a[17]) + (a[3] * a[16]) + (a[4] * a[15]) + (a[5] * a[14]) + (a[6] * a[13]) + (a[7] * a[12]) + (a[8] * a[11]) + (a[9] * a[10]));
        long c20 = 2 * ((a[2] * a[18]) + (a[3] * a[17]) + (a[4] * a[16]) + (a[5] * a[15]) + (a[6] * a[14]) + (a[7] * a[13]) + (a[8] * a[12]) + (a[9] * a[11])) + (a[10] * a[10]);
        long c21 = 2 * ((a[3] * a[18]) + (a[4] * a[17]) + (a[5] * a[16]) + (a[6] * a[15]) + (a[7] * a[14]) + (a[8] * a[13]) + (a[9] * a[12]) + (a[10] * a[11]));
        long c22 = 2 * ((a[4] * a[18]) + (a[5] * a[17]) + (a[6] * a[16]) + (a[7] * a[15]) + (a[8] * a[14]) + (a[9] * a[13]) + (a[10] * a[12])) + (a[11] * a[11]);
        long c23 = 2 * ((a[5] * a[18]) + (a[6] * a[17]) + (a[7] * a[16]) + (a[8] * a[15]) + (a[9] * a[14]) + (a[10] * a[13]) + (a[11] * a[12]));
        long c24 = 2 * ((a[6] * a[18]) + (a[7] * a[17]) + (a[8] * a[16]) + (a[9] * a[15]) + (a[10] * a[14]) + (a[11] * a[13])) + (a[12] * a[12]);
        long c25 = 2 * ((a[7] * a[18]) + (a[8] * a[17]) + (a[9] * a[16]) + (a[10] * a[15]) + (a[11] * a[14]) + (a[12] * a[13]));
        long c26 = 2 * ((a[8] * a[18]) + (a[9] * a[17]) + (a[10] * a[16]) + (a[11] * a[15]) + (a[12] * a[14])) + (a[13] * a[13]);
        long c27 = 2 * ((a[9] * a[18]) + (a[10] * a[17]) + (a[11] * a[16]) + (a[12] * a[15]) + (a[13] * a[14]));
        long c28 = 2 * ((a[10] * a[18]) + (a[11] * a[17]) + (a[12] * a[16]) + (a[13] * a[15])) + (a[14] * a[14]);
        long c29 = 2 * ((a[11] * a[18]) + (a[12] * a[17]) + (a[13] * a[16]) + (a[14] * a[15]));
        long c30 = 2 * ((a[12] * a[18]) + (a[13] * a[17]) + (a[14] * a[16])) + (a[15] * a[15]);
        long c31 = 2 * ((a[13] * a[18]) + (a[14] * a[17]) + (a[15] * a[16]));
        long c32 = 2 * ((a[14] * a[18]) + (a[15] * a[17])) + (a[16] * a[16]);
        long c33 = 2 * ((a[15] * a[18]) + (a[16] * a[17]));
        long c34 = 2 * ((a[16] * a[18])) + (a[17] * a[17]);
        long c35 = 2 * ((a[17] * a[18]));
        long c36 = (a[18] * a[18]);

        carryReduce(r, c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30, c31, c32, c33, c34, c35, c36);
    }
}

