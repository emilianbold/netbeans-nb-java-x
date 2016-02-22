/*
 * Copyright (c) 2003, 2016, Oracle and/or its affiliates. All rights reserved.
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

package jdk.internal.math;

/**
 * This class contains additional constants documenting limits of the
 * {@code double} type.
 *
 * @author Joseph D. Darcy
 */

public class DoubleConsts {
    /**
     * Don't let anyone instantiate this class.
     */
    private DoubleConsts() {}

    /**
     * The number of logical bits in the significand of a
     * {@code double} number, including the implicit bit.
     */
    public static final int SIGNIFICAND_WIDTH   = 53;

    /**
     * The exponent the smallest positive {@code double}
     * subnormal value would have if it could be normalized..
     */
    public static final int     MIN_SUB_EXPONENT = Double.MIN_EXPONENT -
                                                   (SIGNIFICAND_WIDTH - 1);

    /**
     * Bias used in representing a {@code double} exponent.
     */
    public static final int     EXP_BIAS        = 1023;

    /**
     * Bit mask to isolate the sign bit of a {@code double}.
     */
    public static final long    SIGN_BIT_MASK   = 0x8000000000000000L;

    /**
     * Bit mask to isolate the exponent field of a
     * {@code double}.
     */
    public static final long    EXP_BIT_MASK    = 0x7FF0000000000000L;

    /**
     * Bit mask to isolate the significand field of a
     * {@code double}.
     */
    public static final long    SIGNIF_BIT_MASK = 0x000FFFFFFFFFFFFFL;

    static {
        // verify bit masks cover all bit positions and that the bit
        // masks are non-overlapping
        assert(((SIGN_BIT_MASK | EXP_BIT_MASK | SIGNIF_BIT_MASK) == ~0L) &&
               (((SIGN_BIT_MASK & EXP_BIT_MASK) == 0L) &&
                ((SIGN_BIT_MASK & SIGNIF_BIT_MASK) == 0L) &&
                ((EXP_BIT_MASK & SIGNIF_BIT_MASK) == 0L)));
    }
}
