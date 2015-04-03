/*
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
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
 * @test
 * @bug 8026844
 * @summary Test incrementExact
 * @library /testlibrary
 * @modules java.base/sun.misc
 *          java.management
 * @compile IncExactITest.java Verify.java
 * @run main IncExactITest
 *
 */


public class IncExactITest {
    public static int[] values = {1, 1, 1, 1};
    public static void main(String[] args) {
        runTest(new Verify.IncExactI());
    }

    public static void runTest(Verify.UnaryMethod method) {
        for (int i = 0; i < 20000; ++i) {
            Verify.verifyUnary(Integer.MIN_VALUE, method);
            Verify.verifyUnary(Integer.MAX_VALUE - 1, method);
            Verify.verifyUnary(0, method);
            Verify.verifyUnary(values[1], method);
            Verify.verifyUnary(Integer.MAX_VALUE, method);
            Verify.verifyUnary(Integer.MAX_VALUE - values[0] + values[3], method);
            Verify.verifyUnary(Integer.MAX_VALUE - 1 + values[0], method);
        }
    }
}
