/*
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved.
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

/**
 * @test
 * @bug 8073154
 * @build compiler.print.TestProfileReturnTypePrinting
 * @run main/othervm -XX:TypeProfileLevel=020
 *                   -XX:CompileCommand=compileonly,compiler.print.TestProfileReturnTypePrinting::testMethod
 *                   -XX:+IgnoreUnrecognizedVMOptions -XX:+PrintLIR
 *                   compiler.print.TestProfileReturnTypePrinting
 * @summary Verify that c1's LIR that contains ProfileType node could be dumped
 *          without a crash disregard to an exact class knowledge.
 */

package compiler.print;

public class TestProfileReturnTypePrinting {
    private static final int ITERATIONS = 1_000_000;

    public static void main(String args[]) {
        for (int i = 0; i < ITERATIONS; i++) {
            TestProfileReturnTypePrinting.testMethod(i);
        }
    }

    private static int testMethod(int i) {
        return TestProfileReturnTypePrinting.foo().hashCode()
                + TestProfileReturnTypePrinting.bar(i).hashCode();
    }

    /* Exact class of returned value is known statically. */
    private static B foo() {
        return new B();
    }

    /* Exact class of returned value is not known statically. */
    private static Object bar(int i) {
        if (i % 2 == 0) {
            return new A();
        } else {
            return new B();
        }
    }

    private static class A {
    }

    private static class B extends A {
    }
}
