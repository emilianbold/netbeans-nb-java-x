/*
 * Copyright (c) 2017, 2018, Oracle and/or its affiliates. All rights reserved.
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
 * @bug 8186092 8199852
 * @compile ../common/Foo.java
 *          ../common/J.java
 *          I.java
 *          ../common/C.jasm
 *          Task.java
 *          ../common/PreemptingClassLoader.java
 * @run main/othervm Test
 */

public class Test {

    static String expectedErrorMessage1 =
        "loader constraint violation in interface itable initialization for class test.C: " +
        "when selecting method test.I.m()Ltest/Foo; " +
        "the class loader \"<unnamed>\" (instance of PreemptingClassLoader, " +
        "child of \"app\" jdk.internal.loader.ClassLoaders$AppClassLoader) " +
        "for super interface test.I, and the class loader \"app\" " +
        "(instance of jdk.internal.loader.ClassLoaders$AppClassLoader) " +
        "of the selected method's type, test.J have different Class objects " +
        "for the type test.Foo used in the signature";

    static String expectedErrorMessage2 =
        "loader constraint violation in interface itable initialization for class test.C: " +
        "when selecting method test.I.m()Ltest/Foo; " +
        "the class loader \"ItableLdrCnstrnt_Test_Loader\" (instance of PreemptingClassLoader, " +
        "child of \"app\" jdk.internal.loader.ClassLoaders$AppClassLoader) " +
        "for super interface test.I, and the class loader \"app\" " +
        "(instance of jdk.internal.loader.ClassLoaders$AppClassLoader) " +
        "of the selected method's type, test.J have different Class objects " +
        "for the type test.Foo used in the signature";

    // Test that the error message is correct when a loader constraint error is
    // detected during itable creation.
    //
    // In this test, during itable creation for class C, method "m()LFoo;" for
    // C's super interface I has a different class Foo than the selected method's
    // type super interface J.  The selected method is not an overpass method nor
    // otherwise excluded from loader constraint checking.  So, a LinkageError
    // exception should be thrown because the loader constraint check will fail.
    public static void test(String loaderName, String expectedErrorMessage) throws Exception {
        Class<?> c = test.Foo.class; // Forces standard class loader to load Foo.
        String[] classNames = {"test.Task", "test.Foo", "test.C", "test.I"};
        ClassLoader l = new PreemptingClassLoader(loaderName, classNames);
        Runnable r = (Runnable) l.loadClass("test.Task").newInstance();
        try {
            r.run();
            throw new RuntimeException("Expected LinkageError exception not thrown");
        } catch (LinkageError e) {
            String errorMsg = e.getMessage();
            if (!errorMsg.equals(expectedErrorMessage)) {
                System.out.println("Expected: " + expectedErrorMessage + "\n" +
                                   "but got:  " + errorMsg);
                throw new RuntimeException("Wrong LinkageError exception thrown: " + errorMsg);
            }
            System.out.println("Passed with message: " + errorMsg);
        }
    }

    public static void main(String... args) throws Exception {
        test(null, expectedErrorMessage1);
        test("ItableLdrCnstrnt_Test_Loader", expectedErrorMessage2);
    }
}
