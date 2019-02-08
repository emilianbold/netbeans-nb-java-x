/*
 * Copyright (c) 2014, 2018, Oracle and/or its affiliates. All rights reserved.
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
 * @summary Ensure the modifiers are correct
 * @library ../../lib
 * @modules jdk.javadoc/jdk.javadoc.internal.tool
 * @build javadoc.tester.*
 * @run main TestModifierEx
 */

import javadoc.tester.JavadocTester;

public class TestModifierEx extends JavadocTester {

    public static void main(String... args) throws Exception {
        TestModifierEx tester = new TestModifierEx();
        tester.runTests();
    }

    @Test
    public void test1(){
        javadoc("-d", "out-1",
                "-sourcepath", testSrc,
                "-package", "pkg1");
        checkExit(Exit.OK);
        checkOutput("pkg1/Abstract.html", true,
                "<pre>public abstract class <span class=\"typeNameLabel\">Abstract</span>");
        checkOutput("pkg1/Interface.html", true,
                "<pre>interface <span class=\"typeNameLabel\">Interface</span></pre>");
        checkOutput("pkg1/Interface.Kind.html", true,
                "<pre>public static interface <span class=\"typeNameLabel\">Interface.Kind</span></pre>");
        checkOutput("pkg1/Enum.html", true,
                "<pre>public enum <span class=\"typeNameLabel\">Enum</span>");
        checkOutput("pkg1/Klass.StaticEnum.html", true,
                "<pre>public static enum <span class=\"typeNameLabel\">Klass.StaticEnum</span>");
    }

}
