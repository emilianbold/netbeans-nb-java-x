/*
 * Copyright (c) 2013, 2020, Oracle and/or its affiliates. All rights reserved.
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
 * @bug 8241625
 * @summary test the lists genereated by the doclet
 * @library  /tools/lib ../../lib
 * @modules  jdk.javadoc/jdk.javadoc.internal.tool
 * @build    toolbox.ToolBox javadoc.tester.*
 * @run main TestLists
 */

import java.io.IOException;
import java.nio.file.Path;

import javadoc.tester.JavadocTester;
import toolbox.ToolBox;

public class TestLists extends JavadocTester {

    public static void main(String... args) throws Exception {
        TestLists tester = new TestLists();
        tester.runTests(m -> new Object[]{Path.of(m.getName())});
    }

    private final ToolBox tb = new ToolBox();

    @Test
    public void testMemberLists(Path base) throws IOException {
        Path src = base.resolve("src");
        tb.writeJavaFiles(src,
                "package p; public class C {\n"
                + "  public C() { }\n"
                + "  public C(int i) { }\n"
                + "  public int f1;\n"
                + "  public int f2;\n"
                + "  public void m1() { }\n"
                + "  public void m2() { }\n"
                + "}\n",
                "package p; public enum E { E1, E2 }\n",
                "package p; public @interface A { int value(); }\n"
        );

        javadoc("-d", base.resolve("out").toString(),
                "-sourcepath", src.toString(),
                "p");
        checkExit(Exit.OK);

        checkOutput("p/C.html", true,
                "<h2>Field Details</h2>\n"
                + "<ul class=\"member-list\">\n"
                + "<li>\n"
                + "<section class=\"detail\" id=\"f1\">\n"
                + "<h3>f1</h3>\n",
                "<h2>Constructor Details</h2>\n"
                + "<ul class=\"member-list\">\n"
                + "<li>\n"
                + "<section class=\"detail\" id=\"&lt;init&gt;()\">\n"
                + "<h3>C</h3>",
                "<section class=\"method-details\" id=\"method.detail\">\n"
                + "<h2>Method Details</h2>\n"
                + "<ul class=\"member-list\">\n"
                + "<li>\n"
                + "<section class=\"detail\" id=\"m1()\">\n"
                + "<h3>m1</h3>\n");

        checkOutput("p/E.html", true,
                "<h2>Enum Constant Details</h2>\n"
                + "<ul class=\"member-list\">\n"
                + "<li>\n"
                + "<section class=\"detail\" id=\"E1\">\n");

        checkOutput("p/A.html", true,
                "<h2>Element Details</h2>\n"
                + "<ul class=\"member-list\">\n"
                + "<li>\n"
                + "<section class=\"detail\" id=\"value()\">");
    }
}
