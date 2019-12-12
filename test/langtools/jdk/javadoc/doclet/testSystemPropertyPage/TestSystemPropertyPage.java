/*
 * Copyright (c) 2019, Oracle and/or its affiliates. All rights reserved.
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
 * @bug 8215038
 * @summary Add a page that lists all system properties
 * @library /tools/lib ../../lib
 * @modules jdk.javadoc/jdk.javadoc.internal.tool
 * @build javadoc.tester.* toolbox.ToolBox builder.ClassBuilder
 * @run main TestSystemPropertyPage
 */

import java.nio.file.Path;
import java.nio.file.Paths;

import builder.ClassBuilder;
import javadoc.tester.JavadocTester;
import toolbox.ToolBox;

public class TestSystemPropertyPage extends JavadocTester {

    final ToolBox tb;

    public static void main(String... args) throws Exception {
        TestSystemPropertyPage tester = new TestSystemPropertyPage();
        tester.runTests(m -> new Object[]{Paths.get(m.getName())});
    }

    TestSystemPropertyPage() {
        tb = new ToolBox();
    }

    @Test
    public void test(Path base) throws Exception {
        Path srcDir = base.resolve("src");
        Path outDir = base.resolve("out");

        new ClassBuilder(tb, "pkg1.A")
                .setComments("test with {@systemProperty user.name}")
                .setModifiers("public", "class")
                .write(srcDir);

        new ClassBuilder(tb, "pkg2.B")
                .setComments("test with {@systemProperty user.address}, {@index user.home System Property}")
                .setModifiers("public", "class")
                .write(srcDir);

        javadoc("-d", outDir.toString(),
                "-sourcepath", srcDir.toString(),
                "pkg1","pkg2");

        checkExit(Exit.OK);

        checkOutput("index-all.html", true,
                "<a href=\"system-properties.html\">System Properties</a>");

        checkOutput("system-properties.html", true,
                "<table>\n" +
                "<caption><span>System Properties Summary</span><span " +
                "class=\"tabEnd\">&nbsp;</span></caption>\n" +
                "<thead>\n" +
                "<tr>\n" +
                "<th class=\"colFirst\" scope=\"col\">Property</th>\n" +
                "<th class=\"colLast\" scope=\"col\">Referenced In</th>\n" +
                "</tr>\n" +
                "</thead>\n" +
                "<tbody>\n" +
                "<tr class=\"altColor\">\n" +
                "<th class=\"colFirst\" scope=\"row\">user.address</th>\n" +
                "<td class=\"colLast\"><a href=\"pkg2/B.html#user.address\">class pkg2.B</a" +
                "></td>\n" +
                "</tr>\n" +
                "<tr class=\"rowColor\">\n" +
                "<th class=\"colFirst\" scope=\"row\">user.name</th>\n" +
                "<td class=\"colLast\"><a href=\"pkg1/A.html#user.name\">class pkg1.A</a></td" +
                ">\n" +
                "</tr>\n" +
                "</tbody>\n" +
                "</table>");
    }
}
