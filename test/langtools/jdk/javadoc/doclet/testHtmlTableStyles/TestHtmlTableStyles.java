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
 * @bug 8008164 8169819 8183037 8182765 8196202 8184205 8242649
 * @summary Test styles on HTML tables generated by javadoc.
 * @library ../../lib
 * @modules jdk.javadoc/jdk.javadoc.internal.tool
 * @build javadoc.tester.*
 * @run main TestHtmlTableStyles
 */

import javadoc.tester.JavadocTester;

public class TestHtmlTableStyles extends JavadocTester {

    public static void main(String... args) throws Exception {
        TestHtmlTableStyles tester = new TestHtmlTableStyles();
        tester.runTests();
    }

    @Test
    public void test() {
        javadoc("-d", "out",
                "-sourcepath", testSrc,
                "-use",
                "pkg1", "pkg2");
        checkExit(Exit.ERROR);
        checkOutput(Output.OUT, true,
                "attribute not supported in HTML5: summary",
                "attribute border for table only accepts \"\" or \"1\", use CSS instead: BORDER",
                "attribute not supported in HTML5: cellpadding",
                "attribute not supported in HTML5: cellspacing",
                "attribute not supported in HTML5: align");

        checkOutput("pkg1/TestTable.html", true,
                "<table summary=\"Summary\" border cellpadding=3 cellspacing=1>",
                "<div class=\"member-summary\">\n<table class=\"summary-table\">",
                "<div class=\"member-summary\">\n<table class=\"summary-table\">",
                "<div class=\"member-summary\">\n<table class=\"summary-table\">");

        checkOutput("pkg1/package-summary.html", true,
                "<div class=\"type-summary\">\n<table class=\"summary-table\">");

        checkOutput("pkg1/class-use/TestTable.html", true,
                "<div class=\"use-summary\">\n<table class=\"summary-table\">");

        checkOutput("index.html", true,
                "<div class=\"overview-summary\" id=\"all-packages-table\">\n<table class=\"summary-table\">");

        checkOutput("deprecated-list.html", true,
            "<div class=\"deprecated-summary\" id=\"method\">\n<table class=\"summary-table\">");

        checkOutput("constant-values.html", true,
            "<div class=\"constants-summary\">\n<table class=\"summary-table\">");
    }
}
