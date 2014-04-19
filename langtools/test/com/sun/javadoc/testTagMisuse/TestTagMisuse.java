/*
 * Copyright (c) 2001, 2014, Oracle and/or its affiliates. All rights reserved.
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
 * @summary Determine if proper warning messages are printed when know.
 * @author jamieh
 * @library ../lib/
 * @build JavadocTester
 * @build TestTagMisuse
 * @run main TestTagMisuse
 */
public class TestTagMisuse extends JavadocTester {

    private static final String BUG_ID = "no-bug-id";
    private static final String[][] TEST = {
        {WARNING_OUTPUT, "warning - Tag @param cannot be used in field documentation."},
        {WARNING_OUTPUT, "warning - Tag @throws cannot be used in field documentation."},
        {WARNING_OUTPUT, "warning - Tag @return cannot be used in constructor documentation."},
        {WARNING_OUTPUT, "warning - Tag @throws cannot be used in inline documentation."},
    };
    private static final String[][] NEGATED_TEST = NO_TEST;
    private static final String[] ARGS = new String[] {
        "-Xdoclint:none", "-d", BUG_ID, SRC_DIR + "/TestTagMisuse.java"
    };

    /**
     * The entry point of the test.
     * @param args the array of command line arguments.
     */
    public static void main(String[] args) {
        TestTagMisuse tester = new TestTagMisuse();
        tester.run(ARGS, TEST, NEGATED_TEST);
        tester.printSummary();
    }

    /**
     * {@inheritDoc}
     */
    public String getBugId() {
        return BUG_ID;
    }

    /**
     * {@inheritDoc}
     */
    public String getBugName() {
        return getClass().getName();
    }

    /**
     * {@throws blah}
     * Here is a bad field tag:
     * @throws foo
     * @param foo.
     */
    public int field;

    /**
     * Here is a bad constructor tag:
     * @return blah
     */
    public TestTagMisuse(){}

}
