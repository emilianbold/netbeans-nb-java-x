/*
 * Copyright (c) 2013, 2016, Oracle and/or its affiliates. All rights reserved.
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
 * @bug 8009640
 * @summary -profile <compact> does not work when -bootclasspath specified
 * @library /tools/lib
 * @modules jdk.compiler/com.sun.tools.javac.api
 *          jdk.compiler/com.sun.tools.javac.main
 *          jdk.compiler/com.sun.tools.javac.util
 *          jdk.jdeps/com.sun.tools.javap
 * @build toolbox.ToolBox toolbox.JavacTask
 * @run main CheckRejectProfileBCPOptionsIfUsedTogetherTest
 */

import java.nio.file.Paths;

import com.sun.tools.javac.util.Assert;

import toolbox.JavacTask;
import toolbox.Task;
import toolbox.ToolBox;

public class CheckRejectProfileBCPOptionsIfUsedTogetherTest {

    private static final String TestSrc =
        "public class Test {\n" +
        "    javax.swing.JButton b;\n" +
        "}";

    public static void main(String args[]) throws Exception {
        ToolBox tb = new ToolBox();
        tb.writeFile("Test.java", TestSrc);

        Task.Result result = new JavacTask(tb, Task.Mode.CMDLINE)
                .options("-profile", "compact1",
                        "-bootclasspath", Paths.get(ToolBox.testJDK, "jre/lib/rt.jar").toString())
                .files("Test.java")
                .run(Task.Expect.FAIL)
                .writeAll();

        String out = result.getOutput(Task.OutputKind.DIRECT);
        Assert.check(out.startsWith(
                "javac: profile and bootclasspath options cannot be used together"),
                "Incorrect javac error output");
    }

}
