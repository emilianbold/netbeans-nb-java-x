/*
 * Copyright (c) 2015, 2016, Oracle and/or its affiliates. All rights reserved.
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
 * @summary tests for modfule declarations
 * @library /tools/lib
 * @modules
 *      jdk.compiler/com.sun.tools.javac.api
 *      jdk.compiler/com.sun.tools.javac.main
 *      jdk.jdeps/com.sun.tools.javap
 * @build ToolBox ModuleTestBase
 * @run main ModuleInfoTest
 */

import java.nio.file.Files;
import java.nio.file.Path;

public class ModuleInfoTest extends ModuleTestBase {

    public static void main(String... args) throws Exception {
        ModuleInfoTest t = new ModuleInfoTest();
        t.runTests();
    }

    /**
     * Check error message if module declaration not in module-info.java.
     */
    @Test
    void testModuleDeclNotInModuleJava(Path base) throws Exception {
        Path src = base.resolve("src");
        tb.writeFile(src.resolve("M.java"), "module M { }");
        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics")
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("M.java:1:1: compiler.err.module.decl.sb.in.module-info.java"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that a package private class can be put in module-info.java.
     */
    @Test
    void testNotModuleDeclInModuleJava_1(Path base) throws Exception {
        Path src = base.resolve("src");
        tb.writeFile(src.resolve("module-info.java"), "class C { }");
        tb.new JavacTask()
                .options("-XDrawDiagnostics")
                .files(findJavaFiles(src))
                .run()
                .writeAll();
    }

    /**
     * Verify that a public class cannot be put in module-info.java.
     */
    @Test
    void testNotModuleDeclInModuleJava_2(Path base) throws Exception {
        Path src = base.resolve("src");
        tb.writeFile(src.resolve("module-info.java"), "public class C { }");
        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics")
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:8: compiler.err.class.public.should.be.in.file: C"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that only one module decl can be put in module-info.java.
     */
    @Test
    void testSingleModuleDecl(Path base) throws Exception {
        Path src = base.resolve("src");
        tb.writeJavaFiles(src, "module M1 { } /*...*/ module M2 { }");
        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics")
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:14: compiler.err.expected: token.end-of-input"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that missing requires are reported.
     */
    @Test
    void testRequiresNotFound(Path base) throws Exception {
        Path src = base.resolve("src");
        tb.writeJavaFiles(src, "module M1 { requires M2; }");
        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics")
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:22: compiler.err.module.not.found: M2"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that missing exports are reported.
     */
    @Test
    void testExportsNotFound(Path base) throws Exception {
        Path src = base.resolve("src");
        tb.writeJavaFiles(src, "module M1 { exports p to M2; }");
        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics")
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:26: compiler.err.module.not.found: M2"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that a simple loop is detected.
     */
    @Test
    void testRequiresSelf(Path base) throws Exception {
        Path src = base.resolve("src");
        tb.writeJavaFiles(src, "module M { requires M; }");
        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics")
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:21: compiler.err.cyclic.requires: M"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that a multi-module loop is detected.
     */
    @Test
    void testRequiresLoop(Path base) throws Exception {
        Path src = base.resolve("src");
        Path src_m1 = src.resolve("m1");
        tb.writeFile(src_m1.resolve("module-info.java"), "module m1 { requires m2; }");
        Path src_m2 = src.resolve("m2");
        tb.writeFile(src_m2.resolve("module-info.java"), "module m2 { requires m3; }");
        Path src_m3 = src.resolve("m3");
        tb.writeFile(src_m3.resolve("module-info.java"), "module m3 { requires m1; }");

        Path classes = base.resolve("classes");
        Files.createDirectories(classes);

        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics", "-modulesourcepath", src.toString())
                .outdir(classes)
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:22: compiler.err.cyclic.requires: m3"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that a multi-module loop is detected.
     */
    @Test
    void testRequiresPublicLoop(Path base) throws Exception {
        Path src = base.resolve("src");
        Path src_m1 = src.resolve("m1");
        tb.writeFile(src_m1.resolve("module-info.java"), "module m1 { requires m2; }");
        Path src_m2 = src.resolve("m2");
        tb.writeFile(src_m2.resolve("module-info.java"), "module m2 { requires public m3; }");
        Path src_m3 = src.resolve("m3");
        tb.writeFile(src_m3.resolve("module-info.java"), "module m3 { requires m1; }");

        Path classes = base.resolve("classes");
        Files.createDirectories(classes);

        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics", "-modulesourcepath", src.toString())
                .outdir(classes)
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:29: compiler.err.cyclic.requires: m3"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that duplicate requires are detected.
     */
    @Test
    void testDuplicateRequires(Path base) throws Exception {
        Path src = base.resolve("src");
        Path src_m1 = src.resolve("m1");
        tb.writeFile(src_m1.resolve("module-info.java"), "module m1 { }");
        Path src_m2 = src.resolve("m2");
        tb.writeFile(src_m2.resolve("module-info.java"), "module m2 { requires m1; requires m1; }");

        Path classes = base.resolve("classes");
        Files.createDirectories(classes);

        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics", "-modulesourcepath", src.toString())
                .outdir(classes)
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:35: compiler.err.duplicate.requires: m1"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that duplicate exported packages are detected.
     */
    @Test
    void testDuplicateExports_packages(Path base) throws Exception {
        Path src = base.resolve("src");
        tb.writeJavaFiles(src, "module m1 { exports p; exports p; }");

        Path classes = base.resolve("classes");
        Files.createDirectories(classes);

        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics")
                .outdir(classes)
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:32: compiler.err.duplicate.exports: p"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that duplicate exported packages are detected.
     */
    @Test
    void testDuplicateExports_packages2(Path base) throws Exception {
        Path src = base.resolve("src");
        tb.writeJavaFiles(src.resolve("m1"), "module m1 { exports p; exports p to m2; }");
        tb.writeJavaFiles(src.resolve("m2"), "module m2 { }");

        Path classes = base.resolve("classes");
        Files.createDirectories(classes);

        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics", "-modulesourcepath", src.toString())
                .outdir(classes)
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:32: compiler.err.duplicate.exports: p"))
            throw new Exception("expected output not found");
    }

    /**
     * Verify that duplicate exported packages are detected.
     */
    @Test
    void testDuplicateExports_modules(Path base) throws Exception {
        Path src = base.resolve("src");
        Path src_m1 = src.resolve("m1");
        tb.writeFile(src_m1.resolve("module-info.java"), "module m1 { }");
        Path src_m2 = src.resolve("m2");
        tb.writeFile(src_m2.resolve("module-info.java"), "module m2 { exports p to m1, m1; }");

        Path classes = base.resolve("classes");
        Files.createDirectories(classes);

        String log = tb.new JavacTask()
                .options("-XDrawDiagnostics", "-modulesourcepath", src.toString())
                .outdir(classes)
                .files(findJavaFiles(src))
                .run(ToolBox.Expect.FAIL)
                .writeAll()
                .getOutput(ToolBox.OutputKind.DIRECT);

        if (!log.contains("module-info.java:1:30: compiler.err.duplicate.exports: m1"))
            throw new Exception("expected output not found");
    }
}
