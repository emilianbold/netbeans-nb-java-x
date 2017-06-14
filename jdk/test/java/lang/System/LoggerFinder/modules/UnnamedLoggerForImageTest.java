/*
 * Copyright (c) 2017, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
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

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import jdk.testlibrary.JDKToolFinder;

import static jdk.testlibrary.ProcessTools.executeCommand;

/*
 * @test
 * @modules jdk.compiler
 * @summary Test cases which run against customized image, check the situation where
 *            1. logger providers are in unnamed module,
 *            2. clients are in named/unnamed module, image,
 *               patched system module, or Xbootclasspath
 *          This test does not require existence of java.logging module,
 *          but require jdk.compiler module
 * @library /lib/testlibrary /test/lib
 * @build Base jdk.test.lib.compiler.CompilerUtils jdk.testlibrary.*
 * @run main/othervm UnnamedLoggerForImageTest
 */

public class UnnamedLoggerForImageTest extends Base {

    public static void main(String args[]) throws Throwable {
        UnnamedLoggerForImageTest t = new UnnamedLoggerForImageTest();
        t.setup();
        t.test();
    }

    private void setup() throws Throwable {
        setupAllClient();

        setupUnnamedLogger();

        setupJavaBaseImage();
        setupClientImage();
    }

    private void test() throws Throwable {
        if (!checkJMODS()) {
            return;
        }

        // logger client is in unnamed module
        runTest(IMAGE,
                "--class-path", DEST_UNNAMED_LOGGER.toString()
                    + File.pathSeparator + DEST_UNNAMED_CLIENT.toString(),
                CLIENT_B, "unnamed", LOGGER_B);
        // logger client is in named module m.t.a
        runTest(IMAGE,
                "--class-path", DEST_UNNAMED_LOGGER.toString(),
                "--module-path", DEST_NAMED_CLIENT.toString(),
                "-m", CLIENT_A, "unnamed", LOGGER_B);
        // logger client is in named module m.t.a which is in customized image
        runTest(IMAGE_CLIENT,
                "--class-path", DEST_UNNAMED_LOGGER.toString(),
                "-m", CLIENT_A, "unnamed", LOGGER_B);
        // logger client gets logger through boot class BootUsage
        runTest(IMAGE,
                "--class-path", DEST_UNNAMED_LOGGER.toString()
                    + File.pathSeparator + DEST_BOOT_CLIENT.toString(),
                "-Xbootclasspath/a:" + DEST_BOOT_USAGE.toString(),
                BOOT_CLIENT, "system", LAZY_LOGGER, LOGGER_B);
        // logger client gets logger through patched class
        // java.base/java.lang.PatchedUsage
        runTest(IMAGE,
                "--class-path", DEST_UNNAMED_LOGGER.toString()
                    + File.pathSeparator + DEST_PATCHED_CLIENT.toString(),
                "--patch-module", "java.base=" + DEST_PATCHED_USAGE.toString(),
                PATCHED_CLIENT, "system", LAZY_LOGGER, LOGGER_B);
    }
}
