/*
 * Copyright (c) 2014, 2019, Oracle and/or its affiliates. All rights reserved.
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
 *
 */

/*
 * @test
 * @summary AppCDS handling of package.
 * @requires vm.cds
 * @library /test/lib
 * @compile test-classes/C1.java
 * @compile test-classes/C2.java
 * @compile test-classes/PackageSealingTest.java
 * @compile test-classes/Hello.java
 * @run driver PackageSealing
 */

import java.io.File;
import jdk.test.lib.process.OutputAnalyzer;

public class PackageSealing {
    public static void main(String args[]) throws Exception {
        String[] classList = {"sealed/pkg/C1", "pkg/C2", "PackageSealingTest"};
        String appJar = ClassFileInstaller.writeJar("pkg_seal.jar",
            ClassFileInstaller.Manifest.fromSourceFile("test-classes/package_seal.mf"),
            "PackageSealingTest", "sealed/pkg/C1", "pkg/C2");

        String helloJar = JarBuilder.getOrCreateHelloJar();
        String jars = helloJar + File.pathSeparator + appJar;

        // test shared package from -cp path
        TestCommon.testDump(jars, TestCommon.list(classList));
        OutputAnalyzer output;
        output = TestCommon.exec(jars, "PackageSealingTest");
        TestCommon.checkExec(output, "OK");

        // test shared package from -Xbootclasspath/a
        TestCommon.dump(helloJar, TestCommon.list(classList),
                        "-Xbootclasspath/a:" + appJar);
        output = TestCommon.exec(helloJar, "-Xbootclasspath/a:" + appJar, "PackageSealingTest");
        TestCommon.checkExec(output, "OK");
    }
}
