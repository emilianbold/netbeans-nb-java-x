/*
 * Copyright (c) 2018, Oracle and/or its affiliates. All rights reserved.
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

/**
 * @test
 * @summary CDS dumping with JVMTI agent.
 * @requires vm.cds
 * @requires vm.flavor != "minimal"
 * @library /test/lib /test/hotspot/jtreg/runtime/appcds
 * @modules jdk.jartool/sun.tools.jar
 * @compile ../../test-classes/Hello.java
 * @run main/othervm/native DumpingWithJvmtiAgent
 */

import java.io.File;
import java.nio.file.Path;
import java.nio.file.Paths;
import jdk.test.lib.process.OutputAnalyzer;

public class DumpingWithJvmtiAgent {
    private static final String AGENT_LIB_ONLOAD = "AddToSystemCLSearchOnLoad";

    public static void main(String[] args) throws Exception {
        String appJar = JarBuilder.getOrCreateHelloJar();

        // CDS dump with a JVMTI agent with the AllowArchivingWithJavaAgent option.
        // vm should exit with an error message.
        OutputAnalyzer out = TestCommon.dump(
           appJar,
           TestCommon.list("Hello"),
           "-XX:+UnlockDiagnosticVMOptions", "-XX:+AllowArchivingWithJavaAgent",
           "-agentlib:" + AGENT_LIB_ONLOAD + "=" + appJar,
           "-Djava.library.path=" + System.getProperty("java.library.path"));
        out.shouldContain("CDS dumping does not support native JVMTI agent, name: " + AGENT_LIB_ONLOAD)
           .shouldHaveExitValue(1);

        // CDS dump with a JVMTI agent without the AllowArchivingWithJavaAgent option.
        // vm should exit with an error message.
        out = TestCommon.dump(
           appJar,
           TestCommon.list("Hello"),
           "-agentlib:" + AGENT_LIB_ONLOAD + "=" + appJar,
           "-Djava.library.path=" + System.getProperty("java.library.path"));
        out.shouldContain("CDS dumping does not support native JVMTI agent, name: " + AGENT_LIB_ONLOAD)
           .shouldHaveExitValue(1);
    }
}
