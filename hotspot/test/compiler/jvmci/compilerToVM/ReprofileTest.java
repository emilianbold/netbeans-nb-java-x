/*
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved.
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

/**
 * @test
 * @bug 8136421
 * @requires (os.simpleArch == "x64" | os.simpleArch == "sparcv9") & os.arch != "aarch64"
 * @library /testlibrary /../../test/lib /
 * @compile ../common/CompilerToVMHelper.java
 * @build sun.hotspot.WhiteBox
 * @run main ClassFileInstaller
 *      sun.hotspot.WhiteBox
 *      sun.hotspot.WhiteBox$WhiteBoxPermission
 *      jdk.vm.ci.hotspot.CompilerToVMHelper
 * @run main/othervm -XX:+UnlockExperimentalVMOptions -XX:+EnableJVMCI
 *      -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI -Xbootclasspath/a:.
 *      -Xmixed
 *      compiler.jvmci.compilerToVM.ReprofileTest
 */

package compiler.jvmci.compilerToVM;

import compiler.jvmci.common.CTVMUtilities;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import jdk.vm.ci.hotspot.HotSpotResolvedJavaMethodImpl;
import jdk.vm.ci.hotspot.CompilerToVMHelper;
import jdk.vm.ci.meta.ProfilingInfo;
import jdk.test.lib.Asserts;
import jdk.test.lib.Utils;
import sun.hotspot.WhiteBox;

public class ReprofileTest {

    private static final WhiteBox WB = WhiteBox.getWhiteBox();

    public static void main(String[] args) {
        List<Method> testCases = createTestCases();
        testCases.forEach(ReprofileTest::runSanityTest);
    }

    private static List<Method> createTestCases() {
        List<Method> testCases = new ArrayList<>();
        try {

            Class<?> aClass = DummyClass.class;
            testCases.add(aClass.getMethod("withLoop"));

            aClass = DummyClass.class;
            testCases.add(aClass.getDeclaredMethod("dummyFunction"));
        } catch (NoSuchMethodException e) {
            throw new Error("TEST BUG " + e.getMessage(), e);
        }
        return testCases;
    }

    private static void runSanityTest(Method aMethod) {
        HotSpotResolvedJavaMethodImpl method = CTVMUtilities
                .getResolvedMethod(aMethod);
        ProfilingInfo startProfile = method.getProfilingInfo();
        Asserts.assertFalse(startProfile.isMature(), aMethod
                + " : profiling info is mature in the begging");

        long compileThreshold = (Long) WB.getVMFlag("CompileThreshold");
        // make interpreter to profile this method
        try {
            Object obj = aMethod.getDeclaringClass().newInstance();
            for (long i = 0; i < compileThreshold; i++) {
                aMethod.invoke(obj);
            }
        } catch (ReflectiveOperationException e) {
            throw new Error("TEST BUG : " + e.getMessage(), e);
        }
        ProfilingInfo compProfile = method.getProfilingInfo();

        Asserts.assertNE(startProfile.toString(), compProfile.toString(),
                String.format("%s : profiling info wasn't changed after "
                                + "%d invocations",
                        aMethod, compileThreshold));
        Asserts.assertTrue(compProfile.isMature(),
                String.format("%s is not mature after %d invocations",
                        aMethod, compileThreshold));

        CompilerToVMHelper.reprofile(method);
        ProfilingInfo reprofiledProfile = method.getProfilingInfo();

        Asserts.assertNE(startProfile.toString(), reprofiledProfile.toString(),
                aMethod + " : profiling info wasn't changed after reprofiling");
        Asserts.assertNE(compProfile.toString(), reprofiledProfile.toString(),
                aMethod + " : profiling info didn't change after reprofile");
        Asserts.assertFalse(reprofiledProfile.isMature(), aMethod
                + " : profiling info is mature after reprofiling");
    }
}
