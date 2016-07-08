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

/*
 * @test
 * @bug 8136421
 * @requires (vm.simpleArch == "x64" | vm.simpleArch == "sparcv9" | vm.simpleArch == "aarch64")
 * @library /testlibrary /test/lib /
 * @library ../common/patches
 * @modules java.base/jdk.internal.misc
 * @modules java.base/jdk.internal.org.objectweb.asm
 *          java.base/jdk.internal.org.objectweb.asm.tree
 *          jdk.vm.ci/jdk.vm.ci.hotspot
 *          jdk.vm.ci/jdk.vm.ci.code
 *
 * @ignore 8139700
 * @build jdk.vm.ci/jdk.vm.ci.hotspot.CompilerToVMHelper
 * @build sun.hotspot.WhiteBox
 *        compiler.jvmci.compilerToVM.DisassembleCodeBlobTest
 * @run driver ClassFileInstaller sun.hotspot.WhiteBox
 *                                sun.hotspot.WhiteBox$WhiteBoxPermission
 * @run main/othervm -Xbootclasspath/a:.
 *                   -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI
 *                   -XX:+UnlockExperimentalVMOptions -XX:+EnableJVMCI
 *                   compiler.jvmci.compilerToVM.DisassembleCodeBlobTest
 */

package compiler.jvmci.compilerToVM;

import jdk.vm.ci.hotspot.CompilerToVMHelper;
import jdk.vm.ci.code.InstalledCode;
import jdk.test.lib.Asserts;
import sun.hotspot.code.NMethod;

import java.util.List;
import jdk.test.lib.Utils;

public class DisassembleCodeBlobTest {

    public static void main(String[] args) {
        DisassembleCodeBlobTest test
                = new DisassembleCodeBlobTest();
        List<CompileCodeTestCase> testCases
                = CompileCodeTestCase.generate(/* bci = */ -1);
        testCases.addAll(CompileCodeTestCase.generate(/* bci = */ 0));
        testCases.forEach(test::check);
        testCases.stream().findAny().ifPresent(test::checkZero);
        test.checkNull();
    }

    private void checkNull() {
        Utils.runAndCheckException(
                () -> CompilerToVMHelper.disassembleCodeBlob(null),
                NullPointerException.class);
    }

    private void checkZero(CompileCodeTestCase testCase) {
        System.out.println("checkZero for " + testCase);
        testCase.deoptimize();
        InstalledCode installedCode = testCase.toInstalledCode();
        String str = CompilerToVMHelper.disassembleCodeBlob(installedCode);
        Asserts.assertNull(str, testCase
                + " : non-null return value for invalid installCode");
    }

    private void check(CompileCodeTestCase testCase) {
        System.out.println(testCase);
        // to have a clean state
        NMethod nMethod = testCase.deoptimizeAndCompile();
        if (nMethod == null) {
            throw new Error(testCase + " : method is not compiled");
        }
        InstalledCode installedCode = testCase.toInstalledCode();
        String str = CompilerToVMHelper.disassembleCodeBlob(installedCode);
        if (str != null) {
            Asserts.assertGT(str.length(), 0,
                   testCase +  " : returned string has to be non-zero length");
        }
        String str2 = CompilerToVMHelper.disassembleCodeBlob(installedCode);
        Asserts.assertEQ(str, str2,
                testCase + " : 2nd invocation returned different value");
    }
}
