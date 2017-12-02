/*
 * Copyright (c) 2017, Oracle and/or its affiliates. All rights reserved.
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
 * @test TestVerifyGCType
 * @summary Test the VerifyGCType flag to ensure basic functionality.
 * @key gc
 * @requires vm.gc.G1
 * @library /test/lib
 * @build sun.hotspot.WhiteBox
 * @run main ClassFileInstaller sun.hotspot.WhiteBox
 * @run driver TestVerifyGCType
 */

import java.util.ArrayList;
import java.util.Collections;

import jdk.test.lib.Asserts;
import jdk.test.lib.Utils;
import jdk.test.lib.process.OutputAnalyzer;
import jdk.test.lib.process.ProcessTools;
import sun.hotspot.WhiteBox;

public class TestVerifyGCType {
    public static final String VERIFY_TAG    = "[gc,verify]";
    public static final String VERIFY_BEFORE = "Verifying Before GC";
    public static final String VERIFY_DURING = "Verifying During GC";
    public static final String VERIFY_AFTER  = "Verifying After GC";

    public static void main(String args[]) throws Exception {
        testAllVerificationEnabled();
        testAllExplicitlyEnabled();
        testFullAndRemark();
        testConcurrentMark();
        testBadVerificationType();
        testUnsupportedCollector();
    }

    private static void testAllVerificationEnabled() throws Exception {
        // Test with all verification enabled
        OutputAnalyzer output = testWithVerificationType(new String[0]);
        output.shouldHaveExitValue(0);

        verifyCollection("Pause Young", true, false, true, output.getStdout());
        verifyCollection("Pause Initial Mark", true, false, true, output.getStdout());
        verifyCollection("Pause Mixed", true, false, true, output.getStdout());
        verifyCollection("Pause Remark", false, true, false, output.getStdout());
        verifyCollection("Pause Cleanup", false, true, false, output.getStdout());
        verifyCollection("Pause Full", true, true, true, output.getStdout());
    }

    private static void testAllExplicitlyEnabled() throws Exception {
        OutputAnalyzer output;
        // Test with all explicitly enabled
        output = testWithVerificationType(new String[] {
                "young-only", "initial-mark", "mixed", "remark", "cleanup", "full"});
        output.shouldHaveExitValue(0);

        verifyCollection("Pause Young", true, false, true, output.getStdout());
        verifyCollection("Pause Initial Mark", true, false, true, output.getStdout());
        verifyCollection("Pause Mixed", true, false, true, output.getStdout());
        verifyCollection("Pause Remark", false, true, false, output.getStdout());
        verifyCollection("Pause Cleanup", false, true, false, output.getStdout());
        verifyCollection("Pause Full", true, true, true, output.getStdout());
    }

    private static void testFullAndRemark() throws Exception {
        OutputAnalyzer output;
        // Test with full and remark
        output = testWithVerificationType(new String[] {"remark", "full"});
        output.shouldHaveExitValue(0);

        verifyCollection("Pause Young", false, false, false, output.getStdout());
        verifyCollection("Pause Initial Mark", false, false, false, output.getStdout());
        verifyCollection("Pause Mixed", false, false, false, output.getStdout());
        verifyCollection("Pause Remark", false, true, false, output.getStdout());
        verifyCollection("Pause Cleanup", false, false, false, output.getStdout());
        verifyCollection("Pause Full", true, true, true, output.getStdout());
    }

    private static void testConcurrentMark() throws Exception {
        OutputAnalyzer output;
        // Test with full and remark
        output = testWithVerificationType(new String[] {"initial-mark", "cleanup", "remark"});
        output.shouldHaveExitValue(0);

        verifyCollection("Pause Young", false, false, false, output.getStdout());
        verifyCollection("Pause Initial Mark", true, false, true, output.getStdout());
        verifyCollection("Pause Mixed", false, false, false, output.getStdout());
        verifyCollection("Pause Remark", false, true, false, output.getStdout());
        verifyCollection("Pause Cleanup", false, true, false, output.getStdout());
        verifyCollection("Pause Full", false, false, false, output.getStdout());
    }

    private static void testBadVerificationType() throws Exception {
        OutputAnalyzer output;
        // Test bad type
        output = testWithVerificationType(new String[] {"old"});
        output.shouldHaveExitValue(0);

        output.shouldMatch("VerifyGCType: '.*' is unknown. Available types are: young-only, initial-mark, mixed, remark, cleanup and full");
        verifyCollection("Pause Young", true, false, true, output.getStdout());
        verifyCollection("Pause Initial Mark", true, false, true, output.getStdout());
        verifyCollection("Pause Mixed", true, false, true, output.getStdout());
        verifyCollection("Pause Remark", false, true, false, output.getStdout());
        verifyCollection("Pause Cleanup", false, true, false, output.getStdout());
        verifyCollection("Pause Full", true, true, true, output.getStdout());
    }

    private static void testUnsupportedCollector() throws Exception {
        OutputAnalyzer output;
        // Test bad gc
        output = testWithBadGC();
        output.shouldHaveExitValue(0);
        output.shouldMatch("VerifyGCType is not supported by this collector.");
    }

    private static OutputAnalyzer testWithVerificationType(String[] types) throws Exception {
        ArrayList<String> basicOpts = new ArrayList<>();
        Collections.addAll(basicOpts, new String[] {
                                       "-Xbootclasspath/a:.",
                                       "-XX:+UnlockDiagnosticVMOptions",
                                       "-XX:+UseG1GC",
                                       "-XX:+WhiteBoxAPI",
                                       "-XX:+ExplicitGCInvokesConcurrent",
                                       "-Xlog:gc,gc+start,gc+verify=info",
                                       "-XX:+VerifyBeforeGC",
                                       "-XX:+VerifyAfterGC",
                                       "-XX:+VerifyDuringGC"});

        for(String verifyType : types) {
            basicOpts.add("-XX:VerifyGCType="+verifyType);
        }

        basicOpts.add(TriggerGCs.class.getName());

        ProcessBuilder procBuilder =  ProcessTools.createJavaProcessBuilder(basicOpts.toArray(
                                                                            new String[basicOpts.size()]));
        OutputAnalyzer analyzer = new OutputAnalyzer(procBuilder.start());
        return analyzer;
    }

    private static OutputAnalyzer testWithBadGC() throws Exception {
        ProcessBuilder procBuilder =  ProcessTools.createJavaProcessBuilder(new String[] {
                "-XX:+UseParallelGC",
                "-XX:+UnlockDiagnosticVMOptions",
                "-XX:VerifyGCType=full",
                "-version"});

        OutputAnalyzer analyzer = new OutputAnalyzer(procBuilder.start());
        return analyzer;
    }

    private static void verifyCollection(String name, boolean expectBefore, boolean expectDuring, boolean expectAfter, String data) {
        CollectionInfo ci = CollectionInfo.parseFirst(name, data);
        Asserts.assertTrue(ci != null, "Expected GC not found: " + name);

        // Verify Before
        verifyType(ci, expectBefore, VERIFY_BEFORE);
        // Verify During
        verifyType(ci, expectDuring, VERIFY_DURING);
        // Verify After
        verifyType(ci, expectAfter, VERIFY_AFTER);
    }

    private static void verifyType(CollectionInfo ci, boolean shouldExist, String pattern) {
        if (shouldExist) {
            Asserts.assertTrue(ci.containsVerification(pattern), "Missing expected verification for: " + ci.getName());
        } else {
            Asserts.assertFalse(ci.containsVerification(pattern), "Found unexpected verification for: " + ci.getName());
        }
    }

    public static class CollectionInfo {
        String name;
        ArrayList<String> verification;
        public CollectionInfo(String name) {
            this.name = name;
            this.verification = new ArrayList<>();
            System.out.println("Created CollectionInfo: " + name);
        }

        public String getName() {
            return name;
        }

        public void addVerification(String verify) {
            System.out.println("Adding: " + verify);
            verification.add(verify);
        }

        public boolean containsVerification(String contains) {
            for (String entry : verification) {
                if (entry.contains(contains)) {
                    return true;
                }
            }
            return false;
        }

        static CollectionInfo parseFirst(String name, String data) {
            CollectionInfo result = null;
            int firstIndex = data.indexOf(name);
            if (firstIndex == -1) {
                return result;
            }
            int nextIndex = data.indexOf(name, firstIndex + 1);
            if (nextIndex == -1) {
                return result;
            }
            // Found an entry for this name
            result = new CollectionInfo(name);
            String collectionData = data.substring(firstIndex, nextIndex + name.length());
            for (String line : collectionData.split(System.getProperty("line.separator"))) {
                if (line.contains(VERIFY_TAG)) {
                    result.addVerification(line);
                }
            }
            return result;
        }
    }

    public static class TriggerGCs {
        public static void main(String args[]) throws Exception {
            WhiteBox wb = WhiteBox.getWhiteBox();
            // Trigger the different GCs using the WhiteBox API and System.gc()
            // to start a concurrent cycle with -XX:+ExplicitGCInvokesConcurrent.
            wb.fullGC();  // full
            System.gc();  // initial-mark, remark and cleanup
            // Sleep to make sure concurrent cycle is done
            Thread.sleep(1000);
            wb.youngGC(); // young-only
            wb.youngGC(); // mixed
        }
    }
}
