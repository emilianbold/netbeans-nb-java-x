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
 *
 */

/*
 * @test
 * @summary Tests how CDS works when critical library classes are replaced with JVMTI ClassFileLoadHook
 * @library /test/lib
 * @requires vm.cds.archived.java.heap
 * @build sun.hotspot.WhiteBox
 * @run driver ClassFileInstaller -jar whitebox.jar sun.hotspot.WhiteBox
 * @run main/othervm/native ReplaceCriticalClassesForSubgraphs
 */

public class ReplaceCriticalClassesForSubgraphs extends ReplaceCriticalClasses {
    public static void main(String args[]) throws Throwable {
        ReplaceCriticalClassesForSubgraphs rcc = new ReplaceCriticalClassesForSubgraphs();
        rcc.process(args);
    }

    public String[] getTests() {
        String tests[] = {
            // Try to replace classes that are used by the archived subgraph graphs.
            "-early -notshared -subgraph java/lang/module/ResolvedModule jdk.internal.module.ArchivedModuleGraph",
            "-early -notshared -subgraph java/lang/Long java.lang.Long$LongCache",
            "-subgraph java/lang/Long java.lang.Long$LongCache",
        };
        return tests;
    }
}
