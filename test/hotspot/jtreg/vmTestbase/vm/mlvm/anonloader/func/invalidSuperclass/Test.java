/*
 * Copyright (c) 2010, 2018, Oracle and/or its affiliates. All rights reserved.
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
 * @modules java.base/jdk.internal.misc java.base/jdk.internal.org.objectweb.asm
 *
 * @summary converted from VM Testbase vm/mlvm/anonloader/func/invalidSuperclass.
 * VM Testbase keywords: [feature_mlvm]
 *
 * @library /vmTestbase
 *          /test/lib
 * @run driver jdk.test.lib.FileInstaller . .
 *
 * @comment build test class and indify classes
 * @build vm.mlvm.anonloader.func.invalidSuperclass.Test
 * @run driver vm.mlvm.share.IndifiedClassesBuilder
 *
 * @run main/othervm -Xverify:all vm.mlvm.anonloader.func.invalidSuperclass.Test
 */

package vm.mlvm.anonloader.func.invalidSuperclass;

import vm.mlvm.anonloader.share.ReplaceClassParentTest;
import vm.mlvm.share.MlvmTestExecutor;

/**
 * Using Unsafe.defineAnonymousClass to load a class that has a patched superclass name, which is invalid.
 * Verify that such class cannot be loaded.
 *
 */
public class Test extends ReplaceClassParentTest {

    @Override
    protected void initializeTest() throws Throwable {
        super.initializeTest();
        setReplaceParent(String.format("%9999s", "Can you find me?"));
        setRequiredExceptions(java.lang.NoClassDefFoundError.class);
    }

    public static void main(String[] args) {
        MlvmTestExecutor.launch(args);
    }
}
