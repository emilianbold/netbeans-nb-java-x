/*
 * Copyright (c) 2016, Oracle and/or its affiliates. All rights reserved.
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

/*
 * @test
 * @summary class p3.c3 defined in module m1 tries to access c4 defined in an unnamed package
 *          and an unnamed module.
 * @modules java.base/jdk.internal.misc
 * @library /testlibrary /test/lib
 * @compile myloaders/MyDiffClassLoader.java
 * @compile c4.java
 * @compile p3/c3.jcod
 * @compile p3/c3ReadEdgeDiffLoader.jcod
 * @build DiffCL_UmodUpkg
 * @run main/othervm -Xbootclasspath/a:. DiffCL_UmodUpkg
 */

import static jdk.test.lib.Asserts.*;

import java.lang.reflect.Layer;
import java.lang.module.Configuration;
import java.lang.module.ModuleDescriptor;
import java.lang.module.ModuleFinder;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import myloaders.MyDiffClassLoader;

//
// ClassLoader1 --> defines m1 --> packages p3
//                  package p3 in m1 is exported unqualifiedly
//
// class p3.c3 defined in m1 tries to access c4 defined in
// in unnamed module.
//
// Two access attempts occur in this test:
//   1. The first access is not allowed because a strict module
//      cannot read an unnamed module.
//   2. In this scenario a strict module establishes readability
//      to the particular unnamed module it is trying to access.
//      Access is allowed.
//
public class DiffCL_UmodUpkg {

 // Create Layers over the boot layer to test different
 // accessing scenarios of a named module to an unnamed module.

 // Module m1 is a strict module and has not established
 // readability to an unnamed module that c4 is defined in.
 public void test_strictModuleLayer() throws Throwable {

     // Define module:     m1
     // Can read:          java.base
     // Packages:          p3
     // Packages exported: p3 is exported unqualifiedly
     ModuleDescriptor descriptor_m1 =
             new ModuleDescriptor.Builder("m1")
                     .requires("java.base")
                     .exports("p3")
                     .build();

     // Set up a ModuleFinder containing all modules for this layer.
     ModuleFinder finder = ModuleLibrary.of(descriptor_m1);

     // Resolves "m1"
     Configuration cf = Layer.boot()
             .configuration()
             .resolveRequires(finder, ModuleFinder.empty(), Set.of("m1"));

     MyDiffClassLoader.loader1 = new MyDiffClassLoader();
     MyDiffClassLoader.loader2 = new MyDiffClassLoader();

     // map module m1 to class loader.
     // class c2 will be loaded in an unnamed module/loader2
     // to achieve differing class loaders.
     Map<String, ClassLoader> map = new HashMap<>();
     map.put("m1", MyDiffClassLoader.loader1);

     // Create Layer that contains m1
     Layer layer = Layer.boot().defineModules(cf, map::get);

     assertTrue(layer.findLoader("m1") == MyDiffClassLoader.loader1);
     assertTrue(layer.findLoader("java.base") == null);

     // now use the same loader to load class p3.c3
     Class p3_c3_class = MyDiffClassLoader.loader1.loadClass("p3.c3");

     // Attempt access
     try {
         p3_c3_class.newInstance();
         throw new RuntimeException("Test Failed, strict module m1 should not be able to access " +
                                    "public type c4 defined in unnamed module");
     } catch (IllegalAccessError e) {
     }
}

 // Module m1 is a strict module and has established
 // readability to an unnamed module that c4 is defined in.
 public void test_strictModuleUnnamedReadableLayer() throws Throwable {

     // Define module:     m1
     // Can read:          java.base
     // Packages:          p3
     // Packages exported: p3 is exported unqualifiedly
     ModuleDescriptor descriptor_m1 =
             new ModuleDescriptor.Builder("m1")
                     .requires("java.base")
                     .exports("p3")
                     .build();

     // Set up a ModuleFinder containing all modules for this layer.
     ModuleFinder finder = ModuleLibrary.of(descriptor_m1);

     // Resolves "m1"
     Configuration cf = Layer.boot()
             .configuration()
             .resolveRequires(finder, ModuleFinder.empty(), Set.of("m1"));

     MyDiffClassLoader.loader1 = new MyDiffClassLoader();
     MyDiffClassLoader.loader2 = new MyDiffClassLoader();

     // map module m1 to class loader.
     // class c2 will be loaded in an unnamed module/loader2
     // to achieve differing class loaders.
     Map<String, ClassLoader> map = new HashMap<>();
     map.put("m1", MyDiffClassLoader.loader1);

     // Create Layer that contains m1
     Layer layer = Layer.boot().defineModules(cf, map::get);

     assertTrue(layer.findLoader("m1") == MyDiffClassLoader.loader1);
     assertTrue(layer.findLoader("java.base") == null);

     // now use the same loader to load class p3.c3ReadEdgeDiffLoader
     Class p3_c3_class = MyDiffClassLoader.loader1.loadClass("p3.c3ReadEdgeDiffLoader");

     try {
        // Read edge between m1 and the unnamed module that loads c4 is established in
        // C3ReadEdgeDiffLoader's ctor before attempting access.
        p3_c3_class.newInstance();
     } catch (IllegalAccessError e) {
         throw new RuntimeException("Test Failed, module m1 has established readability to " +
                                    "c4 loader's unnamed module, access should be allowed: " + e.getMessage());
     }
 }

 public static void main(String args[]) throws Throwable {
   DiffCL_UmodUpkg test = new DiffCL_UmodUpkg();
   test.test_strictModuleLayer();                // access denied
   test.test_strictModuleUnnamedReadableLayer(); // access allowed
 }
}
