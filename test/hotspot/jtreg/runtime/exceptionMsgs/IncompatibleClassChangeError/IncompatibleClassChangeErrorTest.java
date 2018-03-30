/*
 * Copyright (c) 2018, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2018 SAP SE. All rights reserved.
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
 * @summary Check that the verbose message of ICCE is printed correctly.
 *          The test forces errors in vtable stubs and interpreter.
 * @requires !(os.arch=="arm") & vm.flavor == "server" & !vm.emulatedClient & vm.compMode=="Xmixed" & (!vm.graal.enabled | vm.opt.TieredCompilation == true) & (vm.opt.TieredStopAtLevel == null | vm.opt.TieredStopAtLevel==4)
 * @library /test/lib /
 * @build sun.hotspot.WhiteBox
 * @run driver ClassFileInstaller sun.hotspot.WhiteBox sun.hotspot.WhiteBox$WhiteBoxPermission
 * @compile IncompatibleClassChangeErrorTest.java
 * @compile ImplementsSomeInterfaces.jasm ICC_B.jasm
 * @run main/othervm -Xbootclasspath/a:. -XX:+UnlockDiagnosticVMOptions -XX:+WhiteBoxAPI
 *                   -XX:CompileThreshold=1000 -XX:-BackgroundCompilation -XX:-Inline
 *                   -XX:CompileCommand=exclude,IncompatibleClassChangeErrorTest::test_iccInt
 *                   IncompatibleClassChangeErrorTest
 */

import sun.hotspot.WhiteBox;
import compiler.whitebox.CompilerWhiteBoxTest;
import java.lang.reflect.Method;

// This test assembles an errorneous installation of classes.
// First, compile the test by @compile. This results in a legal set
// of classes.
// Then, with jasm, generate incompatible classes that overwrite
// the class files in the build directory.
// Last, call the real tests throwing IncompatibleClassChangeErrors
// and check the messages generated.
public class IncompatibleClassChangeErrorTest {

    private static final WhiteBox WHITE_BOX = WhiteBox.getWhiteBox();

    private static boolean enableChecks = true;

    private static String expectedErrorMessageInterpreted =
        "Class ImplementsSomeInterfaces " +
        "does not implement the requested interface InterfaceICCE1";
    private static String expectedErrorMessageCompiled =
        "Class ICC_B does not implement the requested interface ICC_iB";
        // old message: "vtable stub"


    private static boolean compile(Class<?> clazz, String name) {
        try {
            Method method = clazz.getMethod(name);
            boolean enqueued = WHITE_BOX.enqueueMethodForCompilation(method, CompilerWhiteBoxTest.COMP_LEVEL_FULL_OPTIMIZATION);
            if (!enqueued) {
                System.out.println("Warning: Blocking compilation failed for " + clazz.getName() + "." + name + " (timeout?)");
                return false;
            } else if (!WHITE_BOX.isMethodCompiled(method)) {
                throw new RuntimeException(clazz.getName() + "." + name + " is not compiled");
            }
        } catch (NoSuchMethodException e) {
            throw new RuntimeException(clazz.getName() + "." + name + " not found", e);
        }
        return true;
    }

    public static boolean setup_test() {
        // Assure all exceptions are loaded.
        new AbstractMethodError();
        new IncompatibleClassChangeError();

        enableChecks = false;
        // Warmup
        System.out.println("warmup:");
        test_iccInt();
        test_icc_compiled_itable_stub();
        enableChecks = true;

        // Compile
        if (!compile(IncompatibleClassChangeErrorTest.class, "test_icc_compiled_itable_stub") ||
            !compile(ICC_C.class, "b") ||
            !compile(ICC_D.class, "b") ||
            !compile(ICC_E.class, "b")) {
          return false;
        }

        System.out.println("warmup done.");
        return true;
    }

    // Should never be compiled.
    public static void test_iccInt() {
        boolean caught_icc = false;
        try {
            InterfaceICCE1 objectInterface = new ImplementsSomeInterfaces();
            // IncompatibleClassChangeError gets thrown in
            // - TemplateTable::invokeinterface()
            // - LinkResolver::runtime_resolve_interface_method()
            objectInterface.aFunctionOfMyInterface();
        } catch (IncompatibleClassChangeError e) {
            String errorMsg = e.getMessage();
            if (enableChecks && !errorMsg.equals(expectedErrorMessageInterpreted)) {
                System.out.println("Expected: " + expectedErrorMessageInterpreted + "\n" +
                                   "but got:  " + errorMsg);
                throw new RuntimeException("Wrong error message of IncompatibleClassChangeError.");
            }
            caught_icc = true;
        } catch (Throwable e) {
            throw new RuntimeException("Caught unexpected exception: " + e);
        }

        // Check we got the exception.
        if (!caught_icc) {
            throw new RuntimeException("Expected IncompatibleClassChangeError was not thrown.");
        }
    }

    // -------------------------------------------------------------------------
    // Test AbstractMethodErrors detected in itable stubs.
    // Note: How can we verify that we really stepped through the vtable stub?
    // - Bimorphic inlining should not happen since we have no profiling data when
    //   we compile the method
    // - As a result, an inline cache call should be generated
    // - This inline cache call is patched into a real vtable call at the first
    //   re-resolve, which happens constantly during the first 10 iterations of the loop.
    // => we should be fine! :-)
    public static void test_icc_compiled_itable_stub() {
        // Allocated the objects we need and call a valid method.
        boolean caught_icc = false;
        ICC_B b = new ICC_B();
        ICC_C c = new ICC_C();
        ICC_D d = new ICC_D();
        ICC_E e = new ICC_E();
        b.a();
        c.a();
        d.a();
        e.a();

        try {
            final int iterations = 10;
            // Test: calls b.b() in the last iteration.
            for (int i = 0; i < iterations; i++) {
                ICC_iB a = b;
                if (i % 3 == 0 && i < iterations - 1) {
                    a = c;
                }
                if (i % 3 == 1 && i < iterations - 1) {
                    a = d;
                }
                if (i % 3 == 2 && i < iterations - 1) {
                    a = e;
                }
                a.b();
            }
        } catch (AbstractMethodError exc) {
            // It's a subclass of IncompatibleClassChangeError, so we must catch this first.
            System.out.println();
            System.out.println(exc);
            if (enableChecks) {
                String errorMsg = exc.getMessage();
                if (errorMsg == null) {
                    throw new RuntimeException("Caught unexpected AbstractMethodError with empty message.");
                }
                throw new RuntimeException("Caught unexpected AbstractMethodError.");
            }
        } catch (IncompatibleClassChangeError exc) {
            caught_icc = true;
            System.out.println();
            String errorMsg = exc.getMessage();
            if (enableChecks && errorMsg == null) {
                System.out.println(exc);
                throw new RuntimeException("Empty error message of IncompatibleClassChangeError.");
            }
            if (enableChecks &&
                !errorMsg.equals(expectedErrorMessageCompiled)) {
                System.out.println("Expected: " + expectedErrorMessageCompiled + "\n" +
                                   "but got:  " + errorMsg);
                System.out.println(exc);
                throw new RuntimeException("Wrong error message of IncompatibleClassChangeError.");
            }
            if (enableChecks) {
                System.out.println("Passed with message: " + errorMsg);
            }
        } catch (Throwable exc) {
            throw exc; // new RuntimeException("Caught unexpected exception: " + exc);
        }

        // Check we got the exception at some point.
        if (enableChecks && !caught_icc) {
            throw new RuntimeException("Expected IncompatibleClassChangeError was not thrown.");
        }
    }

    public static void main(String[] args) throws Exception {
        if (!setup_test()) {
            return;
        }
        test_iccInt();
        test_icc_compiled_itable_stub();
    }
}


// Helper classes to test incompatible class change in interpreter.
//
// The test also contains .jasm files with implementations
// of the classes that shall generate the errors.


//   I0         // interface defining aFunctionOfMyInterface()
//   |
//   |    I1    // interface
//   |     |
//   A0    |    // abstract class
//    \   /
//      C       // class not implementing I1 and
//                       not implementing I0::aFunctionOfMyInterface()
//
// Test is expected to throw error because of missing interface and not
// because of missing method.

interface InterfaceICCE0 {
    public String firstFunctionOfMyInterface0();
    public String secondFunctionOfMyInterface0();
}

interface InterfaceICCE1 {

    public String firstFunctionOfMyInterface();

    public String secondFunctionOfMyInterface();

    public String aFunctionOfMyInterface();
}

abstract class AbstractICCE0 implements InterfaceICCE0 {
    abstract public String firstAbstractMethod();
    abstract public String secondAbstractMethod();

    abstract public String anAbstractMethod();
}

class ImplementsSomeInterfaces extends
        AbstractICCE0
    // This interface is missing in the .jasm implementation.
    implements InterfaceICCE1
{

    public String firstAbstractMethod() {
        return this.getClass().getName();
    }

    public String secondAbstractMethod() {
        return this.getClass().getName();
    }

    // This method is missing in the .jasm implementation.
    public String anAbstractMethod() {
        return this.getClass().getName();
    }

    public String firstFunctionOfMyInterface0() {
        return this.getClass().getName();
    }

    public String secondFunctionOfMyInterface0() {
        return this.getClass().getName();
    }

    public String firstFunctionOfMyInterface() {
        return this.getClass().getName();
    }

    public String secondFunctionOfMyInterface() {
        return this.getClass().getName();
    }

    // This method is missing in the .jasm implementation.
    public String aFunctionOfMyInterface() {
        return this.getClass().getName();
    }
}

// Helper classes to test incompatible class change in itable stub.
//
// Class hierachy:
//
//          iA,iB   (interfaces)
//          /|\ \
//         C D E \
//                B (bad class, missing interface implementation)

interface ICC_iA {
    public void a();
}

interface ICC_iB {
    public void b();
}

// This is the errorneous class. A variant of it not
// implementing ICC_iB is copied into the test before
// it is run.
class ICC_B implements ICC_iA,
                       // This interface is missing in the .jasm implementation.
                       ICC_iB {
    public void a() {
        System.out.print("B.a() ");
    }

    public void b() {
        System.out.print("B.b() ");
    }
}

class ICC_C implements ICC_iA, ICC_iB {
    public void a() {
        System.out.print("C.a() ");
    }

    public void b() {
        System.out.print("C.b() ");
    }
}

class ICC_D implements ICC_iA, ICC_iB {
    public void a() {
        System.out.print("D.a() ");
    }

    public void b() {
        System.out.print("D.b() ");
    }
}

class ICC_E implements ICC_iA, ICC_iB {
    public void a() {
        System.out.print("E.a() ");
    }

    public void b() {
        System.out.print("E.b() ");
    }
}
