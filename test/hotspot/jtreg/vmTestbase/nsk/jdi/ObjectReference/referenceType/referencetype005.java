/*
 * Copyright (c) 2001, 2019, Oracle and/or its affiliates. All rights reserved.
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

package nsk.jdi.ObjectReference.referenceType;

import nsk.share.*;
import nsk.share.jpda.*;
import nsk.share.jdi.*;

import com.sun.jdi.*;
import java.util.*;
import java.io.*;

/**
 * The test for the implementation of an object of the type     <BR>
 * ObjectReference.                                             <BR>
 *                                                              <BR>
 * The test checks up that results of the method                <BR>
 * <code>com.sun.jdi.ObjectReference.referenceType()</code>     <BR>
 * complies with its spec when ObjectReference is ArrayReference<BR>
 * of ClassType.                                                <BR>
 * <BR>
 * The test consequently checks up the following assersions:    <BR>
 * - the type may be a subclass or implementor of the declared type<BR>
 *   of any field or variable which currently holds it;         <BR>
 * - the returned ReferenceType will be a ClassType or ArrayType<BR>
 *   and never an InterfaceType;                                <BR>
 * - the type of an object never changes, so this method will   <BR>
 *   always return the same ReferenceType over the lifetime     <BR>
 *   of the mirrored object.                                    <BR>
 */

public class referencetype005 {

    //----------------------------------------------------- templete section
    static final int PASSED = 0;
    static final int FAILED = 2;
    static final int PASS_BASE = 95;

    //----------------------------------------------------- templete parameters
    static final String
    sHeader1 = "\n==> nsk/jdi/ObjectReference/referenceType/referencetype005  ",
    sHeader2 = "--> debugger: ",
    sHeader3 = "##> debugger: ";

    //----------------------------------------------------- main method

    public static void main (String argv[]) {
        int result = run(argv, System.out);
        System.exit(result + PASS_BASE);
    }

    public static int run (String argv[], PrintStream out) {
        return new referencetype005().runThis(argv, out);
    }

    //--------------------------------------------------   log procedures

    private static Log  logHandler;

    private static void log1(String message) {
        logHandler.display(sHeader1 + message);
    }
    private static void log2(String message) {
        logHandler.display(sHeader2 + message);
    }
    private static void log3(String message) {
        logHandler.complain(sHeader3 + message);
    }

    //  ************************************************    test parameters

    private String debuggeeName =
        "nsk.jdi.ObjectReference.referenceType.referencetype005a";

    private String testedClassName =
        "nsk.jdi.ObjectReference.referenceType.referencetype005aClassForCheck[]";

    String mName = "nsk.jdi.ObjectReference.referenceType";

    //====================================================== test program
    //------------------------------------------------------ common section

    static ArgumentHandler      argsHandler;

    static int waitTime;

    static VirtualMachine  vm   = null;

    ReferenceType     testedClass  = null;

    static int  testExitCode = PASSED;

    static final int returnCode0 = 0;
    static final int returnCode1 = 1;
    static final int returnCode2 = 2;
    static final int returnCode3 = 3;
    static final int returnCode4 = 4;

    //------------------------------------------------------ methods

    private int runThis (String argv[], PrintStream out) {

        Debugee debuggee;

        argsHandler     = new ArgumentHandler(argv);
        logHandler      = new Log(out, argsHandler);
        Binder binder   = new Binder(argsHandler, logHandler);

        if (argsHandler.verbose()) {
            debuggee = binder.bindToDebugee(debuggeeName + " -vbs");
        } else {
            debuggee = binder.bindToDebugee(debuggeeName);
        }

        waitTime = argsHandler.getWaitTime();


        IOPipe pipe     = new IOPipe(debuggee);

        debuggee.redirectStderr(out);
        log2(debuggeeName + " debuggee launched");
        debuggee.resume();

        String line = pipe.readln();
        if ((line == null) || !line.equals("ready")) {
            log3("signal received is not 'ready' but: " + line);
            return FAILED;
        } else {
            log2("'ready' recieved");
        }

        vm = debuggee.VM();

    //------------------------------------------------------  testing section
        log1("      TESTING BEGINS");

        for (int i = 0; ; i++) {

            pipe.println("newcheck");
            line = pipe.readln();

            if (line.equals("checkend")) {
                log2("     : returned string is 'checkend'");
                break ;
            } else if (!line.equals("checkready")) {
                log3("ERROR: returned string is not 'checkready'");
                testExitCode = FAILED;
                break ;
            }

            log1("new checkready: #" + i);

            //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ variable part

            List listOftestedClasses = vm.classesByName(mName + ".referencetype005aTestClass");
            if (listOftestedClasses.size() != 1) {
                log3("ERROR: listOftestedClasses.size() != 1");
                testExitCode = FAILED;
                continue;
            }
            ReferenceType testedClass =
                        (ReferenceType) listOftestedClasses.get(0);

            Field fieldArray = testedClass.fieldByName("cfc");
            if (fieldArray == null) {
                log3("ERROR: fieldArray == null");
                testExitCode = FAILED;
                continue;
            }

            ArrayReference arrayRef = (ArrayReference) testedClass.getValue(fieldArray);

            log2("......ReferenceType testedClass = arrayRef.referenceType();");
            testedClass = arrayRef.referenceType();

            log2("...... check up on equality testedClass.name() to String testedClassName");
            if (!testedClass.name().equals(testedClassName)) {
                log3("ERROR: check that the type may be a subclass or implementor of the declared type FAILED");
                log3("ERROR: String testedClassName : " + testedClassName);
                log3("ERROR: performing statement : !testedClass.name().equals(testedClassName)");
                log3("ERROR: testedClass.name() : " + testedClass.name());
                testExitCode = FAILED;
            }

            try {
                log2(".......check up on cast to ArrayType; no Exception expected");
                ArrayType ct = (ArrayType) testedClass;
            } catch ( Exception e ) {
                log3("ERROR: check that ReferenceType will be a ArrayType FAILED");
                log3("ERROR: ReferenceType testedClass is of the type : " + testedClass);
                log3("ERROR: performing statement: ArrayType ct = (ArrayType) testedClass;");
                log3("ERROR: Exception : " + e);
                testExitCode = FAILED;
            }


            log2(".......check up on equality ReferenceType arrayRef.referenceType() to testedClass");
            ReferenceType testedClass1 = arrayRef.referenceType();
            if (!testedClass1.equals(testedClass)) {
                log3("ERROR: check that the method always return the same RefereneceType object FAILED");
                log3("ERROR: ReferenceType testedClass is of the type : " + testedClass);
                log3("ERROR: performing statement: !arrayRef.referenceType().equals(testedClass)");
                log3("ERROR: returned ReferenceType : " + testedClass1);
                testExitCode = FAILED;
            }

            //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        }
        log1("      TESTING ENDS");

    //--------------------------------------------------   test summary section
    //-------------------------------------------------    standard end section

        pipe.println("quit");
        log2("waiting for the debuggee to finish ...");
        debuggee.waitFor();

        int status = debuggee.getStatus();
        if (status != PASSED + PASS_BASE) {
            log3("debuggee returned UNEXPECTED exit status: " +
                    status + " != PASS_BASE");
            testExitCode = FAILED;
        } else {
            log2("debuggee returned expected exit status: " +
                    status + " == PASS_BASE");
        }

        if (testExitCode != PASSED) {
            logHandler.complain("TEST FAILED");
        }
        return testExitCode;
    }
}
