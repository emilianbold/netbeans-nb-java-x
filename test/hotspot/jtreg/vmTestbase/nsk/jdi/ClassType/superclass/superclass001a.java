/*
 * Copyright (c) 2001, 2018, Oracle and/or its affiliates. All rights reserved.
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

package nsk.jdi.ClassType.superclass;

import nsk.share.*;
import nsk.share.jpda.*;
import nsk.share.jdi.*;


/**
 * This class is used as debuggee application for the superclass001 JDI test.
 */

public class superclass001a {

    //----------------------------------------------------- templete section

    static final int PASSED = 0;
    static final int FAILED = 2;
    static final int PASS_BASE = 95;

    //--------------------------------------------------   log procedures

    static boolean verbMode = false;  // debugger may switch to true

    private static void log1(String message) {
        if (verbMode)
            System.err.println("**> superclass001a: " + message);
    }

    private static void logErr(String message) {
        if (verbMode)
            System.err.println("!!**> superclass001a: " + message);
    }

    //====================================================== test program
    //----------------------------------------------------   main method

    public static void main (String argv[]) {

        for (int i=0; i<argv.length; i++) {
            if ( argv[i].equals("-vbs") || argv[i].equals("-verbose") ) {
                verbMode = true;
                break;
            }
        }
        log1("debuggee started!");

        // informing a debugger of readyness
        ArgumentHandler argHandler = new ArgumentHandler(argv);
        IOPipe pipe = argHandler.createDebugeeIOPipe();
        pipe.println("ready");


        int exitCode = PASSED;
        for (int i = 0; ; i++) {

            String instruction;

            log1("waiting for an instruction from the debugger ...");
            instruction = pipe.readln();
            if (instruction.equals("quit")) {
                log1("'quit' recieved");
                break ;

            } else if (instruction.equals("newcheck")) {
                switch (i) {

    //------------------------------------------------------  section tested

                case 0:
                        Class1ForCheck class1 = new Class1ForCheck();
                        Class2ForCheck class2 = new Class2ForCheck();
                        Class3ForCheck class3 = new Class3ForCheck();

                        pipe.println("checkready");
                        break ;

    //-------------------------------------------------    standard end section

                default:
                                pipe.println("checkend");
                                break ;
                }

            } else {
                logErr("ERRROR: unexpected instruction: " + instruction);
                exitCode = FAILED;
                break ;
            }
        }

        System.exit(exitCode + PASS_BASE);
    }
}

class Class1ForCheck {

    int method1() {
        return 1;
    }

    // static fields

    static boolean   s_boolean;
    static byte      s_byte;
    static char      s_char;
    static double    s_double;
    static float     s_float;
    static int       s_int;
    static long      s_long;
    static Object    s_object;
    static long[]    s_prim_array;
    static Object[]  s_ref_array;

    // instance fields

    boolean  i_boolean;
    byte     i_byte;
    char     i_char;
    double   i_double;
    float    i_float;
    int      i_int;
    long     i_long;
    Object   i_object;
    long[]   i_prim_array;
    Object[] i_ref_array;
}

class Class2ForCheck extends Class1ForCheck {

    int method2() {
        return 1;
    }

    // static fields

    static boolean   s_boolean;
    static byte      s_byte;
    static char      s_char;
    static double    s_double;
    static float     s_float;
    static int       s_int;
    static long      s_long;
    static Object    s_object;
    static long[]    s_prim_array;
    static Object[]  s_ref_array;

    // instance fields

    boolean  i_boolean;
    byte     i_byte;
    char     i_char;
    double   i_double;
    float    i_float;
    int      i_int;
    long     i_long;
    Object   i_object;
    long[]   i_prim_array;
    Object[] i_ref_array;
}

class Class3ForCheck extends Class2ForCheck {

    int method3() {
        return 1;
    }

    // static fields

    static boolean   s_boolean;
    static byte      s_byte;
    static char      s_char;
    static double    s_double;
    static float     s_float;
    static int       s_int;
    static long      s_long;
    static Object    s_object;
    static long[]    s_prim_array;
    static Object[]  s_ref_array;

    // instance fields

    boolean  i_boolean;
    byte     i_byte;
    char     i_char;
    double   i_double;
    float    i_float;
    int      i_int;
    long     i_long;
    Object   i_object;
    long[]   i_prim_array;
    Object[] i_ref_array;
}
