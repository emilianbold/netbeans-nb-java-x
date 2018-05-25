/*
 * Copyright (c) 2003, 2018, Oracle and/or its affiliates. All rights reserved.
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

package nsk.jvmti.PopFrame;

import java.io.PrintStream;

public class popframe009 {

    final static int PASSED = 0;
    final static int FAILED = 2;
    final static int JCK_STATUS_BASE = 95;
    final static int FIBONACCI_INDEX = 32;

    static {
        try {
            System.loadLibrary("popframe009");
        } catch (UnsatisfiedLinkError ule) {
            System.err.println("Could not load popframe009 library");
            System.err.println("java.library.path:"
                + System.getProperty("java.library.path"));
            throw ule;
        }
    }

    native static void getReady();
    native static int check(Thread thread);

    public static Object lock = new Object();
    public static int fibonacciNumber = -1;

    public static void main(String args[]) {
        args = nsk.share.jvmti.JVMTITest.commonInit(args);

        // produce JCK-like exit status.
        System.exit(run(args, System.out) + JCK_STATUS_BASE);
    }

    public static int run(String args[], PrintStream out) {
        TestThread thr = new TestThread();
        getReady();

        synchronized (lock) {
            thr.start();
            try {
                lock.wait();
            } catch (InterruptedException e) {
                throw new Error("Unexpected " + e);
            }
        }

        int res = check(thr);

        try {
            thr.join();
        } catch (InterruptedException e) {
            throw new Error("Unexpected " + e);
        }

        int expected = fibonacci(FIBONACCI_INDEX);
        if (expected != fibonacciNumber) {
           out.print("fibonacci(" + FIBONACCI_INDEX + "):");
           out.print(" expected = " + expected);
           out.println(", actual = " + fibonacciNumber);
           res = FAILED;
        }

        return res;
    }

    // dummy method to be breakpointed in agent
    static void checkPoint() {
    }

    static int fibonacci(int n) {
        if (n <= 2) {
            return 1;
        } else {
            return fibonacci(n-1) + fibonacci(n-2);
        }
    }

    static class TestThread extends Thread {
        public void run() {
            synchronized (lock) {
                lock.notify();
            }
            checkPoint();
            fibonacciNumber = fibonacci(FIBONACCI_INDEX);
            checkPoint();
        }
    }
}
