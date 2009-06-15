/*
 * Copyright 2009 Sun Microsystems, Inc.  All Rights Reserved.
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
 * Please contact Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * CA 95054 USA or visit www.sun.com if you need additional information or
 * have any questions.
 */

/**
 * @test
 * @bug 6823354
 * @summary These methods can be instrinsified by using bit scan, bit test, and population count instructions.
 *
 * @run main/othervm -Xcomp -XX:CompileOnly=Test6823354.lzcomp,Test6823354.tzcomp,.dolzcomp,.dotzcomp Test6823354
 */

import java.net.URLClassLoader;

public class Test6823354 {
    // Arrays of corner case values.
    static final int[]  ia = new int[]  { 0,  1,  -1,  Integer.MIN_VALUE, Integer.MAX_VALUE };
    static final long[] la = new long[] { 0L, 1L, -1L, Long.MIN_VALUE,    Long.MAX_VALUE    };

    public static void main(String[] args) throws Exception {
        // Load the classes and the methods.
        Integer.numberOfLeadingZeros(0);
        Integer.numberOfTrailingZeros(0);
        Long.numberOfLeadingZeros(0);
        Long.numberOfTrailingZeros(0);

        lz();
        tz();
    }

    static void lz() throws Exception {
        // int

        // Test corner cases.
        for (int i = 0; i < ia.length; i++) {
            int x = ia[i];
            check(x, lzcomp(x), lzint(x));
        }

        // Test all possible return values.
        for (int i = 0; i < Integer.SIZE; i++) {
            int x = 1 << i;
            check(x, lzcomp(x), lzint(x));
        }

        String classname = "Test6823354$lzconI";

        // Test Ideal optimizations (constant values).
        for (int i = 0; i < ia.length; i++) {
            testclass(classname, ia[i]);
        }

        // Test Ideal optimizations (constant values).
        for (int i = 0; i < Integer.SIZE; i++) {
            int x = 1 << i;
            testclass(classname, x);
        }


        // long

        // Test corner cases.
        for (int i = 0; i < ia.length; i++) {
            long x = la[i];
            check(x, lzcomp(x), lzint(x));
        }

        // Test all possible return values.
        for (int i = 0; i < Long.SIZE; i++) {
            long x = 1L << i;
            check(x, lzcomp(x), lzint(x));
        }

        classname = "Test6823354$lzconL";

        // Test Ideal optimizations (constant values).
        for (int i = 0; i < la.length; i++) {
            testclass(classname, la[i]);
        }

        // Test Ideal optimizations (constant values).
        for (int i = 0; i < Long.SIZE; i++) {
            long x = 1L << i;
            testclass(classname, x);
        }
    }

    static void tz() throws Exception {
        // int

        // Test corner cases.
        for (int i = 0; i < ia.length; i++) {
            int x = ia[i];
            check(x, tzcomp(x), tzint(x));
        }

        // Test all possible return values.
        for (int i = 0; i < Integer.SIZE; i++) {
            int x = 1 << i;
            check(x, tzcomp(x), tzint(x));
        }

        String classname = "Test6823354$tzconI";

        // Test Ideal optimizations (constant values).
        for (int i = 0; i < ia.length; i++) {
            testclass(classname, ia[i]);
        }

        // Test Ideal optimizations (constant values).
        for (int i = 0; i < Integer.SIZE; i++) {
            int x = 1 << i;
            testclass(classname, x);
        }


        // long

        // Test corner cases.
        for (int i = 0; i < la.length; i++) {
            long x = la[i];
            check(x, tzcomp(x), tzint(x));
        }

        // Test all possible return values.
        for (int i = 0; i < Long.SIZE; i++) {
            long x = 1L << i;
            check(x, tzcomp(x), tzint(x));
        }

        classname = "Test6823354$tzconL";

        // Test Ideal optimizations (constant values).
        for (int i = 0; i < la.length; i++) {
            testclass(classname, la[i]);
        }

        // Test Ideal optimizations (constant values).
        for (int i = 0; i < Long.SIZE; i++) {
            long x = 1L << i;
            testclass(classname, x);
        }
    }

    static void check(int value, int result, int expected) {
        //System.out.println(value + ": " + result + ", " + expected);
        if (result != expected)
            throw new InternalError(value + " failed: " + result + " != " + expected);
    }

    static void check(long value, long result, long expected) {
        //System.out.println(value + ": " + result + ", " + expected);
        if (result != expected)
            throw new InternalError(value + " failed: " + result + " != " + expected);
    }

    static int lzint( int i)  { return Integer.numberOfLeadingZeros(i); }
    static int lzcomp(int i)  { return Integer.numberOfLeadingZeros(i); }

    static int lzint( long l) { return Long.numberOfLeadingZeros(l); }
    static int lzcomp(long l) { return Long.numberOfLeadingZeros(l); }

    static int tzint( int i)  { return Integer.numberOfTrailingZeros(i); }
    static int tzcomp(int i)  { return Integer.numberOfTrailingZeros(i); }

    static int tzint( long l) { return Long.numberOfTrailingZeros(l); }
    static int tzcomp(long l) { return Long.numberOfTrailingZeros(l); }

    static void testclass(String classname, int x) throws Exception {
        System.setProperty("value", "" + x);
        loadandrunclass(classname);
    }

    static void testclass(String classname, long x) throws Exception {
        System.setProperty("value", "" + x);
        loadandrunclass(classname);
    }

    static void loadandrunclass(String classname) throws Exception {
        Class cl = Class.forName(classname);
        URLClassLoader apploader = (URLClassLoader) cl.getClassLoader();
        ClassLoader loader = new URLClassLoader(apploader.getURLs(), apploader.getParent());
        Class c = loader.loadClass(classname);
        Runnable r = (Runnable) c.newInstance();
        r.run();
    }

    public static class lzconI implements Runnable {
        static final int VALUE;

        static {
            int value = 0;
            try {
                value = Integer.decode(System.getProperty("value"));
            } catch (Throwable e) {}
            VALUE = value;
        }

        public void run() { check(VALUE, lzint(VALUE), dolzcomp()); }
        static int dolzcomp() { return lzcomp(VALUE); }
    }

    public static class lzconL implements Runnable {
        static final long VALUE;

        static {
            long value = 0;
            try {
                value = Long.decode(System.getProperty("value"));
            } catch (Throwable e) {}
            VALUE = value;
        }

        public void run() { check(VALUE, lzint(VALUE), dolzcomp()); }
        static int dolzcomp() { return lzcomp(VALUE); }
    }

    public static class tzconI implements Runnable {
        static final int VALUE;

        static {
            int value = 0;
            try {
                value = Integer.decode(System.getProperty("value"));
            } catch (Throwable e) {}
            VALUE = value;
        }

        public void run() { check(VALUE, tzint(VALUE), dotzcomp()); }
        static int dotzcomp() { return tzcomp(VALUE); }
    }

    public static class tzconL implements Runnable {
        static final long VALUE;

        static {
            long value = 0;
            try {
                value = Long.decode(System.getProperty("value"));
            } catch (Throwable e) {}
            VALUE = value;
        }

        public void run() { check(VALUE, tzint(VALUE), dotzcomp()); }
        static int dotzcomp() { return tzcomp(VALUE); }
    }
}
