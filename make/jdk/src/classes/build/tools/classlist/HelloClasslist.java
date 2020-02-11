/*
 * Copyright (c) 2016, 2019, Oracle and/or its affiliates. All rights reserved.
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

/**
 * This application is meant to be run to create a classlist file representing
 * common use.
 *
 * The classlist is produced by adding -XX:DumpLoadedClassList=classlist
 */
package build.tools.classlist;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.net.InetAddress;
import java.nio.file.FileSystems;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.stream.Stream;
import java.util.logging.*;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.text.DateFormat;

import static java.util.stream.Collectors.*;

/**
 * This class is used to generate a classlist during build. Intent
 * is to touch a reasonable amount of JDK classes that are commonly
 * loaded and used early.
 */
public class HelloClasslist {

    private static final Logger LOGGER = Logger.getLogger("Hello");

    public static void main(String ... args) throws Throwable {

        FileSystems.getDefault();

        List<String> strings = Arrays.asList("Hello", "World!", "From: ",
                InetAddress.getLoopbackAddress().toString());

        String helloWorld = strings.parallelStream()
                .map(s -> s.toLowerCase(Locale.ROOT))
                .collect(joining(","));

        Stream.of(helloWorld.split("([,x-z]{1,3})([\\s]*)"))
                .map(String::toString)
                .forEach(System.out::println);

        // Common concatenation patterns
        String SS     = String.valueOf(args.length) + String.valueOf(args.length);
        String CS     = "string" + String.valueOf(args.length);
        String SC     = String.valueOf(args.length) + "string";
        String SCS    = String.valueOf(args.length) + "string" + String.valueOf(args.length);
        String CSS    = "string" + String.valueOf(args.length) + String.valueOf(args.length);
        String CSC    = "string" + String.valueOf(args.length) + "string";
        String SSC    = String.valueOf(args.length) + String.valueOf(args.length) + "string";
        String CSCS   = "string" + String.valueOf(args.length) + "string" + String.valueOf(args.length);
        String SCSC   = String.valueOf(args.length) + "string" + String.valueOf(args.length) + "string";
        String CSCSC  = "string" + String.valueOf(args.length) + "string" + String.valueOf(args.length) + "string";
        String SCSCS  = String.valueOf(args.length) + "string" + String.valueOf(args.length) + "string" + String.valueOf(args.length);
        String CI     = "string" + args.length;
        String IC     = args.length + "string";
        String SI     = String.valueOf(args.length) + args.length;
        String IS     = args.length + String.valueOf(args.length);
        String CIS    = "string" + args.length + String.valueOf(args.length);
        String CSCI   = "string" + String.valueOf(args.length) + "string" + args.length;
        String CIC    = "string" + args.length + "string";
        String CICI   = "string" + args.length + "string" + args.length;
        String CJ     = "string" + System.currentTimeMillis();
        String JC     = System.currentTimeMillis() + "string";
        String CD     = "string" + (args.length/2.0);
        String CJC    = "string" + System.currentTimeMillis() + "string";
        String CJCJ   = "string" + System.currentTimeMillis() + "string" + System.currentTimeMillis();
        String CJCJC  = "string" + System.currentTimeMillis() + "string" + System.currentTimeMillis() + "string";

        String newDate = DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(
                LocalDateTime.now(ZoneId.of("GMT")));

        String oldDate = String.format("%s%n",
                DateFormat.getDateInstance(DateFormat.DEFAULT, Locale.ROOT)
                        .format(new Date()));

        // A selection of trivial and relatively common MH operations
        invoke(MethodHandles.identity(double.class), 1.0);
        invoke(MethodHandles.identity(int.class), 1);
        invoke(MethodHandles.identity(String.class), "x");

        invoke(handle("staticMethod_V", MethodType.methodType(void.class)));

        LOGGER.log(Level.FINE, "New Date: " + newDate + " - old: " + oldDate);
    }

    public static void staticMethod_V() {}

    private static MethodHandle handle(String name, MethodType type) throws Throwable {
        return MethodHandles.lookup().findStatic(HelloClasslist.class, name, type);
    }

    private static Object invoke(MethodHandle mh, Object ... args) throws Throwable {
        try {
            for (Object o : args) {
                mh = MethodHandles.insertArguments(mh, 0, o);
            }
            return mh.invoke();
        } catch (Throwable t) {
            LOGGER.warning("Failed to find, link and/or invoke " + mh.toString() + ": " + t.getMessage());
            throw t;
        }
    }
}
