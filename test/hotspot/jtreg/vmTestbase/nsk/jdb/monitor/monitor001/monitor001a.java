/*
 * Copyright (c) 2002, 2018, Oracle and/or its affiliates. All rights reserved.
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

package nsk.jdb.monitor.monitor001;

import nsk.share.*;
import nsk.share.jpda.*;
import nsk.share.jdb.*;

import java.io.*;

//    THIS TEST IS LINE NUMBER SENSITIVE

/* This is debuggee aplication */
public class monitor001a {
    static monitor001a _monitor001a = new monitor001a();

    public static void main(String args[]) {
        System.exit(monitor001.JCK_STATUS_BASE + _monitor001a.runIt(args, System.out));
    }

    static void lastBreak () {}

    public int runIt(String args[], PrintStream out) {
        JdbArgumentHandler argumentHandler = new JdbArgumentHandler(args);
        Log log = new Log(out, argumentHandler);
        int localInt = 0; // monitor001.LINE_NUMBER
        localInt++; // dummy breakpoint
        log.display("Debuggee PASSED");
        return monitor001.PASSED;
    }
}
