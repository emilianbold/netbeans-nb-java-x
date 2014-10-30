/*
 * Copyright (c) 2005, 2014, Oracle and/or its affiliates. All rights reserved.
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
 * @bug     6258271
 * @summary DiagnosticMessage exposes internal name __input
 * @author  Peter von der Ah\u00e9
 */

import java.io.*;
import java.util.Arrays;
import javax.tools.*;

public class T6258271 {
    public static void main(String... args) throws IOException {
        JavaCompiler javac = ToolProvider.getSystemJavaCompiler();
        DiagnosticListener<JavaFileObject> dl =  new DiagnosticListener<JavaFileObject>() {
                public void report(Diagnostic<? extends JavaFileObject> message) {
                    JavaFileObject fo = message.getSource();
                    if ("__input".equals(fo.toUri().getPath()))
                        throw new AssertionError(fo);
                    System.out.println(message);
                }
            };
        try (StandardJavaFileManager fm = javac.getStandardFileManager(dl, null, null)) {
            Iterable<? extends JavaFileObject> files =
                fm.getJavaFileObjectsFromStrings(Arrays.asList("nofile.java"));
            javac.getTask(null, fm, dl, null, null, files).call();
        }
    }
}
