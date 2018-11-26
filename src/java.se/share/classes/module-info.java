/*
 * Copyright (c) 2014, 2018, Oracle and/or its affiliates. All rights reserved.
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
 * Defines the API of the Java SE Platform.
 *
 * <dl>
 * <dt class="simpleTagLabel" style="font-family:'DejaVu Sans', Arial, Helvetica, sans serif">Optional for the Java SE Platform:</dt>
 * <dd>
 *   <a href="{@docRoot}/../specs/jni/index.html">Java Native Interface (JNI)</a><br>
 *   <a href="{@docRoot}/../specs/jvmti.html">Java Virtual Machine Tool Interface (JVM TI)</a><br>
 *   <a href="{@docRoot}/../specs/jdwp/jdwp-spec.html">Java Debug Wire Protocol (JDWP)</a><br>
 * </dd>
 * </dl>
 *
 * @moduleGraph
 * @since 9
 */
module java.se {
    requires transitive java.compiler;
    requires transitive java.datatransfer;
    requires transitive java.desktop;
    requires transitive java.instrument;
    requires transitive java.logging;
    requires transitive java.management;
    requires transitive java.management.rmi;
    requires transitive java.naming;
    requires transitive java.net.http;
    requires transitive java.prefs;
    requires transitive java.rmi;
    requires transitive java.scripting;
    requires transitive java.security.jgss;
    requires transitive java.security.sasl;
    requires transitive java.sql;
    requires transitive java.sql.rowset;
    requires transitive java.transaction.xa;
    requires transitive java.xml;
    requires transitive java.xml.crypto;
}
