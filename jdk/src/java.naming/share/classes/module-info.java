/*
 * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved.
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
 * Defines the Java Naming and Directory Interface (JNDI) API.
 */
module java.naming {
    requires java.security.sasl;

    exports javax.naming;
    exports javax.naming.directory;
    exports javax.naming.event;
    exports javax.naming.ldap;
    exports javax.naming.spi;
    exports com.sun.jndi.toolkit.ctx to
        jdk.naming.dns;
    exports com.sun.jndi.toolkit.url to
        java.corba,
        jdk.naming.dns,
        jdk.naming.rmi;
    uses javax.naming.ldap.StartTlsResponse;
    uses javax.naming.spi.InitialContextFactory;
    provides java.security.Provider with sun.security.provider.certpath.ldap.JdkLDAP;
}

