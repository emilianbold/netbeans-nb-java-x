/*
 * Copyright (c) 1999, 2020, Oracle and/or its affiliates. All rights reserved.
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
// Stub class generated by rmic, do not edit.
// Contents subject to change without notice.

package com.sun.jndi.rmi.registry;

/**
 * ReferenceWrapper_Stub.
 */
@SuppressWarnings({"deprecation", "rawtypes", "unchecked"})
public final class ReferenceWrapper_Stub
        extends java.rmi.server.RemoteStub
        implements com.sun.jndi.rmi.registry.RemoteReference, java.rmi.Remote {
    private static final long serialVersionUID = 2;

    private static java.lang.reflect.Method $method_getReference_0;

    static {
        try {
            $method_getReference_0 = com.sun.jndi.rmi.registry.RemoteReference.class.getMethod("getReference", new java.lang.Class[]{});
        } catch (java.lang.NoSuchMethodException e) {
            throw new java.lang.NoSuchMethodError(
                    "stub class initialization failed");
        }
    }

    // constructors
    public ReferenceWrapper_Stub(java.rmi.server.RemoteRef ref) {
        super(ref);
    }

    // methods from remote interfaces

    // implementation of getReference()
    public javax.naming.Reference getReference()
            throws java.rmi.RemoteException, javax.naming.NamingException {
        try {
            Object $result = ref.invoke(this, $method_getReference_0, null, 3529874867989176284L);
            return ((javax.naming.Reference) $result);
        } catch (java.lang.RuntimeException e) {
            throw e;
        } catch (java.rmi.RemoteException e) {
            throw e;
        } catch (javax.naming.NamingException e) {
            throw e;
        } catch (java.lang.Exception e) {
            throw new java.rmi.UnexpectedException("undeclared checked exception", e);
        }
    }
}
