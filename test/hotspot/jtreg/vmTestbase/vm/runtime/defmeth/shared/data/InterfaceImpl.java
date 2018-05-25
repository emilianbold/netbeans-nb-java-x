/*
 * Copyright (c) 2013, 2018, Oracle and/or its affiliates. All rights reserved.
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

package vm.runtime.defmeth.shared.data;

import vm.runtime.defmeth.shared.data.method.Method;

/**
 * Represents Java interface class.
 */
public class InterfaceImpl extends ClazzImpl implements Interface {
    Interface[] parents; // superinterfaces

    public InterfaceImpl(int flags, String name, int ver, String sig, Interface[] parents, Method... methods) {
        super(name, ver, flags, sig, methods);
        this.parents = parents;
    }

    public Interface[] parents() {
        return parents;
    }

    @Override
    public void visit(Visitor v) {
        v.visitInterface(this);
    }
}
