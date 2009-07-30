/*
 * Copyright 2007-2008 Sun Microsystems, Inc.  All Rights Reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the LICENSE file that accompanied this code.
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

package com.sun.tools.classfile;

import java.io.IOException;

/*
 *  <p><b>This is NOT part of any API supported by Sun Microsystems.  If
 *  you write code that depends on this, you do so at your own risk.
 *  This code and its internal interfaces are subject to change or
 *  deletion without notice.</b>
 */
public class Method {
    Method(ClassReader cr) throws IOException {
        access_flags = new AccessFlags(cr);
        name_index = cr.readUnsignedShort();
        descriptor = new Descriptor(cr);
        attributes = new Attributes(cr);
    }

    public Method(AccessFlags access_flags,
            int name_index, Descriptor descriptor,
            Attributes attributes) {
        this.access_flags = access_flags;
        this.name_index = name_index;
        this.descriptor = descriptor;
        this.attributes = attributes;
    }

    public int byteLength() {
        return 6 + attributes.byteLength();
    }

    public String getName(ConstantPool constant_pool) throws ConstantPoolException {
        return constant_pool.getUTF8Value(name_index);
    }

    public final AccessFlags access_flags;
    public final int name_index;
    public final Descriptor descriptor;
    public final Attributes attributes;
}
