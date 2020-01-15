/*
 * Copyright (c) 2019, Oracle and/or its affiliates. All rights reserved.
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 *  This code is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License version 2 only, as
 *  published by the Free Software Foundation.
 *
 *  This code is distributed in the hope that it will be useful, but WITHOUT
 *  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 *  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  version 2 for more details (a copy is included in the LICENSE file that
 *  accompanied this code).
 *
 *  You should have received a copy of the GNU General Public License version
 *  2 along with this work; if not, write to the Free Software Foundation,
 *  Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 *   Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 *  or visit www.oracle.com if you need additional information or have any
 *  questions.
 *
 */

/*
 * @test
 * @run testng TestTypeAccess
 */

import jdk.incubator.foreign.MemorySegment;
import jdk.incubator.foreign.MemoryLayouts;
import org.testng.annotations.*;

import java.lang.invoke.VarHandle;
import java.lang.invoke.WrongMethodTypeException;

public class TestTypeAccess {

    static final VarHandle INT_HANDLE = MemoryLayouts.JAVA_INT.varHandle(int.class);

    @Test(expectedExceptions=ClassCastException.class)
    public void testMemoryAddressCoordinateAsString() {
        try (MemorySegment s = MemorySegment.allocateNative(8)) {
            int v = (int)INT_HANDLE.get("string");
        }
    }

    @Test(expectedExceptions=WrongMethodTypeException.class)
    public void testMemoryCoordinatePrimitive() {
        try (MemorySegment s = MemorySegment.allocateNative(8)) {
            int v = (int)INT_HANDLE.get(1);
        }
    }
}
