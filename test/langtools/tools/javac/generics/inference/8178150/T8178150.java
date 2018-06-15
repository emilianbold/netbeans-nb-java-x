/*
 * Copyright (c) 2018, Oracle and/or its affiliates. All rights reserved.
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

/*
 * @test
 * @bug 8178150
 * @summary Regression in logic for handling inference stuck constraints
 * @compile T8178150.java
 */

import java.util.*;
import java.util.function.*;
import java.util.logging.*;

class T8178150 {

    public static void test(List<List<String>> testList, Logger LOGGER) {
        testList.forEach(T8178150.bind(cast(LOGGER::info), iterable -> ""));
        testList.forEach(T8178150.bind_transitive(cast_transitive(LOGGER::info), iterable -> ""));
    }

    private static <T1, T2> TestProcedure<T1, T2> bind(Consumer<T2> delegate, Function<? super T1, T2> function) {
        return null;
    }

    private static <C> Consumer<C> cast(Consumer<C> consumer) {
        return consumer;
    }

    private static <T1, T2, U extends T2> TestProcedure<T1, T2> bind_transitive(Consumer<U> delegate, Function<? super T1, T2> function) {
        return null;
    }

    private static <C> C cast_transitive(C consumer) {
        return consumer;
    }

    private static final class TestProcedure<X1, X2> implements Consumer<X1> {
        @Override
        public void accept(final X1 t1) { }
    }
}
