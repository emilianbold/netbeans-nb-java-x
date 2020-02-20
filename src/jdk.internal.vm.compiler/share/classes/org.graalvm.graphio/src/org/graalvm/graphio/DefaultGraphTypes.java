/*
 * Copyright (c) 2011, 2020, Oracle and/or its affiliates. All rights reserved.
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


package org.graalvm.graphio;

final class DefaultGraphTypes implements GraphTypes {
    static final GraphTypes DEFAULT = new DefaultGraphTypes();

    private DefaultGraphTypes() {
    }

    @SuppressWarnings("unchecked")
    @Override
    public Class<?> enumClass(Object enumValue) {
        if (enumValue instanceof Enum<?>) {
            // check that the enum class is not actually an anonymous subclass:
            Class<? extends Enum<?>> enumClass = (Class<? extends Enum<?>>) enumValue.getClass();
            Enum<?>[] constants = enumClass.getEnumConstants();
            if (constants == null && enumClass.isAnonymousClass()) {
                enumClass = (Class<? extends Enum<?>>) enumClass.getSuperclass();
            }
            return enumClass;
        }
        return null;
    }

    @Override
    public int enumOrdinal(Object obj) {
        if (obj instanceof Enum<?>) {
            return ((Enum<?>) obj).ordinal();
        }
        return -1;
    }

    @SuppressWarnings("unchecked")
    @Override
    public String[] enumTypeValues(Object clazz) {
        if (clazz instanceof Class<?>) {
            Class<? extends Enum<?>> enumClass = (Class<? extends Enum<?>>) clazz;
            Enum<?>[] constants = enumClass.getEnumConstants();
            if (constants != null) {
                String[] names = new String[constants.length];
                for (int i = 0; i < constants.length; i++) {
                    names[i] = constants[i].name();
                }
                return names;
            }
        }
        return null;
    }

    @Override
    public String typeName(Object clazz) {
        if (clazz instanceof Class<?>) {
            return ((Class<?>) clazz).getName();
        }
        return null;
    }

}
