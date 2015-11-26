/*
 * Copyright (c) 2005, 2015, Oracle and/or its affiliates. All rights reserved.
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

package jdk.test.lib.jittester;
// an utility class to limit steps in the production of an expression
public class ProductionLimiter {

    private static Integer limit = -1;

    public static void setUnlimited() {
        limit = -1;
    }

    // initialize limit state
    public static void setLimit() {
        limit = ProductionParams.productionLimit.value();
    }

    // iterate a limit, throwing exception in case it hit
    public static void limitProduction() throws ProductionFailedException {
        if (limit > 0) {
            limit--;
        }
        if (limit != -1 && limit <= 0) {
            throw new ProductionFailedException();
        }
    }
}
