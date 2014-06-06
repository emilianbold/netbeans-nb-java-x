/*
 * Copyright (c) 2010, 2013, Oracle and/or its affiliates. All rights reserved.
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

/**
 * JDK-8019811: Number coercion for return values of static calls was broken
 *
 * @test
 * @run
 */

function f(x) {
    var window = 17;
    return function (x) {
    return true
    } (x) >> window;
}

Function("L:if((function x ()3)() + arguments++) {return; } else if (new gc()) while(((x2.prop = functional)) && 0){ }");

Function("var x = x -= '' ");

Function("switch((Math.pow ? x = 1.2e3 : 3)) { default: return; }")

Function("x = 0.1, x\ntrue\n~this");

Function("with((function (x)x2)() ^ this){return; }");

