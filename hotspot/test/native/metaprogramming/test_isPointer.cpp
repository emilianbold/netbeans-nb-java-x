/*
 * Copyright (c) 2017, Oracle and/or its affiliates. All rights reserved.
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
 *
 */

#include "precompiled.hpp"
#include "memory/allocation.hpp"
#include "metaprogramming/isPointer.hpp"
#include "utilities/debug.hpp"

class IsPointerTest: AllStatic {
  class A: AllStatic {};

  static const bool ip_voidptr = IsPointer<void*>::value;
  STATIC_ASSERT(ip_voidptr);

  static const bool ip_Aptr = IsPointer<A*>::value;
  STATIC_ASSERT(ip_Aptr);

  static const bool ip_cAptr = IsPointer<const A*>::value;
  STATIC_ASSERT(ip_cAptr);

  static const bool ip_vAptr = IsPointer<volatile A*>::value;
  STATIC_ASSERT(ip_vAptr);

  static const bool ip_Avptr = IsPointer<A* volatile>::value;
  STATIC_ASSERT(ip_Avptr);

  static const bool ip_intptrt = IsPointer<intptr_t>::value;
  STATIC_ASSERT(!ip_intptrt);

  static const bool ip_char = IsPointer<char>::value;
  STATIC_ASSERT(!ip_char);
};
