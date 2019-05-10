/*
 * Copyright (c) 2011, 2019, Oracle and/or its affiliates. All rights reserved.
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
#include "oops/metadata.hpp"
#include "memory/metaspace.hpp"
#include "memory/resourceArea.hpp"
#include "prims/jvmtiRedefineClasses.hpp"

void Metadata::set_on_stack(const bool value) {
  // nothing to set for most metadata
  // Can't inline because this materializes the vtable on some C++ compilers.
}

void Metadata::print_on(outputStream* st) const {
  ResourceMark rm;
  // print title
  st->print("%s", internal_name());
  print_address_on(st);
  st->cr();
}

void Metadata::print() const { print_on(tty); }
void Metadata::print_value() const { print_value_on(tty); }

char* Metadata::print_value_string() const {
  char buf[256];
  stringStream st(buf, sizeof(buf));
  print_value_on(&st);
  return st.as_string();
}
