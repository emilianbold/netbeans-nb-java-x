/*
 * Copyright (c) 2010, 2015 Oracle and/or its affiliates. All rights reserved.
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

#ifndef SHARE_VM_GC_IMPLEMENTATION_PARALLELSCAVENGE_PSCOMPACTIONMANAGER_INLINE_HPP
#define SHARE_VM_GC_IMPLEMENTATION_PARALLELSCAVENGE_PSCOMPACTIONMANAGER_INLINE_HPP

#include "gc_implementation/parallelScavenge/psCompactionManager.hpp"
#include "gc_implementation/parallelScavenge/psParallelCompact.hpp"
#include "oops/objArrayKlass.inline.hpp"
#include "oops/oop.pcgc.inline.hpp"

void ParCompactionManager::push_objarray(oop obj, size_t index)
{
  ObjArrayTask task(obj, index);
  assert(task.is_valid(), "bad ObjArrayTask");
  _objarray_stack.push(task);
}

void ParCompactionManager::push_region(size_t index)
{
#ifdef ASSERT
  const ParallelCompactData& sd = PSParallelCompact::summary_data();
  ParallelCompactData::RegionData* const region_ptr = sd.region(index);
  assert(region_ptr->claimed(), "must be claimed");
  assert(region_ptr->_pushed++ == 0, "should only be pushed once");
#endif
  region_stack()->push(index);
}

inline void ParCompactionManager::follow_contents(oop obj) {
  obj->follow_contents(this);
}

inline void ParCompactionManager::follow_contents(objArrayOop obj, int index) {
  ObjArrayKlass* k = (ObjArrayKlass*)obj->klass();
  k->oop_follow_contents(this, obj, index);
}

inline void ParCompactionManager::update_contents(oop obj) {
  obj->update_contents(this);
}

#endif // SHARE_VM_GC_IMPLEMENTATION_PARALLELSCAVENGE_PSCOMPACTIONMANAGER_INLINE_HPP
