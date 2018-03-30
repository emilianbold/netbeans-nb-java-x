/*
 * Copyright (c) 2018, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_AARCH64_GC_SHARED_MODREFBARRIERSETASSEMBLER_AARCH64_HPP
#define CPU_AARCH64_GC_SHARED_MODREFBARRIERSETASSEMBLER_AARCH64_HPP

#include "asm/macroAssembler.hpp"
#include "gc/shared/barrierSetAssembler.hpp"

class ModRefBarrierSetAssembler: public BarrierSetAssembler {
protected:
  virtual void gen_write_ref_array_pre_barrier(MacroAssembler* masm, DecoratorSet decorators,
                                               Register addr, Register count, RegSet saved_regs) {}
  virtual void gen_write_ref_array_post_barrier(MacroAssembler* masm, DecoratorSet decorators,
                                                Register start, Register end, Register tmp, RegSet saved_regs) {}

public:
  virtual void arraycopy_prologue(MacroAssembler* masm, DecoratorSet decorators, bool is_oop,
                                  Register addr, Register count, RegSet saved_regs);
  virtual void arraycopy_epilogue(MacroAssembler* masm, DecoratorSet decorators, bool is_oop,
                                  Register start, Register end, Register tmp, RegSet saved_regs);
};

#endif // CPU_AARCH64_GC_SHARED_MODREFBARRIERSETASSEMBLER_AARCH64_HPP
