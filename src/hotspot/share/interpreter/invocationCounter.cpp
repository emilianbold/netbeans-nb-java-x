/*
 * Copyright (c) 1997, 2019, Oracle and/or its affiliates. All rights reserved.
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
#include "interpreter/invocationCounter.hpp"

void InvocationCounter::init() {
  _counter = 0;  // reset all the bits, including the sticky carry
}

void InvocationCounter::set(uint count, uint flag) {
  _counter = (count << number_of_noncount_bits) | (flag & carry_mask);
}

void InvocationCounter::set(uint count) {
  uint carry = (_counter & carry_mask);    // the carry bit is sticky
  _counter = (count << number_of_noncount_bits) | carry;
}

void InvocationCounter::update(uint new_count) {
  // Don't make the method look like it's never been executed
  uint counter = raw_counter();
  uint c = extract_count(counter);
  uint f = extract_carry(counter);
  // prevent from going to zero, to distinguish from never-executed methods
  if (c > 0 && new_count == 0) new_count = 1;
  set(new_count, f);
}

void InvocationCounter::set_carry_and_reduce() {
  uint counter = raw_counter();
  // The carry bit now indicates that this counter had achieved a very
  // large value.  Now reduce the value, so that the method can be
  // executed many more times before re-entering the VM.
  uint old_count = extract_count(counter);
  uint new_count = MIN2(old_count, (uint)(CompileThreshold / 2));
  // prevent from going to zero, to distinguish from never-executed methods
  if (new_count == 0)  new_count = 1;
  if (old_count != new_count)  set(new_count, carry_mask);
}

void InvocationCounter::set_carry_on_overflow() {
  if (!carry() && count() > InvocationCounter::count_limit / 2) {
    set_carry();
  }
}

void InvocationCounter::reset() {
  update(0);
}

void InvocationCounter::decay() {
  update(count() >> 1);
}

void InvocationCounter::print() {
  uint counter = raw_counter();
  tty->print_cr("invocation count: up = %d, limit = %d, carry = %s",
                                   extract_count(counter), limit(),
                                   extract_carry(counter) ? "true" : "false");
}

#ifdef CC_INTERP
int                       InvocationCounter::InterpreterInvocationLimit;
int                       InvocationCounter::InterpreterBackwardBranchLimit;
#endif

void invocationCounter_init() {
#ifdef CC_INTERP
  InterpreterInvocationLimit = CompileThreshold << number_of_noncount_bits;

  // When methodData is collected, the backward branch limit is compared against a
  // methodData counter, rather than an InvocationCounter.  In the former case, we
  // don't need the shift by number_of_noncount_bits, but we do need to adjust
  // the factor by which we scale the threshold.
  if (ProfileInterpreter) {
    InterpreterBackwardBranchLimit = (int)((int64_t)CompileThreshold * (OnStackReplacePercentage - InterpreterProfilePercentage) / 100);
  } else {
    InterpreterBackwardBranchLimit = (int)(((int64_t)CompileThreshold * OnStackReplacePercentage / 100) << number_of_noncount_bits);
  }

  assert(0 <= InterpreterBackwardBranchLimit, "OSR threshold should be non-negative");
#endif
}
