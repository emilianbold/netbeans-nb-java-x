/*
 * Copyright (c) 2010, 2018, Oracle and/or its affiliates. All rights reserved.
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
#include "compiler/compileBroker.hpp"
#include "memory/resourceArea.hpp"
#include "runtime/arguments.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/safepointVerifiers.hpp"
#include "runtime/simpleThresholdPolicy.hpp"
#include "runtime/simpleThresholdPolicy.inline.hpp"
#include "code/scopeDesc.hpp"
#if INCLUDE_JVMCI
#include "jvmci/jvmciRuntime.hpp"
#endif

#ifdef TIERED

void SimpleThresholdPolicy::print_counters(const char* prefix, const methodHandle& mh) {
  int invocation_count = mh->invocation_count();
  int backedge_count = mh->backedge_count();
  MethodData* mdh = mh->method_data();
  int mdo_invocations = 0, mdo_backedges = 0;
  int mdo_invocations_start = 0, mdo_backedges_start = 0;
  if (mdh != NULL) {
    mdo_invocations = mdh->invocation_count();
    mdo_backedges = mdh->backedge_count();
    mdo_invocations_start = mdh->invocation_count_start();
    mdo_backedges_start = mdh->backedge_count_start();
  }
  tty->print(" %stotal=%d,%d %smdo=%d(%d),%d(%d)", prefix,
      invocation_count, backedge_count, prefix,
      mdo_invocations, mdo_invocations_start,
      mdo_backedges, mdo_backedges_start);
  tty->print(" %smax levels=%d,%d", prefix,
      mh->highest_comp_level(), mh->highest_osr_comp_level());
}

// Print an event.
void SimpleThresholdPolicy::print_event(EventType type, const methodHandle& mh, const methodHandle& imh,
                                        int bci, CompLevel level) {
  bool inlinee_event = mh() != imh();

  ttyLocker tty_lock;
  tty->print("%lf: [", os::elapsedTime());

  switch(type) {
  case CALL:
    tty->print("call");
    break;
  case LOOP:
    tty->print("loop");
    break;
  case COMPILE:
    tty->print("compile");
    break;
  case REMOVE_FROM_QUEUE:
    tty->print("remove-from-queue");
    break;
  case UPDATE_IN_QUEUE:
    tty->print("update-in-queue");
    break;
  case REPROFILE:
    tty->print("reprofile");
    break;
  case MAKE_NOT_ENTRANT:
    tty->print("make-not-entrant");
    break;
  default:
    tty->print("unknown");
  }

  tty->print(" level=%d ", level);

  ResourceMark rm;
  char *method_name = mh->name_and_sig_as_C_string();
  tty->print("[%s", method_name);
  if (inlinee_event) {
    char *inlinee_name = imh->name_and_sig_as_C_string();
    tty->print(" [%s]] ", inlinee_name);
  }
  else tty->print("] ");
  tty->print("@%d queues=%d,%d", bci, CompileBroker::queue_size(CompLevel_full_profile),
                                      CompileBroker::queue_size(CompLevel_full_optimization));

  print_specific(type, mh, imh, bci, level);

  if (type != COMPILE) {
    print_counters("", mh);
    if (inlinee_event) {
      print_counters("inlinee ", imh);
    }
    tty->print(" compilable=");
    bool need_comma = false;
    if (!mh->is_not_compilable(CompLevel_full_profile)) {
      tty->print("c1");
      need_comma = true;
    }
    if (!mh->is_not_osr_compilable(CompLevel_full_profile)) {
      if (need_comma) tty->print(",");
      tty->print("c1-osr");
      need_comma = true;
    }
    if (!mh->is_not_compilable(CompLevel_full_optimization)) {
      if (need_comma) tty->print(",");
      tty->print("c2");
      need_comma = true;
    }
    if (!mh->is_not_osr_compilable(CompLevel_full_optimization)) {
      if (need_comma) tty->print(",");
      tty->print("c2-osr");
    }
    tty->print(" status=");
    if (mh->queued_for_compilation()) {
      tty->print("in-queue");
    } else tty->print("idle");
  }
  tty->print_cr("]");
}

void SimpleThresholdPolicy::initialize() {
  if (FLAG_IS_DEFAULT(CICompilerCount)) {
    FLAG_SET_DEFAULT(CICompilerCount, 3);
  }
  int count = CICompilerCount;
#ifdef _LP64
  // On 64-bit systems, scale the number of compiler threads with
  // the number of cores available on the system. Scaling is not
  // performed on 32-bit systems because it can lead to exhaustion
  // of the virtual memory address space available to the JVM.
  if (CICompilerCountPerCPU) {
    count = MAX2(log2_intptr(os::active_processor_count()) * 3 / 2, 2);
    FLAG_SET_ERGO(intx, CICompilerCount, count);
  }
#endif
  if (TieredStopAtLevel < CompLevel_full_optimization) {
    // No C2 compiler thread required
    set_c1_count(count);
  } else {
    set_c1_count(MAX2(count / 3, 1));
    set_c2_count(MAX2(count - c1_count(), 1));
  }
  assert(count == c1_count() + c2_count(), "inconsistent compiler thread count");
}

void SimpleThresholdPolicy::set_carry_if_necessary(InvocationCounter *counter) {
  if (!counter->carry() && counter->count() > InvocationCounter::count_limit / 2) {
    counter->set_carry_flag();
  }
}

// Set carry flags on the counters if necessary
void SimpleThresholdPolicy::handle_counter_overflow(Method* method) {
  MethodCounters *mcs = method->method_counters();
  if (mcs != NULL) {
    set_carry_if_necessary(mcs->invocation_counter());
    set_carry_if_necessary(mcs->backedge_counter());
  }
  MethodData* mdo = method->method_data();
  if (mdo != NULL) {
    set_carry_if_necessary(mdo->invocation_counter());
    set_carry_if_necessary(mdo->backedge_counter());
  }
}

// Called with the queue locked and with at least one element
CompileTask* SimpleThresholdPolicy::select_task(CompileQueue* compile_queue) {
  return select_task_helper(compile_queue);
}

void SimpleThresholdPolicy::reprofile(ScopeDesc* trap_scope, bool is_osr) {
  for (ScopeDesc* sd = trap_scope;; sd = sd->sender()) {
    if (PrintTieredEvents) {
      methodHandle mh(sd->method());
      print_event(REPROFILE, mh, mh, InvocationEntryBci, CompLevel_none);
    }
    MethodData* mdo = sd->method()->method_data();
    if (mdo != NULL) {
      mdo->reset_start_counters();
    }
    if (sd->is_top()) break;
  }
}

nmethod* SimpleThresholdPolicy::event(const methodHandle& method, const methodHandle& inlinee,
                                      int branch_bci, int bci, CompLevel comp_level, CompiledMethod* nm, JavaThread* thread) {
  if (comp_level == CompLevel_none &&
      JvmtiExport::can_post_interpreter_events() &&
      thread->is_interp_only_mode()) {
    return NULL;
  }
  if (CompileTheWorld || ReplayCompiles) {
    // Don't trigger other compiles in testing mode
    return NULL;
  }

  handle_counter_overflow(method());
  if (method() != inlinee()) {
    handle_counter_overflow(inlinee());
  }

  if (PrintTieredEvents) {
    print_event(bci == InvocationEntryBci ? CALL : LOOP, method, inlinee, bci, comp_level);
  }

  if (bci == InvocationEntryBci) {
    method_invocation_event(method, inlinee, comp_level, nm, thread);
  } else {
    // method == inlinee if the event originated in the main method
    method_back_branch_event(method, inlinee, bci, comp_level, nm, thread);
    // Check if event led to a higher level OSR compilation
    nmethod* osr_nm = inlinee->lookup_osr_nmethod_for(bci, comp_level, false);
    if (osr_nm != NULL && osr_nm->comp_level() > comp_level) {
      // Perform OSR with new nmethod
      return osr_nm;
    }
  }
  return NULL;
}

// Check if the method can be compiled, change level if necessary
void SimpleThresholdPolicy::compile(const methodHandle& mh, int bci, CompLevel level, JavaThread* thread) {
  assert(level <= TieredStopAtLevel, "Invalid compilation level");
  if (level == CompLevel_none) {
    return;
  }
  if (level == CompLevel_aot) {
    if (mh->has_aot_code()) {
      if (PrintTieredEvents) {
        print_event(COMPILE, mh, mh, bci, level);
      }
      MutexLocker ml(Compile_lock);
      NoSafepointVerifier nsv;
      if (mh->has_aot_code() && mh->code() != mh->aot_code()) {
        mh->aot_code()->make_entrant();
        if (mh->has_compiled_code()) {
          mh->code()->make_not_entrant();
        }
        Method::set_code(mh, mh->aot_code());
      }
    }
    return;
  }

  // Check if the method can be compiled. If it cannot be compiled with C1, continue profiling
  // in the interpreter and then compile with C2 (the transition function will request that,
  // see common() ). If the method cannot be compiled with C2 but still can with C1, compile it with
  // pure C1.
  if (!can_be_compiled(mh, level)) {
    if (level == CompLevel_full_optimization && can_be_compiled(mh, CompLevel_simple)) {
        compile(mh, bci, CompLevel_simple, thread);
    }
    return;
  }
  if (bci != InvocationEntryBci && mh->is_not_osr_compilable(level)) {
    return;
  }
  if (!CompileBroker::compilation_is_in_queue(mh)) {
    if (PrintTieredEvents) {
      print_event(COMPILE, mh, mh, bci, level);
    }
    submit_compile(mh, bci, level, thread);
  }
}

// Tell the broker to compile the method
void SimpleThresholdPolicy::submit_compile(const methodHandle& mh, int bci, CompLevel level, JavaThread* thread) {
  int hot_count = (bci == InvocationEntryBci) ? mh->invocation_count() : mh->backedge_count();
  CompileBroker::compile_method(mh, bci, level, mh, hot_count, CompileTask::Reason_Tiered, thread);
}

// Call and loop predicates determine whether a transition to a higher
// compilation level should be performed (pointers to predicate functions
// are passed to common() transition function).
bool SimpleThresholdPolicy::loop_predicate(int i, int b, CompLevel cur_level, Method* method) {
  switch(cur_level) {
  case CompLevel_aot: {
    return loop_predicate_helper<CompLevel_aot>(i, b, 1.0, method);
  }
  case CompLevel_none:
  case CompLevel_limited_profile: {
    return loop_predicate_helper<CompLevel_none>(i, b, 1.0, method);
  }
  case CompLevel_full_profile: {
    return loop_predicate_helper<CompLevel_full_profile>(i, b, 1.0, method);
  }
  default:
    return true;
  }
}

bool SimpleThresholdPolicy::call_predicate(int i, int b, CompLevel cur_level, Method* method) {
  switch(cur_level) {
  case CompLevel_aot: {
    return call_predicate_helper<CompLevel_aot>(i, b, 1.0, method);
  }
  case CompLevel_none:
  case CompLevel_limited_profile: {
    return call_predicate_helper<CompLevel_none>(i, b, 1.0, method);
  }
  case CompLevel_full_profile: {
    return call_predicate_helper<CompLevel_full_profile>(i, b, 1.0, method);
  }
  default:
    return true;
  }
}

// Determine is a method is mature.
bool SimpleThresholdPolicy::is_mature(Method* method) {
  if (is_trivial(method)) return true;
  MethodData* mdo = method->method_data();
  if (mdo != NULL) {
    int i = mdo->invocation_count();
    int b = mdo->backedge_count();
    double k = ProfileMaturityPercentage / 100.0;
    return call_predicate_helper<CompLevel_full_profile>(i, b, k, method) ||
           loop_predicate_helper<CompLevel_full_profile>(i, b, k, method);
  }
  return false;
}

// Common transition function. Given a predicate determines if a method should transition to another level.
CompLevel SimpleThresholdPolicy::common(Predicate p, Method* method, CompLevel cur_level) {
  CompLevel next_level = cur_level;
  int i = method->invocation_count();
  int b = method->backedge_count();

  if (is_trivial(method) && cur_level != CompLevel_aot) {
    next_level = CompLevel_simple;
  } else {
    switch(cur_level) {
    case CompLevel_aot: {
      if ((this->*p)(i, b, cur_level, method)) {
        next_level = CompLevel_full_profile;
      }
    }
    break;
    case CompLevel_none:
      // If we were at full profile level, would we switch to full opt?
      if (common(p, method, CompLevel_full_profile) == CompLevel_full_optimization) {
        next_level = CompLevel_full_optimization;
      } else if ((this->*p)(i, b, cur_level, method)) {
        next_level = CompLevel_full_profile;
      }
      break;
    case CompLevel_limited_profile:
    case CompLevel_full_profile:
      {
        MethodData* mdo = method->method_data();
        if (mdo != NULL) {
          if (mdo->would_profile()) {
            int mdo_i = mdo->invocation_count_delta();
            int mdo_b = mdo->backedge_count_delta();
            if ((this->*p)(mdo_i, mdo_b, cur_level, method)) {
              next_level = CompLevel_full_optimization;
            }
          } else {
            next_level = CompLevel_full_optimization;
          }
        }
      }
      break;
    default:
      break;
    }
  }
  return MIN2(next_level, (CompLevel)TieredStopAtLevel);
}

// Determine if a method should be compiled with a normal entry point at a different level.
CompLevel SimpleThresholdPolicy::call_event(Method* method,  CompLevel cur_level, JavaThread* thread) {
  CompLevel osr_level = MIN2((CompLevel) method->highest_osr_comp_level(),
                             common(&SimpleThresholdPolicy::loop_predicate, method, cur_level));
  CompLevel next_level = common(&SimpleThresholdPolicy::call_predicate, method, cur_level);

  // If OSR method level is greater than the regular method level, the levels should be
  // equalized by raising the regular method level in order to avoid OSRs during each
  // invocation of the method.
  if (osr_level == CompLevel_full_optimization && cur_level == CompLevel_full_profile) {
    MethodData* mdo = method->method_data();
    guarantee(mdo != NULL, "MDO should not be NULL");
    if (mdo->invocation_count() >= 1) {
      next_level = CompLevel_full_optimization;
    }
  } else {
    next_level = MAX2(osr_level, next_level);
  }
#if INCLUDE_JVMCI
  if (UseJVMCICompiler) {
    next_level = JVMCIRuntime::adjust_comp_level(method, false, next_level, thread);
  }
#endif
  return next_level;
}

// Determine if we should do an OSR compilation of a given method.
CompLevel SimpleThresholdPolicy::loop_event(Method* method, CompLevel cur_level, JavaThread* thread) {
  CompLevel next_level = common(&SimpleThresholdPolicy::loop_predicate, method, cur_level);
  if (cur_level == CompLevel_none) {
    // If there is a live OSR method that means that we deopted to the interpreter
    // for the transition.
    CompLevel osr_level = MIN2((CompLevel)method->highest_osr_comp_level(), next_level);
    if (osr_level > CompLevel_none) {
      return osr_level;
    }
  }
#if INCLUDE_JVMCI
  if (UseJVMCICompiler) {
    next_level = JVMCIRuntime::adjust_comp_level(method, true, next_level, thread);
  }
#endif
  return next_level;
}


// Handle the invocation event.
void SimpleThresholdPolicy::method_invocation_event(const methodHandle& mh, const methodHandle& imh,
                                              CompLevel level, CompiledMethod* nm, JavaThread* thread) {
  if (is_compilation_enabled() && !CompileBroker::compilation_is_in_queue(mh)) {
    CompLevel next_level = call_event(mh(), level, thread);
    if (next_level != level) {
      compile(mh, InvocationEntryBci, next_level, thread);
    }
  }
}

// Handle the back branch event. Notice that we can compile the method
// with a regular entry from here.
void SimpleThresholdPolicy::method_back_branch_event(const methodHandle& mh, const methodHandle& imh,
                                                     int bci, CompLevel level, CompiledMethod* nm, JavaThread* thread) {
  // If the method is already compiling, quickly bail out.
  if (is_compilation_enabled() && !CompileBroker::compilation_is_in_queue(mh)) {
    // Use loop event as an opportunity to also check there's been
    // enough calls.
    CompLevel cur_level = comp_level(mh());
    CompLevel next_level = call_event(mh(), cur_level, thread);
    CompLevel next_osr_level = loop_event(mh(), level, thread);

    next_level = MAX2(next_level,
                      next_osr_level < CompLevel_full_optimization ? next_osr_level : cur_level);
    bool is_compiling = false;
    if (next_level != cur_level) {
      compile(mh, InvocationEntryBci, next_level, thread);
      is_compiling = true;
    }

    // Do the OSR version
    if (!is_compiling && next_osr_level != level) {
      compile(mh, bci, next_osr_level, thread);
    }
  }
}

#endif
