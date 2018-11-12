/*
 * Copyright (c) 2002, 2018, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2012, 2018 SAP SE. All rights reserved.
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

#ifndef CPU_PPC_VM_GLOBALS_PPC_HPP
#define CPU_PPC_VM_GLOBALS_PPC_HPP

#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

// Sets the default values for platform dependent flags used by the runtime system.
// (see globals.hpp)

define_pd_global(bool, ShareVtableStubs,      true);
define_pd_global(bool, NeedsDeoptSuspend,     false); // Only register window machines need this.


define_pd_global(bool, ImplicitNullChecks,    true);  // Generate code for implicit null checks.
define_pd_global(bool, TrapBasedNullChecks,   true);
define_pd_global(bool, UncommonNullCast,      true);  // Uncommon-trap NULLs passed to check cast.

#define DEFAULT_STACK_YELLOW_PAGES (2)
#define DEFAULT_STACK_RED_PAGES (1)
// Java_java_net_SocketOutputStream_socketWrite0() uses a 64k buffer on the
// stack if compiled for unix and LP64. To pass stack overflow tests we need
// 20 shadow pages.
#define DEFAULT_STACK_SHADOW_PAGES (20 DEBUG_ONLY(+2))
#define DEFAULT_STACK_RESERVED_PAGES (1)

#define MIN_STACK_YELLOW_PAGES DEFAULT_STACK_YELLOW_PAGES
#define MIN_STACK_RED_PAGES DEFAULT_STACK_RED_PAGES
#define MIN_STACK_SHADOW_PAGES (3 DEBUG_ONLY(+1))
#define MIN_STACK_RESERVED_PAGES (0)

define_pd_global(intx, StackYellowPages,      DEFAULT_STACK_YELLOW_PAGES);
define_pd_global(intx, StackRedPages,         DEFAULT_STACK_RED_PAGES);
define_pd_global(intx, StackShadowPages,      DEFAULT_STACK_SHADOW_PAGES);
define_pd_global(intx, StackReservedPages,    DEFAULT_STACK_RESERVED_PAGES);

// Use large code-entry alignment.
define_pd_global(uintx, CodeCacheSegmentSize,  128);
define_pd_global(intx,  CodeEntryAlignment,    128);
define_pd_global(intx,  OptoLoopAlignment,     16);
define_pd_global(intx,  InlineFrequencyCount,  100);
define_pd_global(intx,  InlineSmallCode,       1500);

// Flags for template interpreter.
define_pd_global(bool, RewriteBytecodes,      true);
define_pd_global(bool, RewriteFrequentPairs,  true);

define_pd_global(bool, PreserveFramePointer,  false);

// GC Ergo Flags
define_pd_global(size_t, CMSYoungGenPerWorker, 16*M);  // Default max size of CMS young gen, per GC worker thread.

define_pd_global(uintx, TypeProfileLevel, 111);

define_pd_global(bool, CompactStrings, true);

// 2x unrolled loop is shorter with more than 9 HeapWords.
define_pd_global(intx, InitArrayShortSize, 9*BytesPerLong);

define_pd_global(bool, ThreadLocalHandshakes, true);

// Platform dependent flag handling: flags only defined on this platform.
#define ARCH_FLAGS(develop,      \
                   product,      \
                   diagnostic,   \
                   experimental, \
                   notproduct,   \
                   range,        \
                   constraint,   \
                   writeable)    \
                                                                            \
  product(uintx, PowerArchitecturePPC64, 0,                                 \
          "CPU Version: x for PowerX. Currently recognizes Power5 to "      \
          "Power8. Default is 0. Newer CPUs will be recognized as Power8.") \
                                                                            \
  product(bool, SuperwordUseVSX, false,                                     \
          "Use Power8 VSX instructions for superword optimization.")        \
                                                                            \
  /* Reoptimize code-sequences of calls at runtime, e.g. replace an */      \
  /* indirect call by a direct call.                                */      \
  product(bool, ReoptimizeCallSequences, true,                              \
          "Reoptimize code-sequences of calls at runtime.")                 \
                                                                            \
  /* Power 8: Configure Data Stream Control Register. */                    \
  product(uint64_t,DSCR_PPC64, (uintx)-1,                                   \
          "Power8 or later: Specify encoded value for Data Stream Control " \
          "Register")                                                       \
  product(uint64_t,DSCR_DPFD_PPC64, 8,                                      \
          "Power8 or later: DPFD (default prefetch depth) value of the "    \
          "Data Stream Control Register."                                   \
          " 0: hardware default, 1: none, 2-7: min-max, 8: don't touch")    \
  product(uint64_t,DSCR_URG_PPC64, 8,                                       \
          "Power8 or later: URG (depth attainment urgency) value of the "   \
          "Data Stream Control Register."                                   \
          " 0: hardware default, 1: none, 2-7: min-max, 8: don't touch")    \
                                                                            \
  product(bool, UseLoadInstructionsForStackBangingPPC64, false,             \
          "Use load instructions for stack banging.")                       \
                                                                            \
  /* special instructions */                                                \
                                                                            \
  product(bool, UseCountLeadingZerosInstructionsPPC64, true,                \
          "Use count leading zeros instructions.")                          \
                                                                            \
  product(bool, UseExtendedLoadAndReserveInstructionsPPC64, false,          \
          "Use extended versions of load-and-reserve instructions.")        \
                                                                            \
  product(bool, UseRotateAndMaskInstructionsPPC64, true,                    \
          "Use rotate and mask instructions.")                              \
                                                                            \
  product(bool, UseStaticBranchPredictionInCompareAndSwapPPC64, true,       \
          "Use static branch prediction hints in CAS operations.")          \
  product(bool, UseStaticBranchPredictionForUncommonPathsPPC64, false,      \
          "Use static branch prediction hints for uncommon paths.")         \
                                                                            \
  product(bool, UsePower6SchedulerPPC64, false,                             \
          "Use Power6 Scheduler.")                                          \
                                                                            \
  product(bool, InsertEndGroupPPC64, false,                                 \
          "Insert EndGroup instructions to optimize for Power6.")           \
                                                                            \
  /* Trap based checks. */                                                  \
  /* Trap based checks use the ppc trap instructions to check certain */    \
  /* conditions. This instruction raises a SIGTRAP caught by the      */    \
  /* exception handler of the VM.                                     */    \
  product(bool, UseSIGTRAP, true,                                           \
          "Allow trap instructions that make use of SIGTRAP. Use this to "  \
          "switch off all optimizations requiring SIGTRAP.")                \
  product(bool, TrapBasedICMissChecks, true,                                \
          "Raise and handle SIGTRAP if inline cache miss detected.")        \
  product(bool, TrapBasedNotEntrantChecks, true,                            \
          "Raise and handle SIGTRAP if calling not entrant or zombie"       \
          " method.")                                                       \
  product(bool, TraceTraps, false, "Trace all traps the signal handler"     \
          "handles.")                                                       \
                                                                            \
  product(bool, ZapMemory, false, "Write 0x0101... to empty memory."        \
          " Use this to ease debugging.")                                   \
                                                                            \
  /* Use Restricted Transactional Memory for lock elision */                \
  product(bool, UseRTMLocking, false,                                       \
          "Enable RTM lock eliding for inflated locks in compiled code")    \
                                                                            \
  experimental(bool, UseRTMForStackLocks, false,                            \
          "Enable RTM lock eliding for stack locks in compiled code")       \
                                                                            \
  product(bool, UseRTMDeopt, false,                                         \
          "Perform deopt and recompilation based on RTM abort ratio")       \
                                                                            \
  product(int, RTMRetryCount, 5,                                            \
          "Number of RTM retries on lock abort or busy")                    \
          range(0, max_jint)                                                \
                                                                            \
  experimental(int, RTMSpinLoopCount, 100,                                  \
          "Spin count for lock to become free before RTM retry")            \
          range(0, 32767) /* immediate operand limit on ppc */              \
                                                                            \
  experimental(int, RTMAbortThreshold, 1000,                                \
          "Calculate abort ratio after this number of aborts")              \
          range(0, max_jint)                                                \
                                                                            \
  experimental(int, RTMLockingThreshold, 10000,                             \
          "Lock count at which to do RTM lock eliding without "             \
          "abort ratio calculation")                                        \
          range(0, max_jint)                                                \
                                                                            \
  experimental(int, RTMAbortRatio, 50,                                      \
          "Lock abort ratio at which to stop use RTM lock eliding")         \
          range(0, 100) /* natural range */                                 \
                                                                            \
  experimental(int, RTMTotalCountIncrRate, 64,                              \
          "Increment total RTM attempted lock count once every n times")    \
          range(1, 32767) /* immediate operand limit on ppc */              \
          constraint(RTMTotalCountIncrRateConstraintFunc,AfterErgo)         \
                                                                            \
  experimental(intx, RTMLockingCalculationDelay, 0,                         \
          "Number of milliseconds to wait before start calculating aborts " \
          "for RTM locking")                                                \
                                                                            \
  experimental(bool, UseRTMXendForLockBusy, true,                           \
          "Use RTM Xend instead of Xabort when lock busy")                  \

#endif // CPU_PPC_VM_GLOBALS_PPC_HPP
