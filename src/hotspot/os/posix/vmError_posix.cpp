/*
 * Copyright (c) 2003, 2020, Oracle and/or its affiliates. All rights reserved.
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
#include "memory/metaspaceShared.hpp"
#include "runtime/arguments.hpp"
#include "runtime/os.hpp"
#include "runtime/thread.hpp"
#include "utilities/debug.hpp"
#include "utilities/vmError.hpp"

#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>

#ifdef LINUX
#include <sys/syscall.h>
#include <unistd.h>
#endif
#ifdef AIX
#include <unistd.h>
#endif
#ifdef BSD
#include <sys/syscall.h>
#include <unistd.h>
#endif


// handle all synchronous program error signals which may happen during error
// reporting. They must be unblocked, caught, handled.

static const int SIGNALS[] = { SIGSEGV, SIGBUS, SIGILL, SIGFPE, SIGTRAP }; // add more if needed
static const int NUM_SIGNALS = sizeof(SIGNALS) / sizeof(int);

// Space for our "saved" signal flags and handlers
static int resettedSigflags[NUM_SIGNALS];
static address resettedSighandler[NUM_SIGNALS];

// Needed for cancelable steps.
static volatile pthread_t reporter_thread_id;

void VMError::reporting_started() {
  // record pthread id of reporter thread.
  reporter_thread_id = ::pthread_self();
}

void VMError::interrupt_reporting_thread() {
  // We misuse SIGILL here, but it does not really matter. We need
  //  a signal which is handled by crash_handler and not likely to
  //  occurr during error reporting itself.
  ::pthread_kill(reporter_thread_id, SIGILL);
}

static void save_signal(int idx, int sig)
{
  struct sigaction sa;
  sigaction(sig, NULL, &sa);
  resettedSigflags[idx]   = sa.sa_flags;
  resettedSighandler[idx] = (sa.sa_flags & SA_SIGINFO)
                              ? CAST_FROM_FN_PTR(address, sa.sa_sigaction)
                              : CAST_FROM_FN_PTR(address, sa.sa_handler);
}

int VMError::get_resetted_sigflags(int sig) {
  for (int i = 0; i < NUM_SIGNALS; i++) {
    if (SIGNALS[i] == sig) {
      return resettedSigflags[i];
    }
  }
  return -1;
}

address VMError::get_resetted_sighandler(int sig) {
  for (int i = 0; i < NUM_SIGNALS; i++) {
    if (SIGNALS[i] == sig) {
      return resettedSighandler[i];
    }
  }
  return NULL;
}

static void crash_handler(int sig, siginfo_t* info, void* ucVoid) {
  // unmask current signal
  sigset_t newset;
  sigemptyset(&newset);
  sigaddset(&newset, sig);
  // also unmask other synchronous signals
  for (int i = 0; i < NUM_SIGNALS; i++) {
    sigaddset(&newset, SIGNALS[i]);
  }
  os::Posix::unblock_thread_signal_mask(&newset);

  // support safefetch faults in error handling
  ucontext_t* const uc = (ucontext_t*) ucVoid;
  address pc = (uc != NULL) ? os::Posix::ucontext_get_pc(uc) : NULL;

  // Correct pc for SIGILL, SIGFPE (see JDK-8176872)
  if (sig == SIGILL || sig == SIGFPE) {
    pc = (address) info->si_addr;
  }

  // Needed to make it possible to call SafeFetch.. APIs in error handling.
  if (uc && pc && StubRoutines::is_safefetch_fault(pc)) {
    os::Posix::ucontext_set_pc(uc, StubRoutines::continuation_for_safefetch_fault(pc));
    return;
  }

  // Needed because asserts may happen in error handling too.
#ifdef CAN_SHOW_REGISTERS_ON_ASSERT
  if ((sig == SIGSEGV || sig == SIGBUS) && info != NULL && info->si_addr == g_assert_poison) {
    if (handle_assert_poison_fault(ucVoid, info->si_addr)) {
      return;
    }
  }
#endif // CAN_SHOW_REGISTERS_ON_ASSERT

  VMError::report_and_die(NULL, sig, pc, info, ucVoid);
}

void VMError::reset_signal_handlers() {
  // install signal handlers for all synchronous program error signals
  sigset_t newset;
  sigemptyset(&newset);

  for (int i = 0; i < NUM_SIGNALS; i++) {
    save_signal(i, SIGNALS[i]);
    os::signal(SIGNALS[i], CAST_FROM_FN_PTR(void *, crash_handler));
    sigaddset(&newset, SIGNALS[i]);
  }
  os::Posix::unblock_thread_signal_mask(&newset);

}

// Write a hint to the stream in case siginfo relates to a segv/bus error
// and the offending address points into CDS archive.
void VMError::check_failing_cds_access(outputStream* st, const void* siginfo) {
#if INCLUDE_CDS
  if (siginfo && UseSharedSpaces) {
    const siginfo_t* const si = (siginfo_t*)siginfo;
    if (si->si_signo == SIGBUS || si->si_signo == SIGSEGV) {
      const void* const fault_addr = si->si_addr;
      if (fault_addr != NULL) {
        if (MetaspaceShared::is_in_shared_metaspace(fault_addr)) {
          st->print("Error accessing class data sharing archive. "
            "Mapped file inaccessible during execution, possible disk/network problem.");
        }
      }
    }
  }
#endif
}

