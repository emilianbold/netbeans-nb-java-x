/*
 * Copyright (c) 2018, 2019, Oracle and/or its affiliates. All rights reserved.
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

#ifndef SHARE_GC_SHARED_WEAKPROCESSORPHASETIMES_HPP
#define SHARE_GC_SHARED_WEAKPROCESSORPHASETIMES_HPP

#include "gc/shared/oopStorageSet.hpp"
#include "gc/shared/weakProcessorPhases.hpp"
#include "memory/allocation.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/ticks.hpp"

template<typename T> class WorkerDataArray;

class WeakProcessorPhaseTimes : public CHeapObj<mtGC> {
  enum {
    DeadItems,
    TotalItems
  };
  uint _max_threads;
  uint _active_workers;

  // Total time for weak processor.
  double _total_time_sec;

  // Total time and associated items for each serially processed phase.
  static const uint phase_data_count = WeakProcessorPhases::serial_phase_count;
  // +1 because serial_phase_count == 0 in some build configurations.
  // Simpler to always allocate extra space than conditionalize.
  double _phase_times_sec[phase_data_count + 1];
  size_t _phase_dead_items[phase_data_count + 1];
  size_t _phase_total_items[phase_data_count + 1];
  void reset_phase_data();

  // Per-worker times and linked items.
  static const uint worker_data_count = WeakProcessorPhases::oopstorage_phase_count;
  WorkerDataArray<double>* _worker_data[worker_data_count];
  WorkerDataArray<size_t>* _worker_dead_items[worker_data_count];
  WorkerDataArray<size_t>* _worker_total_items[worker_data_count];

  WorkerDataArray<double>* worker_data(WeakProcessorPhase phase) const;

  void log_st_phase(WeakProcessorPhase phase, uint indent) const;
  void log_mt_phase_summary(WeakProcessorPhase phase, uint indent) const;
  template <typename T>
  void log_mt_phase_details(WorkerDataArray<T>* data, uint indent) const;

public:
  WeakProcessorPhaseTimes(uint max_threads);
  ~WeakProcessorPhaseTimes();

  uint max_threads() const;
  uint active_workers() const;
  void set_active_workers(uint n);

  double total_time_sec() const;
  double phase_time_sec(WeakProcessorPhase phase) const;
  double worker_time_sec(uint worker_id, WeakProcessorPhase phase) const;

  void record_total_time_sec(double time_sec);
  void record_phase_time_sec(WeakProcessorPhase phase, double time_sec);
  void record_phase_items(WeakProcessorPhase phase, size_t num_dead, size_t num_total);
  void record_worker_time_sec(uint worker_id, WeakProcessorPhase phase, double time_sec);
  void record_worker_items(uint worker_id, WeakProcessorPhase phase, size_t num_dead, size_t num_total);

  void reset();

  void log_print(uint indent = 0) const;
  void log_print_phases(uint indent = 0) const;
};

// Record total weak processor time and worker count in times.
// Does nothing if times is NULL.
class WeakProcessorTimeTracker : StackObj {
  WeakProcessorPhaseTimes* _times;
  Ticks _start_time;

public:
  WeakProcessorTimeTracker(WeakProcessorPhaseTimes* times);
  ~WeakProcessorTimeTracker();
};

// Record phase time contribution for the current thread in phase times.
// Does nothing if phase times is NULL.
class WeakProcessorPhaseTimeTracker : StackObj {
private:
  WeakProcessorPhaseTimes* _times;
  WeakProcessorPhase _phase;
  uint _worker_id;
  Ticks _start_time;

public:
  // For tracking serial phase times.
  // Precondition: WeakProcessorPhases::is_serial(phase)
  WeakProcessorPhaseTimeTracker(WeakProcessorPhaseTimes* times,
                                WeakProcessorPhase phase);

  // For tracking possibly parallel phase times (even if processed by
  // only one thread).
  // Precondition: WeakProcessorPhases::is_oopstorage(phase)
  // Precondition: worker_id < times->max_threads().
  WeakProcessorPhaseTimeTracker(WeakProcessorPhaseTimes* times,
                                WeakProcessorPhase phase,
                                uint worker_id);

  ~WeakProcessorPhaseTimeTracker();
};

#endif // SHARE_GC_SHARED_WEAKPROCESSORPHASETIMES_HPP
