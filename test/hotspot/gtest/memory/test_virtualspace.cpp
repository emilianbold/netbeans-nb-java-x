/*
 * Copyright (c) 2018, 2020, Oracle and/or its affiliates. All rights reserved.
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

#include "precompiled.hpp"
#include "memory/virtualspace.hpp"
#include "runtime/os.hpp"
#include "oops/oop.hpp"
#include "utilities/align.hpp"
#include "unittest.hpp"

namespace {
  class MemoryReleaser {
    ReservedSpace* const _rs;
   public:
    MemoryReleaser(ReservedSpace* rs) : _rs(rs) { }
    ~MemoryReleaser() {
      if (_rs->special()) {
        EXPECT_TRUE(os::release_memory_special(_rs->base(), _rs->size()));
      } else {
        EXPECT_TRUE(os::release_memory(_rs->base(), _rs->size()));
      }
    }
  };

  static void small_page_write(void* addr, size_t size) {
    size_t page_size = os::vm_page_size();

    char* end = (char*) addr + size;
    for (char* p = (char*) addr; p < end; p += page_size) {
      *p = 1;
    }
  }

  // have to use these functions, as gtest's _PRED macros don't like is_aligned
  // nor (is_aligned<size_t, size_t>)
  static bool is_size_aligned(size_t size, size_t alignment) {
    return is_aligned(size, alignment);
  }
  static bool is_ptr_aligned(void* ptr, size_t alignment) {
    return is_aligned(ptr, alignment);
  }

  static void test_reserved_size(size_t size) {
    ASSERT_PRED2(is_size_aligned, size, os::vm_allocation_granularity());

    ReservedSpace rs(size);
    MemoryReleaser releaser(&rs);

    EXPECT_TRUE(rs.base() != NULL) << "rs.special: " << rs.special();
    EXPECT_EQ(size, rs.size()) << "rs.special: " << rs.special();

    if (rs.special()) {
      small_page_write(rs.base(), size);
    }
  }

  static void test_reserved_size_alignment(size_t size, size_t alignment) {
    ASSERT_PRED2(is_size_aligned, size, alignment) << "Incorrect input parameters";

    ReservedSpace rs(size, alignment, UseLargePages, (char *) NULL);

    ASSERT_TRUE(rs.base() != NULL) << "rs.special = " << rs.special();
    ASSERT_EQ(size, rs.size()) << "rs.special = " << rs.special();

    EXPECT_PRED2(is_ptr_aligned, rs.base(), alignment)
            << "aligned sizes should always give aligned addresses";
    EXPECT_PRED2(is_ptr_aligned, (void*) rs.size(), alignment)
            << "aligned sizes should always give aligned addresses";

    if (rs.special()) {
      small_page_write(rs.base(), size);
    }
  }

  static void test_reserved_size_alignment_page_type(size_t size, size_t alignment, bool maybe_large) {
    if (size < alignment) {
      // Tests might set -XX:LargePageSizeInBytes=<small pages> and cause unexpected input arguments for this test.
      ASSERT_EQ((size_t) os::vm_page_size(), os::large_page_size()) << "Test needs further refinement";
      return;
    }

    ASSERT_PRED2(is_size_aligned, size, os::vm_allocation_granularity()) << "Must be at least AG aligned";
    ASSERT_PRED2(is_size_aligned, size, alignment) << "Must be at least AG aligned";

    bool large = maybe_large && UseLargePages && size >= os::large_page_size();

    ReservedSpace rs(size, alignment, large);
    MemoryReleaser releaser(&rs);

    EXPECT_TRUE(rs.base() != NULL) << "rs.special: " << rs.special();
    EXPECT_EQ(size, rs.size()) << "rs.special: " << rs.special();

    if (rs.special()) {
      small_page_write(rs.base(), size);
    }
  }
}

TEST_VM(ReservedSpace, size_alignment) {
  size_t size = 2 * 1024 * 1024;
  size_t ag   = os::vm_allocation_granularity();

  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment(size,      ag));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment(size * 2,  ag));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment(size * 10, ag));
}

TEST_VM(ReservedSpace, size) {
  size_t size = 2 * 1024 * 1024;
  size_t ag = os::vm_allocation_granularity();

  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 1));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 2));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 10));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(ag));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size - ag));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size + ag));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 2));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 2 - ag));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 2 + ag));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 3));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 3 - ag));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 3 + ag));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 10));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size(size * 10 + size / 2));
}

TEST_VM(ReservedSpace, size_alignment_page_type) {
  size_t ag = os::vm_allocation_granularity();

  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(ag,      ag    , false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(ag * 2,  ag    , false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(ag * 3,  ag    , false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(ag * 2,  ag * 2, false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(ag * 4,  ag * 2, false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(ag * 8,  ag * 2, false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(ag * 4,  ag * 4, false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(ag * 8,  ag * 4, false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(ag * 16, ag * 4, false));
}

TEST_VM(ReservedSpace, size_alignment_page_type_large_page) {
  if (!UseLargePages) {
    return;
  }

  size_t ag = os::vm_allocation_granularity();
  size_t lp = os::large_page_size();

  // Without large pages
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp,     ag * 4, false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 2, ag * 4, false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 4, ag * 4, false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp,     lp    , false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 2, lp    , false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 3, lp    , false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 2, lp * 2, false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 4, lp * 2, false));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 8, lp * 2, false));

  // With large pages
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp, ag * 4    , true));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 2, ag * 4, true));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 4, ag * 4, true));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp, lp        , true));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 2, lp    , true));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 3, lp    , true));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 2, lp * 2, true));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 4, lp * 2, true));
  EXPECT_NO_FATAL_FAILURE(test_reserved_size_alignment_page_type(lp * 8, lp * 2, true));
}

namespace {
  enum TestLargePages {
    Default,
    Disable,
    Reserve,
    Commit
  };

  class ReservedSpaceReleaser {
    ReservedSpace* const _rs;
   public:
    ReservedSpaceReleaser(ReservedSpace* rs) : _rs(rs) { }
    ~ReservedSpaceReleaser() {
      _rs->release();
    }
  };

  ReservedSpace reserve_memory(size_t reserve_size_aligned, TestLargePages mode) {
    switch(mode) {
      default:
      case Default:
      case Reserve:
        return ReservedSpace(reserve_size_aligned);
      case Disable:
      case Commit:
        return ReservedSpace(reserve_size_aligned,
                             os::vm_allocation_granularity(),
                             /* large */ false);
    }
  }

  bool initialize_virtual_space(VirtualSpace& vs, ReservedSpace rs, TestLargePages mode) {
    switch(mode) {
      default:
      case Default:
      case Reserve:
        return vs.initialize(rs, 0);
      case Disable:
        return vs.initialize_with_granularity(rs, 0, os::vm_page_size());
      case Commit:
        return vs.initialize_with_granularity(rs, 0, os::page_size_for_region_unaligned(rs.size(), 1));
    }
  }

 void test_virtual_space_actual_committed_space(size_t reserve_size, size_t commit_size,
                                                TestLargePages mode = Default) {
    size_t granularity = os::vm_allocation_granularity();
    size_t reserve_size_aligned = align_up(reserve_size, granularity);

    ReservedSpace reserved = reserve_memory(reserve_size_aligned, mode);
    ReservedSpaceReleaser releaser(&reserved);

    ASSERT_TRUE(reserved.is_reserved());

    VirtualSpace vs;
    ASSERT_TRUE(initialize_virtual_space(vs, reserved, mode)) << "Failed to initialize VirtualSpace";
    vs.expand_by(commit_size, false);

    if (vs.special()) {
      EXPECT_EQ(reserve_size_aligned, vs.actual_committed_size());
    } else {
      EXPECT_GE(vs.actual_committed_size(), commit_size);
      // Approximate the commit granularity.
      // Make sure that we don't commit using large pages
      // if large pages has been disabled for this VirtualSpace.
      size_t commit_granularity = (mode == Disable || !UseLargePages) ?
                                   os::vm_page_size() : os::large_page_size();
      EXPECT_LT(vs.actual_committed_size(), commit_size + commit_granularity);
    }
  }
}

TEST_VM(VirtualSpace, actual_committed_space) {
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(4 * K,  0));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(4 * K,  4 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(8 * K,  0));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(8 * K,  4 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(8 * K,  8 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(12 * K, 0));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(12 * K, 4 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(12 * K, 8 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(12 * K, 12 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(64 * K, 0));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(64 * K, 32 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(64 * K, 64 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(2 * M,  0));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(2 * M,  4 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(2 * M,  64 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(2 * M,  1 * M));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(2 * M,  2 * M));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 0));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 4 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 8 * K));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 1 * M));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 2 * M));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 5 * M));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 10 * M));
}

TEST_VM(VirtualSpace, actual_committed_space_one_large_page) {
  if (!UseLargePages) {
    return;
  }

  size_t large_page_size = os::large_page_size();

  ReservedSpace reserved(large_page_size, large_page_size, true);
  ReservedSpaceReleaser releaser(&reserved);
  ASSERT_TRUE(reserved.is_reserved());

  VirtualSpace vs;
  ASSERT_TRUE(vs.initialize(reserved, 0)) << "Failed to initialize VirtualSpace";
  vs.expand_by(large_page_size, false);

  EXPECT_EQ(large_page_size, vs.actual_committed_size());
}

TEST_VM(VirtualSpace, disable_large_pages) {
  if (!UseLargePages) {
    return;
  }
  // These test cases verify that if we force VirtualSpace to disable large pages
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 0,      Disable));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 4 * K,  Disable));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 8 * K,  Disable));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 1 * M,  Disable));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 2 * M,  Disable));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 5 * M,  Disable));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 10 * M, Disable));

  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 0,      Reserve));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 4 * K,  Reserve));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 8 * K,  Reserve));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 1 * M,  Reserve));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 2 * M,  Reserve));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 5 * M,  Reserve));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 10 * M, Reserve));

  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 0,      Commit));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 4 * K,  Commit));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 8 * K,  Commit));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 1 * M,  Commit));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 2 * M,  Commit));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 5 * M,  Commit));
  EXPECT_NO_FATAL_FAILURE(test_virtual_space_actual_committed_space(10 * M, 10 * M, Commit));
}
