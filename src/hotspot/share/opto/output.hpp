/*
 * Copyright (c) 2000, 2019, Oracle and/or its affiliates. All rights reserved.
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

#ifndef SHARE_OPTO_OUTPUT_HPP
#define SHARE_OPTO_OUTPUT_HPP

#include "opto/ad.hpp"
#include "opto/constantTable.hpp"
#include "opto/phase.hpp"
#include "code/debugInfo.hpp"
#include "code/exceptionHandlerTable.hpp"
#include "utilities/globalDefinitions.hpp"
#include "utilities/macros.hpp"

class AbstractCompiler;
class Arena;
class Bundle;
class Block;
class Block_Array;
class ciMethod;
class Compile;
class MachNode;
class MachSafePointNode;
class Node;
class PhaseCFG;
#ifndef PRODUCT
#define DEBUG_ARG(x) , x
#else
#define DEBUG_ARG(x)
#endif

// Define the initial sizes for allocation of the resizable code buffer
enum {
  initial_const_capacity =   4 * 1024
};

class BufferSizingData {
public:
  int _stub;
  int _code;
  int _const;
  int _reloc;

  BufferSizingData() :
    _stub(0),
    _code(0),
    _const(0),
    _reloc(0)
  { };
};

class PhaseOutput : public Phase {
private:
  // Instruction bits passed off to the VM
  int                    _method_size;           // Size of nmethod code segment in bytes
  CodeBuffer             _code_buffer;           // Where the code is assembled
  int                    _first_block_size;      // Size of unvalidated entry point code / OSR poison code
  ExceptionHandlerTable  _handler_table;         // Table of native-code exception handlers
  ImplicitExceptionTable _inc_table;             // Table of implicit null checks in native code
  OopMapSet*             _oop_map_set;           // Table of oop maps (one for each safepoint location)
  BufferBlob*            _scratch_buffer_blob;   // For temporary code buffers.
  relocInfo*             _scratch_locs_memory;   // For temporary code buffers.
  int                    _scratch_const_size;    // For temporary code buffers.
  bool                   _in_scratch_emit_size;  // true when in scratch_emit_size.

  int                    _frame_slots;           // Size of total frame in stack slots
  CodeOffsets            _code_offsets;          // Offsets into the code for various interesting entries

  uint                   _node_bundling_limit;
  Bundle*                _node_bundling_base;    // Information for instruction bundling

  // For deopt
  int                    _orig_pc_slot;
  int                    _orig_pc_slot_offset_in_bytes;

  ConstantTable          _constant_table;        // The constant table for this compilation unit.

  BufferSizingData       _buf_sizes;
  Block*                 _block;
  uint                   _index;

  void perform_mach_node_analysis();
  void pd_perform_mach_node_analysis();

public:
  PhaseOutput();
  ~PhaseOutput();

  // Convert Nodes to instruction bits and pass off to the VM
  void Output();
  bool need_stack_bang(int frame_size_in_bytes) const;
  bool need_register_stack_bang() const;
  void compute_loop_first_inst_sizes();

  void install_code(ciMethod*         target,
                    int               entry_bci,
                    AbstractCompiler* compiler,
                    bool              has_unsafe_access,
                    bool              has_wide_vectors,
                    RTMState          rtm_state);

  void install_stub(const char* stub_name,
                    bool        caller_must_gc_arguments);

  // Constant table
  ConstantTable& constant_table() { return _constant_table; }

  // Code emission iterator
  Block* block()   { return _block; }
  int index()      { return _index; }

  // The architecture description provides short branch variants for some long
  // branch instructions. Replace eligible long branches with short branches.
  void shorten_branches(uint* blk_starts);
  ObjectValue* sv_for_node_id(GrowableArray<ScopeValue*> *objs, int id);
  void set_sv_for_object_node(GrowableArray<ScopeValue*> *objs, ObjectValue* sv);
  void FillLocArray( int idx, MachSafePointNode* sfpt, Node *local,
                     GrowableArray<ScopeValue*> *array,
                     GrowableArray<ScopeValue*> *objs );

  void Process_OopMap_Node(MachNode *mach, int current_offset);

  // Initialize code buffer
  void estimate_buffer_size(int& const_req);
  CodeBuffer* init_buffer();

  // Write out basic block data to code buffer
  void fill_buffer(CodeBuffer* cb, uint* blk_starts);

  // Compute the information for the exception tables
  void FillExceptionTables(uint cnt, uint *call_returns, uint *inct_starts, Label *blk_labels);

  // Perform instruction scheduling and bundling over the sequence of
  // instructions in backwards order.
  void ScheduleAndBundle();

  void install();

  // Instruction bits passed off to the VM
  int               code_size()                 { return _method_size; }
  CodeBuffer*       code_buffer()               { return &_code_buffer; }
  int               first_block_size()          { return _first_block_size; }
  void              set_frame_complete(int off) { if (!in_scratch_emit_size()) { _code_offsets.set_value(CodeOffsets::Frame_Complete, off); } }
  ExceptionHandlerTable*  handler_table()       { return &_handler_table; }
  ImplicitExceptionTable* inc_table()           { return &_inc_table; }
  OopMapSet*        oop_map_set()               { return _oop_map_set; }

  // Scratch buffer
  BufferBlob*       scratch_buffer_blob()       { return _scratch_buffer_blob; }
  void         init_scratch_buffer_blob(int const_size);
  void        clear_scratch_buffer_blob();
  void          set_scratch_buffer_blob(BufferBlob* b) { _scratch_buffer_blob = b; }
  relocInfo*        scratch_locs_memory()       { return _scratch_locs_memory; }
  void          set_scratch_locs_memory(relocInfo* b)  { _scratch_locs_memory = b; }

  // emit to scratch blob, report resulting size
  uint              scratch_emit_size(const Node* n);
  void       set_in_scratch_emit_size(bool x)   {        _in_scratch_emit_size = x; }
  bool           in_scratch_emit_size() const   { return _in_scratch_emit_size;     }

  enum ScratchBufferBlob {
    MAX_inst_size       = 2048,
    MAX_locs_size       = 128, // number of relocInfo elements
    MAX_const_size      = 128,
    MAX_stubs_size      = 128
  };

  int               frame_slots() const         { return _frame_slots; }
  int               frame_size_in_words() const; // frame_slots in units of the polymorphic 'words'
  int               frame_size_in_bytes() const { return _frame_slots << LogBytesPerInt; }

  int               bang_size_in_bytes() const;

  uint              node_bundling_limit();
  Bundle*           node_bundling_base();
  void          set_node_bundling_limit(uint n) { _node_bundling_limit = n; }
  void          set_node_bundling_base(Bundle* b) { _node_bundling_base = b; }

  Bundle* node_bundling(const Node *n);
  bool valid_bundle_info(const Node *n);

  bool starts_bundle(const Node *n) const;

  // Dump formatted assembly
#if defined(SUPPORT_OPTO_ASSEMBLY)
  void dump_asm_on(outputStream* ost, int* pcs, uint pc_limit);
  void dump_asm(int* pcs = NULL, uint pc_limit = 0) { dump_asm_on(tty, pcs, pc_limit); }
#else
  void dump_asm_on(outputStream* ost, int* pcs, uint pc_limit) { return; }
  void dump_asm(int* pcs = NULL, uint pc_limit = 0) { return; }
#endif

  // Build OopMaps for each GC point
  void BuildOopMaps();

#ifndef PRODUCT
  static void print_statistics();
#endif
};

#endif // SHARE_OPTO_OUTPUT_HPP
