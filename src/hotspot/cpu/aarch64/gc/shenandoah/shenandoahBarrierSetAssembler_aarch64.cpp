/*
 * Copyright (c) 2018, Red Hat, Inc. All rights reserved.
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
#include "gc/shenandoah/shenandoahBarrierSetAssembler.hpp"
#include "gc/shenandoah/shenandoahHeap.hpp"
#include "gc/shenandoah/shenandoahHeapRegion.hpp"
#include "gc/shenandoah/shenandoahHeuristics.hpp"
#include "gc/shenandoah/shenandoahRuntime.hpp"
#include "gc/shenandoah/shenandoahThreadLocalData.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interp_masm.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/thread.hpp"
#ifdef COMPILER1
#include "c1/c1_LIRAssembler.hpp"
#include "c1/c1_MacroAssembler.hpp"
#include "gc/shenandoah/c1/shenandoahBarrierSetC1.hpp"
#endif

#define __ masm->

address ShenandoahBarrierSetAssembler::_shenandoah_wb = NULL;

void ShenandoahBarrierSetAssembler::arraycopy_prologue(MacroAssembler* masm, DecoratorSet decorators, bool is_oop,
                                                       Register addr, Register count, RegSet saved_regs) {
  if (is_oop) {
    bool dest_uninitialized = (decorators & IS_DEST_UNINITIALIZED) != 0;
    if (!dest_uninitialized && !ShenandoahHeap::heap()->heuristics()->can_do_traversal_gc()) {
      __ push(saved_regs, sp);
      if (count == c_rarg0) {
        if (addr == c_rarg1) {
          // exactly backwards!!
          __ mov(rscratch1, c_rarg0);
          __ mov(c_rarg0, c_rarg1);
          __ mov(c_rarg1, rscratch1);
        } else {
          __ mov(c_rarg1, count);
          __ mov(c_rarg0, addr);
        }
      } else {
        __ mov(c_rarg0, addr);
        __ mov(c_rarg1, count);
      }
      if (UseCompressedOops) {
        __ call_VM_leaf(CAST_FROM_FN_PTR(address, ShenandoahRuntime::write_ref_array_pre_narrow_oop_entry), 2);
      } else {
        __ call_VM_leaf(CAST_FROM_FN_PTR(address, ShenandoahRuntime::write_ref_array_pre_oop_entry), 2);
      }
      __ pop(saved_regs, sp);
    }
  }
}

void ShenandoahBarrierSetAssembler::arraycopy_epilogue(MacroAssembler* masm, DecoratorSet decorators, bool is_oop,
                                                       Register start, Register end, Register scratch, RegSet saved_regs) {
  if (is_oop) {
    __ push(saved_regs, sp);
    // must compute element count unless barrier set interface is changed (other platforms supply count)
    assert_different_registers(start, end, scratch);
    __ lea(scratch, Address(end, BytesPerHeapOop));
    __ sub(scratch, scratch, start);               // subtract start to get #bytes
    __ lsr(scratch, scratch, LogBytesPerHeapOop);  // convert to element count
    __ mov(c_rarg0, start);
    __ mov(c_rarg1, scratch);
    __ call_VM_leaf(CAST_FROM_FN_PTR(address, ShenandoahRuntime::write_ref_array_post_entry), 2);
    __ pop(saved_regs, sp);
  }
}

void ShenandoahBarrierSetAssembler::shenandoah_write_barrier_pre(MacroAssembler* masm,
                                                                 Register obj,
                                                                 Register pre_val,
                                                                 Register thread,
                                                                 Register tmp,
                                                                 bool tosca_live,
                                                                 bool expand_call) {
  if (ShenandoahSATBBarrier) {
    satb_write_barrier_pre(masm, obj, pre_val, thread, tmp, tosca_live, expand_call);
  }
}

void ShenandoahBarrierSetAssembler::satb_write_barrier_pre(MacroAssembler* masm,
                                                           Register obj,
                                                           Register pre_val,
                                                           Register thread,
                                                           Register tmp,
                                                           bool tosca_live,
                                                           bool expand_call) {
  // If expand_call is true then we expand the call_VM_leaf macro
  // directly to skip generating the check by
  // InterpreterMacroAssembler::call_VM_leaf_base that checks _last_sp.

  assert(thread == rthread, "must be");

  Label done;
  Label runtime;

  assert_different_registers(obj, pre_val, tmp, rscratch1);
  assert(pre_val != noreg &&  tmp != noreg, "expecting a register");

  Address in_progress(thread, in_bytes(ShenandoahThreadLocalData::satb_mark_queue_active_offset()));
  Address index(thread, in_bytes(ShenandoahThreadLocalData::satb_mark_queue_index_offset()));
  Address buffer(thread, in_bytes(ShenandoahThreadLocalData::satb_mark_queue_buffer_offset()));

  // Is marking active?
  if (in_bytes(SATBMarkQueue::byte_width_of_active()) == 4) {
    __ ldrw(tmp, in_progress);
  } else {
    assert(in_bytes(SATBMarkQueue::byte_width_of_active()) == 1, "Assumption");
    __ ldrb(tmp, in_progress);
  }
  __ cbzw(tmp, done);

  // Do we need to load the previous value?
  if (obj != noreg) {
    __ load_heap_oop(pre_val, Address(obj, 0), noreg, noreg, AS_RAW);
  }

  // Is the previous value null?
  __ cbz(pre_val, done);

  // Can we store original value in the thread's buffer?
  // Is index == 0?
  // (The index field is typed as size_t.)

  __ ldr(tmp, index);                      // tmp := *index_adr
  __ cbz(tmp, runtime);                    // tmp == 0?
                                        // If yes, goto runtime

  __ sub(tmp, tmp, wordSize);              // tmp := tmp - wordSize
  __ str(tmp, index);                      // *index_adr := tmp
  __ ldr(rscratch1, buffer);
  __ add(tmp, tmp, rscratch1);             // tmp := tmp + *buffer_adr

  // Record the previous value
  __ str(pre_val, Address(tmp, 0));
  __ b(done);

  __ bind(runtime);
  // save the live input values
  RegSet saved = RegSet::of(pre_val);
  if (tosca_live) saved += RegSet::of(r0);
  if (obj != noreg) saved += RegSet::of(obj);

  __ push(saved, sp);

  // Calling the runtime using the regular call_VM_leaf mechanism generates
  // code (generated by InterpreterMacroAssember::call_VM_leaf_base)
  // that checks that the *(rfp+frame::interpreter_frame_last_sp) == NULL.
  //
  // If we care generating the pre-barrier without a frame (e.g. in the
  // intrinsified Reference.get() routine) then ebp might be pointing to
  // the caller frame and so this check will most likely fail at runtime.
  //
  // Expanding the call directly bypasses the generation of the check.
  // So when we do not have have a full interpreter frame on the stack
  // expand_call should be passed true.

  if (expand_call) {
    assert(pre_val != c_rarg1, "smashed arg");
    __ super_call_VM_leaf(CAST_FROM_FN_PTR(address, ShenandoahRuntime::write_ref_field_pre_entry), pre_val, thread);
  } else {
    __ call_VM_leaf(CAST_FROM_FN_PTR(address, ShenandoahRuntime::write_ref_field_pre_entry), pre_val, thread);
  }

  __ pop(saved, sp);

  __ bind(done);
}

void ShenandoahBarrierSetAssembler::read_barrier(MacroAssembler* masm, Register dst) {
  if (ShenandoahReadBarrier) {
    read_barrier_impl(masm, dst);
  }
}

void ShenandoahBarrierSetAssembler::read_barrier_impl(MacroAssembler* masm, Register dst) {
  assert(UseShenandoahGC && (ShenandoahReadBarrier || ShenandoahStoreValReadBarrier || ShenandoahCASBarrier), "should be enabled");
  Label is_null;
  __ cbz(dst, is_null);
  read_barrier_not_null_impl(masm, dst);
  __ bind(is_null);
}

void ShenandoahBarrierSetAssembler::read_barrier_not_null(MacroAssembler* masm, Register dst) {
  if (ShenandoahReadBarrier) {
    read_barrier_not_null_impl(masm, dst);
  }
}


void ShenandoahBarrierSetAssembler::read_barrier_not_null_impl(MacroAssembler* masm, Register dst) {
  assert(UseShenandoahGC && (ShenandoahReadBarrier || ShenandoahStoreValReadBarrier || ShenandoahCASBarrier), "should be enabled");
  __ ldr(dst, Address(dst, ShenandoahBrooksPointer::byte_offset()));
}

void ShenandoahBarrierSetAssembler::write_barrier(MacroAssembler* masm, Register dst) {
  if (ShenandoahWriteBarrier) {
    write_barrier_impl(masm, dst);
  }
}

void ShenandoahBarrierSetAssembler::write_barrier_impl(MacroAssembler* masm, Register dst) {
  assert(UseShenandoahGC && (ShenandoahWriteBarrier || ShenandoahStoreValEnqueueBarrier), "Should be enabled");
  assert(dst != rscratch1, "need rscratch1");
  assert(dst != rscratch2, "need rscratch2");

  Label done;

  Address gc_state(rthread, in_bytes(ShenandoahThreadLocalData::gc_state_offset()));
  __ ldrb(rscratch1, gc_state);

  // Check for heap stability
  __ mov(rscratch2, ShenandoahHeap::HAS_FORWARDED | ShenandoahHeap::EVACUATION | ShenandoahHeap::TRAVERSAL);
  __ tst(rscratch1, rscratch2);
  __ br(Assembler::EQ, done);

  // Heap is unstable, need to perform the read-barrier even if WB is inactive
  __ ldr(dst, Address(dst, ShenandoahBrooksPointer::byte_offset()));

  // Check for evacuation-in-progress and jump to WB slow-path if needed
  __ mov(rscratch2, ShenandoahHeap::EVACUATION | ShenandoahHeap::TRAVERSAL);
  __ tst(rscratch1, rscratch2);
  __ br(Assembler::EQ, done);

  RegSet to_save = RegSet::of(r0);
  if (dst != r0) {
    __ push(to_save, sp);
    __ mov(r0, dst);
  }

  __ far_call(RuntimeAddress(CAST_FROM_FN_PTR(address, ShenandoahBarrierSetAssembler::shenandoah_wb())));

  if (dst != r0) {
    __ mov(dst, r0);
    __ pop(to_save, sp);
  }

  __ bind(done);
}

void ShenandoahBarrierSetAssembler::storeval_barrier(MacroAssembler* masm, Register dst, Register tmp) {
  if (ShenandoahStoreValEnqueueBarrier) {
    Label is_null;
    __ cbz(dst, is_null);
    write_barrier_impl(masm, dst);
    __ bind(is_null);
    // Save possibly live regs.
    RegSet live_regs = RegSet::range(r0, r4) - dst;
    __ push(live_regs, sp);
    __ strd(v0, __ pre(sp, 2 * -wordSize));

    satb_write_barrier_pre(masm, noreg, dst, rthread, tmp, true, false);

    // Restore possibly live regs.
    __ ldrd(v0, __ post(sp, 2 * wordSize));
    __ pop(live_regs, sp);
  }
  if (ShenandoahStoreValReadBarrier) {
    read_barrier_impl(masm, dst);
  }
}

void ShenandoahBarrierSetAssembler::load_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type,
                                            Register dst, Address src, Register tmp1, Register tmp_thread) {
  bool on_oop = type == T_OBJECT || type == T_ARRAY;
  bool in_heap = (decorators & IN_HEAP) != 0;
  bool on_weak = (decorators & ON_WEAK_OOP_REF) != 0;
  bool on_phantom = (decorators & ON_PHANTOM_OOP_REF) != 0;
  bool on_reference = on_weak || on_phantom;

  if (in_heap) {
    read_barrier_not_null(masm, src.base());
  }

  BarrierSetAssembler::load_at(masm, decorators, type, dst, src, tmp1, tmp_thread);
  if (ShenandoahKeepAliveBarrier && on_oop && on_reference) {
    __ enter();
    satb_write_barrier_pre(masm /* masm */,
                           noreg /* obj */,
                           dst /* pre_val */,
                           rthread /* thread */,
                           tmp1 /* tmp */,
                           true /* tosca_live */,
                           true /* expand_call */);
    __ leave();
  }
}

void ShenandoahBarrierSetAssembler::store_at(MacroAssembler* masm, DecoratorSet decorators, BasicType type,
                                             Address dst, Register val, Register tmp1, Register tmp2) {
  bool on_oop = type == T_OBJECT || type == T_ARRAY;
  bool in_heap = (decorators & IN_HEAP) != 0;
  if (in_heap) {
    write_barrier(masm, dst.base());
  }
  if (!on_oop) {
    BarrierSetAssembler::store_at(masm, decorators, type, dst, val, tmp1, tmp2);
    return;
  }

  // flatten object address if needed
  if (dst.index() == noreg && dst.offset() == 0) {
    if (dst.base() != r3) {
      __ mov(r3, dst.base());
    }
  } else {
    __ lea(r3, dst);
  }

  shenandoah_write_barrier_pre(masm,
                               r3 /* obj */,
                               tmp2 /* pre_val */,
                               rthread /* thread */,
                               tmp1  /* tmp */,
                               val != noreg /* tosca_live */,
                               false /* expand_call */);

  if (val == noreg) {
    BarrierSetAssembler::store_at(masm, decorators, type, Address(r3, 0), noreg, noreg, noreg);
  } else {
    storeval_barrier(masm, val, tmp1);
    // G1 barrier needs uncompressed oop for region cross check.
    Register new_val = val;
    if (UseCompressedOops) {
      new_val = rscratch2;
      __ mov(new_val, val);
    }
    BarrierSetAssembler::store_at(masm, decorators, type, Address(r3, 0), val, noreg, noreg);
  }

}

void ShenandoahBarrierSetAssembler::obj_equals(MacroAssembler* masm, Register op1, Register op2) {
  __ cmp(op1, op2);
  if (ShenandoahAcmpBarrier) {
    Label done;
    __ br(Assembler::EQ, done);
    // The object may have been evacuated, but we won't see it without a
    // membar here.
    __ membar(Assembler::LoadStore| Assembler::LoadLoad);
    read_barrier(masm, op1);
    read_barrier(masm, op2);
    __ cmp(op1, op2);
    __ bind(done);
  }
}

void ShenandoahBarrierSetAssembler::tlab_allocate(MacroAssembler* masm, Register obj,
                                                  Register var_size_in_bytes,
                                                  int con_size_in_bytes,
                                                  Register t1,
                                                  Register t2,
                                                  Label& slow_case) {

  assert_different_registers(obj, t2);
  assert_different_registers(obj, var_size_in_bytes);
  Register end = t2;

  __ ldr(obj, Address(rthread, JavaThread::tlab_top_offset()));
  if (var_size_in_bytes == noreg) {
    __ lea(end, Address(obj, (int) (con_size_in_bytes + ShenandoahBrooksPointer::byte_size())));
  } else {
    __ add(var_size_in_bytes, var_size_in_bytes, ShenandoahBrooksPointer::byte_size());
    __ lea(end, Address(obj, var_size_in_bytes));
  }
  __ ldr(rscratch1, Address(rthread, JavaThread::tlab_end_offset()));
  __ cmp(end, rscratch1);
  __ br(Assembler::HI, slow_case);

  // update the tlab top pointer
  __ str(end, Address(rthread, JavaThread::tlab_top_offset()));

  __ add(obj, obj, ShenandoahBrooksPointer::byte_size());
  __ str(obj, Address(obj, ShenandoahBrooksPointer::byte_offset()));

  // recover var_size_in_bytes if necessary
  if (var_size_in_bytes == end) {
    __ sub(var_size_in_bytes, var_size_in_bytes, obj);
  }
}

void ShenandoahBarrierSetAssembler::resolve(MacroAssembler* masm, DecoratorSet decorators, Register obj) {
  bool oop_not_null = (decorators & IS_NOT_NULL) != 0;
  bool is_write = (decorators & ACCESS_WRITE) != 0;
  if (is_write) {
    if (oop_not_null) {
      write_barrier(masm, obj);
    } else {
      Label done;
      __ cbz(obj, done);
      write_barrier(masm, obj);
      __ bind(done);
    }
  } else {
    if (oop_not_null) {
      read_barrier_not_null(masm, obj);
    } else {
      read_barrier(masm, obj);
    }
  }
}

void ShenandoahBarrierSetAssembler::cmpxchg_oop(MacroAssembler* masm, Register addr, Register expected, Register new_val,
                                                bool acquire, bool release, bool weak, bool encode,
                                                Register tmp1, Register tmp2, Register tmp3,
                                                Register result) {

  if (!ShenandoahCASBarrier) {
    if (UseCompressedOops) {
      if (encode) {
        __ encode_heap_oop(tmp1, expected);
        expected = tmp1;
        __ encode_heap_oop(tmp3, new_val);
        new_val = tmp3;
      }
      __ cmpxchg(addr, expected, new_val, Assembler::word, /* acquire*/ true, /* release*/ true, /* weak*/ false, rscratch1);
      __ membar(__ AnyAny);
    } else {
      __ cmpxchg(addr, expected, new_val, Assembler::xword, /* acquire*/ true, /* release*/ true, /* weak*/ false, rscratch1);
      __ membar(__ AnyAny);
    }
    return;
  }

  if (encode) {
    storeval_barrier(masm, new_val, tmp3);
  }

  if (UseCompressedOops) {
    if (encode) {
      __ encode_heap_oop(tmp1, expected);
      expected = tmp1;
      __ encode_heap_oop(tmp2, new_val);
      new_val = tmp2;
    }
  }
  bool is_cae = (result != noreg);
  bool is_narrow = UseCompressedOops;
  Assembler::operand_size size = is_narrow ? Assembler::word : Assembler::xword;
  if (! is_cae) result = rscratch1;

  assert_different_registers(addr, expected, new_val, result, tmp3);

  Label retry, done, fail;

  // CAS, using LL/SC pair.
  __ bind(retry);
  __ load_exclusive(result, addr, size, acquire);
  if (is_narrow) {
    __ cmpw(result, expected);
  } else {
    __ cmp(result, expected);
  }
  __ br(Assembler::NE, fail);
  __ store_exclusive(tmp3, new_val, addr, size, release);
  if (weak) {
    __ cmpw(tmp3, 0u); // If the store fails, return NE to our caller
  } else {
    __ cbnzw(tmp3, retry);
  }
  __ b(done);

 __  bind(fail);
  // Check if rb(expected)==rb(result)
  // Shuffle registers so that we have memory value ready for next expected.
  __ mov(tmp3, expected);
  __ mov(expected, result);
  if (is_narrow) {
    __ decode_heap_oop(result, result);
    __ decode_heap_oop(tmp3, tmp3);
  }
  read_barrier_impl(masm, result);
  read_barrier_impl(masm, tmp3);
  __ cmp(result, tmp3);
  // Retry with expected now being the value we just loaded from addr.
  __ br(Assembler::EQ, retry);
  if (is_narrow && is_cae) {
    // For cmp-and-exchange and narrow oops, we need to restore
    // the compressed old-value. We moved it to 'expected' a few lines up.
    __ mov(result, expected);
  }
  __ bind(done);

}

#ifdef COMPILER1

#undef __
#define __ ce->masm()->

void ShenandoahBarrierSetAssembler::gen_pre_barrier_stub(LIR_Assembler* ce, ShenandoahPreBarrierStub* stub) {
  ShenandoahBarrierSetC1* bs = (ShenandoahBarrierSetC1*)BarrierSet::barrier_set()->barrier_set_c1();
  // At this point we know that marking is in progress.
  // If do_load() is true then we have to emit the
  // load of the previous value; otherwise it has already
  // been loaded into _pre_val.

  __ bind(*stub->entry());

  assert(stub->pre_val()->is_register(), "Precondition.");

  Register pre_val_reg = stub->pre_val()->as_register();

  if (stub->do_load()) {
    ce->mem2reg(stub->addr(), stub->pre_val(), T_OBJECT, stub->patch_code(), stub->info(), false /*wide*/, false /*unaligned*/);
  }
  __ cbz(pre_val_reg, *stub->continuation());
  ce->store_parameter(stub->pre_val()->as_register(), 0);
  __ far_call(RuntimeAddress(bs->pre_barrier_c1_runtime_code_blob()->code_begin()));
  __ b(*stub->continuation());
}

void ShenandoahBarrierSetAssembler::gen_write_barrier_stub(LIR_Assembler* ce, ShenandoahWriteBarrierStub* stub) {

  Register obj = stub->obj()->as_register();
  Register res = stub->result()->as_register();

  Label done;

  __ bind(*stub->entry());

  if (res != obj) {
    __ mov(res, obj);
  }
  // Check for null.
  if (stub->needs_null_check()) {
    __ cbz(res, done);
  }

  write_barrier(ce->masm(), res);

  __ bind(done);
  __ b(*stub->continuation());
}

#undef __

#define __ sasm->

void ShenandoahBarrierSetAssembler::generate_c1_pre_barrier_runtime_stub(StubAssembler* sasm) {
  __ prologue("shenandoah_pre_barrier", false);

  // arg0 : previous value of memory

  BarrierSet* bs = BarrierSet::barrier_set();

  const Register pre_val = r0;
  const Register thread = rthread;
  const Register tmp = rscratch1;

  Address queue_index(thread, in_bytes(ShenandoahThreadLocalData::satb_mark_queue_index_offset()));
  Address buffer(thread, in_bytes(ShenandoahThreadLocalData::satb_mark_queue_buffer_offset()));

  Label done;
  Label runtime;

  // Is marking still active?
  Address gc_state(thread, in_bytes(ShenandoahThreadLocalData::gc_state_offset()));
  __ ldrb(tmp, gc_state);
  __ mov(rscratch2, ShenandoahHeap::MARKING | ShenandoahHeap::TRAVERSAL);
  __ tst(tmp, rscratch2);
  __ br(Assembler::EQ, done);

  // Can we store original value in the thread's buffer?
  __ ldr(tmp, queue_index);
  __ cbz(tmp, runtime);

  __ sub(tmp, tmp, wordSize);
  __ str(tmp, queue_index);
  __ ldr(rscratch2, buffer);
  __ add(tmp, tmp, rscratch2);
  __ load_parameter(0, rscratch2);
  __ str(rscratch2, Address(tmp, 0));
  __ b(done);

  __ bind(runtime);
  __ push_call_clobbered_registers();
  __ load_parameter(0, pre_val);
  __ call_VM_leaf(CAST_FROM_FN_PTR(address, ShenandoahRuntime::write_ref_field_pre_entry), pre_val, thread);
  __ pop_call_clobbered_registers();
  __ bind(done);

  __ epilogue();
}

#undef __

#endif // COMPILER1

address ShenandoahBarrierSetAssembler::shenandoah_wb() {
  assert(_shenandoah_wb != NULL, "need write barrier stub");
  return _shenandoah_wb;
}

#define __ cgen->assembler()->

// Shenandoah write barrier.
//
// Input:
//   r0: OOP to evacuate.  Not null.
//
// Output:
//   r0: Pointer to evacuated OOP.
//
// Trash rscratch1, rscratch2.  Preserve everything else.
address ShenandoahBarrierSetAssembler::generate_shenandoah_wb(StubCodeGenerator* cgen) {

  __ align(6);
  StubCodeMark mark(cgen, "StubRoutines", "shenandoah_wb");
  address start = __ pc();

  Label work;
  __ mov(rscratch2, ShenandoahHeap::in_cset_fast_test_addr());
  __ lsr(rscratch1, r0, ShenandoahHeapRegion::region_size_bytes_shift_jint());
  __ ldrb(rscratch2, Address(rscratch2, rscratch1));
  __ tbnz(rscratch2, 0, work);
  __ ret(lr);
  __ bind(work);

  Register obj = r0;

  __ enter(); // required for proper stackwalking of RuntimeStub frame

  __ push_call_clobbered_registers();

  __ mov(lr, CAST_FROM_FN_PTR(address, ShenandoahRuntime::write_barrier_JRT));
  __ blrt(lr, 1, 0, MacroAssembler::ret_type_integral);
  __ mov(rscratch1, obj);
  __ pop_call_clobbered_registers();
  __ mov(obj, rscratch1);

  __ leave(); // required for proper stackwalking of RuntimeStub frame
  __ ret(lr);

  return start;
}

#undef __

void ShenandoahBarrierSetAssembler::barrier_stubs_init() {
  if (ShenandoahWriteBarrier || ShenandoahStoreValEnqueueBarrier) {
    int stub_code_size = 2048;
    ResourceMark rm;
    BufferBlob* bb = BufferBlob::create("shenandoah_barrier_stubs", stub_code_size);
    CodeBuffer buf(bb);
    StubCodeGenerator cgen(&buf);
    _shenandoah_wb = generate_shenandoah_wb(&cgen);
  }
}
