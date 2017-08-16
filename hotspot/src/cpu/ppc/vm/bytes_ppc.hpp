/*
 * Copyright (c) 1997, 2016, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2012, 2016 SAP SE. All rights reserved.
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

#ifndef CPU_PPC_VM_BYTES_PPC_HPP
#define CPU_PPC_VM_BYTES_PPC_HPP

#include "memory/allocation.hpp"

class Bytes: AllStatic {
 public:
  // Efficient reading and writing of unaligned unsigned data in platform-specific byte ordering
  // PowerPC needs to check for alignment.

  // Can I count on address always being a pointer to an unsigned char? Yes.

#if defined(VM_LITTLE_ENDIAN)

  // Forward declarations of the compiler-dependent implementation
  static inline u2 swap_u2(u2 x);
  static inline u4 swap_u4(u4 x);
  static inline u8 swap_u8(u8 x);

  static inline u2   get_native_u2(address p) {
    return (intptr_t(p) & 1) == 0
             ?   *(u2*)p
             :   ( u2(p[1]) << 8 )
               | ( u2(p[0])      );
  }

  static inline u4   get_native_u4(address p) {
    switch (intptr_t(p) & 3) {
     case 0:  return *(u4*)p;

     case 2:  return (  u4( ((u2*)p)[1] ) << 16  )
                   | (  u4( ((u2*)p)[0] )        );

    default:  return ( u4(p[3]) << 24 )
                   | ( u4(p[2]) << 16 )
                   | ( u4(p[1]) <<  8 )
                   |   u4(p[0]);
    }
  }

  static inline u8   get_native_u8(address p) {
    switch (intptr_t(p) & 7) {
      case 0:  return *(u8*)p;

      case 4:  return (  u8( ((u4*)p)[1] ) << 32  )
                    | (  u8( ((u4*)p)[0] )        );

      case 2:  return (  u8( ((u2*)p)[3] ) << 48  )
                    | (  u8( ((u2*)p)[2] ) << 32  )
                    | (  u8( ((u2*)p)[1] ) << 16  )
                    | (  u8( ((u2*)p)[0] )        );

     default:  return ( u8(p[7]) << 56 )
                    | ( u8(p[6]) << 48 )
                    | ( u8(p[5]) << 40 )
                    | ( u8(p[4]) << 32 )
                    | ( u8(p[3]) << 24 )
                    | ( u8(p[2]) << 16 )
                    | ( u8(p[1]) <<  8 )
                    |   u8(p[0]);
    }
  }



  static inline void put_native_u2(address p, u2 x) {
    if ( (intptr_t(p) & 1) == 0 )  *(u2*)p = x;
    else {
      p[1] = x >> 8;
      p[0] = x;
    }
  }

  static inline void put_native_u4(address p, u4 x) {
    switch ( intptr_t(p) & 3 ) {
    case 0:  *(u4*)p = x;
              break;

    case 2:  ((u2*)p)[1] = x >> 16;
             ((u2*)p)[0] = x;
             break;

    default: ((u1*)p)[3] = x >> 24;
             ((u1*)p)[2] = x >> 16;
             ((u1*)p)[1] = x >>  8;
             ((u1*)p)[0] = x;
             break;
    }
  }

  static inline void put_native_u8(address p, u8 x) {
    switch ( intptr_t(p) & 7 ) {
    case 0:  *(u8*)p = x;
             break;

    case 4:  ((u4*)p)[1] = x >> 32;
             ((u4*)p)[0] = x;
             break;

    case 2:  ((u2*)p)[3] = x >> 48;
             ((u2*)p)[2] = x >> 32;
             ((u2*)p)[1] = x >> 16;
             ((u2*)p)[0] = x;
             break;

    default: ((u1*)p)[7] = x >> 56;
             ((u1*)p)[6] = x >> 48;
             ((u1*)p)[5] = x >> 40;
             ((u1*)p)[4] = x >> 32;
             ((u1*)p)[3] = x >> 24;
             ((u1*)p)[2] = x >> 16;
             ((u1*)p)[1] = x >>  8;
             ((u1*)p)[0] = x;
    }
  }

  // Efficient reading and writing of unaligned unsigned data in Java byte ordering (i.e. big-endian ordering)
  // (no byte-order reversal is needed since Power CPUs are big-endian oriented).
  static inline u2   get_Java_u2(address p) { return swap_u2(get_native_u2(p)); }
  static inline u4   get_Java_u4(address p) { return swap_u4(get_native_u4(p)); }
  static inline u8   get_Java_u8(address p) { return swap_u8(get_native_u8(p)); }

  static inline void put_Java_u2(address p, u2 x)     { put_native_u2(p, swap_u2(x)); }
  static inline void put_Java_u4(address p, u4 x)     { put_native_u4(p, swap_u4(x)); }
  static inline void put_Java_u8(address p, u8 x)     { put_native_u8(p, swap_u8(x)); }

#else // !defined(VM_LITTLE_ENDIAN)

  // Thus, a swap between native and Java ordering is always a no-op:
  static inline u2   swap_u2(u2 x)  { return x; }
  static inline u4   swap_u4(u4 x)  { return x; }
  static inline u8   swap_u8(u8 x)  { return x; }

  static inline u2   get_native_u2(address p) {
    return (intptr_t(p) & 1) == 0
             ?   *(u2*)p
             :   ( u2(p[0]) << 8 )
               | ( u2(p[1])      );
  }

  static inline u4   get_native_u4(address p) {
    switch (intptr_t(p) & 3) {
     case 0:  return *(u4*)p;

     case 2:  return (  u4( ((u2*)p)[0] ) << 16  )
                   | (  u4( ((u2*)p)[1] )        );

    default:  return ( u4(p[0]) << 24 )
                   | ( u4(p[1]) << 16 )
                   | ( u4(p[2]) <<  8 )
                   |   u4(p[3]);
    }
  }

  static inline u8   get_native_u8(address p) {
    switch (intptr_t(p) & 7) {
      case 0:  return *(u8*)p;

      case 4:  return (  u8( ((u4*)p)[0] ) << 32  )
                    | (  u8( ((u4*)p)[1] )        );

      case 2:  return (  u8( ((u2*)p)[0] ) << 48  )
                    | (  u8( ((u2*)p)[1] ) << 32  )
                    | (  u8( ((u2*)p)[2] ) << 16  )
                    | (  u8( ((u2*)p)[3] )        );

     default:  return ( u8(p[0]) << 56 )
                    | ( u8(p[1]) << 48 )
                    | ( u8(p[2]) << 40 )
                    | ( u8(p[3]) << 32 )
                    | ( u8(p[4]) << 24 )
                    | ( u8(p[5]) << 16 )
                    | ( u8(p[6]) <<  8 )
                    |   u8(p[7]);
    }
  }



  static inline void put_native_u2(address p, u2 x) {
    if ( (intptr_t(p) & 1) == 0 ) { *(u2*)p = x; }
    else {
      p[0] = x >> 8;
      p[1] = x;
    }
  }

  static inline void put_native_u4(address p, u4 x) {
    switch ( intptr_t(p) & 3 ) {
    case 0:  *(u4*)p = x;
              break;

    case 2:  ((u2*)p)[0] = x >> 16;
             ((u2*)p)[1] = x;
             break;

    default: ((u1*)p)[0] = x >> 24;
             ((u1*)p)[1] = x >> 16;
             ((u1*)p)[2] = x >>  8;
             ((u1*)p)[3] = x;
             break;
    }
  }

  static inline void put_native_u8(address p, u8 x) {
    switch ( intptr_t(p) & 7 ) {
    case 0:  *(u8*)p = x;
             break;

    case 4:  ((u4*)p)[0] = x >> 32;
             ((u4*)p)[1] = x;
             break;

    case 2:  ((u2*)p)[0] = x >> 48;
             ((u2*)p)[1] = x >> 32;
             ((u2*)p)[2] = x >> 16;
             ((u2*)p)[3] = x;
             break;

    default: ((u1*)p)[0] = x >> 56;
             ((u1*)p)[1] = x >> 48;
             ((u1*)p)[2] = x >> 40;
             ((u1*)p)[3] = x >> 32;
             ((u1*)p)[4] = x >> 24;
             ((u1*)p)[5] = x >> 16;
             ((u1*)p)[6] = x >>  8;
             ((u1*)p)[7] = x;
    }
  }

  // Efficient reading and writing of unaligned unsigned data in Java byte ordering (i.e. big-endian ordering)
  // (no byte-order reversal is needed since Power CPUs are big-endian oriented).
  static inline u2   get_Java_u2(address p) { return get_native_u2(p); }
  static inline u4   get_Java_u4(address p) { return get_native_u4(p); }
  static inline u8   get_Java_u8(address p) { return get_native_u8(p); }

  static inline void put_Java_u2(address p, u2 x)     { put_native_u2(p, x); }
  static inline void put_Java_u4(address p, u4 x)     { put_native_u4(p, x); }
  static inline void put_Java_u8(address p, u8 x)     { put_native_u8(p, x); }

#endif // VM_LITTLE_ENDIAN
};

#include OS_CPU_HEADER_INLINE(bytes)

#endif // CPU_PPC_VM_BYTES_PPC_HPP
