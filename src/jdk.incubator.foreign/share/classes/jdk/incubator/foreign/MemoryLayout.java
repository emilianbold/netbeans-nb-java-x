/*
 *  Copyright (c) 2019, Oracle and/or its affiliates. All rights reserved.
 *  DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 *  This code is free software; you can redistribute it and/or modify it
 *  under the terms of the GNU General Public License version 2 only, as
 *  published by the Free Software Foundation.  Oracle designates this
 *  particular file as subject to the "Classpath" exception as provided
 *  by Oracle in the LICENSE file that accompanied this code.
 *
 *  This code is distributed in the hope that it will be useful, but WITHOUT
 *  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 *  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 *  version 2 for more details (a copy is included in the LICENSE file that
 *  accompanied this code).
 *
 *  You should have received a copy of the GNU General Public License version
 *  2 along with this work; if not, write to the Free Software Foundation,
 *  Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 *   Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 *  or visit www.oracle.com if you need additional information or have any
 *  questions.
 *
 */
package jdk.incubator.foreign;

import jdk.internal.foreign.LayoutPath;
import jdk.internal.foreign.Utils;

import java.lang.constant.Constable;
import java.lang.constant.DynamicConstantDesc;
import java.lang.invoke.VarHandle;
import java.nio.ByteOrder;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.OptionalLong;

/**
 * A memory layout can be used to describe the contents of a memory segment in a <em>language neutral</em> fashion.
 * There are two leaves in the layout hierarchy, <em>value layouts</em>, which are used to represent values of given size and kind (see
 * {@link ValueLayout}) and <em>padding layouts</em> which are used, as the name suggests, to represent a portion of a memory
 * segment whose contents should be ignored, and which are primarily present for alignment reasons (see {@link MemoryLayout#ofPaddingBits(long)}).
 * Some common value layout constants are defined in the {@link MemoryLayouts} class.
 * <p>
 * More complex layouts can be derived from simpler ones: a <em>sequence layout</em> denotes a repetition of one or more
 * element layout (see {@link SequenceLayout}); a <em>group layout</em> denotes an aggregation of (typically) heterogeneous
 * member layouts (see {@link GroupLayout}).
 * <p>
 * All implementations of this interface must be <a href="{@docRoot}/java.base/java/lang/doc-files/ValueBased.html">value-based</a>;
 * use of identity-sensitive operations (including reference equality ({@code ==}), identity hash code, or synchronization) on
 * instances of {@code MemoryLayout} may have unpredictable results and should be avoided. The {@code equals} method should
 * be used for comparisons.
 * <p>
 * Non-platform classes should not implement {@linkplain MemoryLayout} directly.
 *
 * <h2><a id = "layout-align">Size, alignment and byte order</a></h2>
 *
 * All layouts have a size; layout size for value and padding layouts is always explicitly denoted; this means that a layout description
 * always has the same size in bits, regardless of the platform in which it is used. For derived layouts, the size is computed
 * as follows:
 * <ul>
 *     <li>for a <em>finite</em> sequence layout <em>S</em> whose element layout is <em>E</em> and size is L,
 *     the size of <em>S</em> is that of <em>E</em>, multiplied by <em>L</em></li>
 *     <li>the size of an <em>unbounded</em> sequence layout is <em>unknown</em></li>
 *     <li>for a group layout <em>G</em> with member layouts <em>M1</em>, <em>M2</em>, ... <em>Mn</em> whose sizes are
 *     <em>S1</em>, <em>S2</em>, ... <em>Sn</em>, respectively, the size of <em>G</em> is either <em>S1 + S2 + ... + Sn</em> or
 *     <em>max(S1, S2, ... Sn)</em> depending on whether the group is a <em>struct</em> or an <em>union</em>, respectively</li>
 * </ul>
 * <p>
 * Furthermore, all layouts feature a <em>natural alignment</em> which can be inferred as follows:
 * <ul>
 *     <li>for value and padding layout <em>L</em> whose size is <em>N</em>, the natural alignment of <em>L</em> is <em>N</em></li>
 *     <li>for a sequence layout <em>S</em> whose element layout is <em>E</em>, the natural alignment of <em>S</em> is that of <em>E</em></li>
 *     <li>for a group layout <em>G</em> with member layouts <em>M1</em>, <em>M2</em>, ... <em>Mn</em> whose alignments are
 *     <em>A1</em>, <em>A2</em>, ... <em>An</em>, respectively, the natural alignment of <em>G</em> is <em>max(A1, A2 ... An)</em></li>
 * </ul>
 * A layout's natural alignment can be overridden if needed (see {@link MemoryLayout#withBitAlignment(long)}), which can be useful to describe
 * hyper-aligned layouts.
 * <p>
 * All value layouts have an <em>explicit</em> byte order (see {@link java.nio.ByteOrder}) which is set when the layout is created.
 *
 * <h2><a id = "layout-paths">Layout paths</a></h2>
 *
 * A <em>layout path</em> originates from a <em>root</em> layout (typically a group or a sequence layout) and terminates
 * at a layout nested within the root layout - this is the layout <em>selected</em> by the layout path.
 * Layout paths are typically expressed as a sequence of one or more {@link PathElement} instances.
 * <p>
 * Layout paths are useful in order to e.g. to obtain offset of leaf elements inside arbitrarily nested layouts
 * (see {@link MemoryLayout#offset(PathElement...)}), or to quickly obtain a memory access handle corresponding to the selected
 * layout (see {@link MemoryLayout#varHandle(Class, PathElement...)}).
 * <p>
 * Such <em>layout paths</em> can be constructed programmatically using the methods in this class.
 * For instance, given a layout constructed as follows:
 * <blockquote><pre>{@code
SequenceLayout seq = MemoryLayout.ofSequence(5,
    MemoryLayout.ofStruct(
        MemoryLayout.ofPaddingBits(32),
        MemoryLayout.ofValueBits(32, ByteOrder.BIG_ENDIAN).withName("value")
));
 * }</pre></blockquote>
 *
 * We can obtain the offset of the member layout named <code>value</code> from <code>seq</code>, as follows:
 * <blockquote><pre>{@code
long valueOffset = seq.offset(PathElement.sequenceElement(), PathElement.groupElement("value"));
 * }</pre></blockquote>
 *
 * Layout paths can feature one or more <em>free dimensions</em>. For instance, a layout path traversing
 * an unspecified sequence element (that is, where one of the path component was obtained with the
 * {@link PathElement#sequenceElement()} method) features an additional free dimension, which will have to be bound at runtime;
 * that is, the memory access var handle associated with such a layout path expression will feature an extra {@code long}
 * access coordinate. The layout path constructed in the above example features exactly one free dimension.
 *
 * @apiNote In the future, if the Java language permits, {@link MemoryLayout}
 * may become a {@code sealed} interface, which would prohibit subclassing except by
 * explicitly permitted types.
 *
 * @implSpec
 * Implementations of this class are immutable and thread-safe.
 */
public interface MemoryLayout extends Constable {

    /**
     * Returns an {@link Optional} containing the nominal descriptor for this
     * layout, if one can be constructed, or an empty {@link Optional}
     * if one cannot be constructed.
     *
     * @return An {@link Optional} containing the resulting nominal descriptor,
     * or an empty {@link Optional} if one cannot be constructed.
     */
    @Override
    Optional<? extends DynamicConstantDesc<? extends MemoryLayout>> describeConstable();

    /**
     * Computes the layout size, in bits.
     *
     * @return the layout size, in bits.
     * @throws UnsupportedOperationException if the layout is, or contains, a sequence layout with unspecified size (see {@link SequenceLayout}).
     */
    long bitSize();

    /**
     * Computes the layout size, in bytes.
     *
     * @return the layout size, in bytes.
     * @throws UnsupportedOperationException if the layout is, or contains, a sequence layout with unspecified size (see {@link SequenceLayout}),
     * or if {@code bitSize()} is not a multiple of 8.
     */
    default long byteSize() {
        return Utils.bitsToBytesOrThrow(bitSize(),
                () -> new UnsupportedOperationException("Cannot compute byte size; bit size is not a multiple of 8"));
    }

    /**
     * Return the <em>name</em> (if any) associated with this layout.
     *
     * @return the layout <em>name</em> (if any).
     * @see MemoryLayout#withName(String)
     */
    Optional<String> name();

    /**
     * Creates a new layout which features the desired layout <em>name</em>.
     *
     * @param name the layout name.
     * @return a new layout which is the same as this layout, except for the <em>name</em> associated to it.
     * @see MemoryLayout#name()
     */
    MemoryLayout withName(String name);

    /**
     * Returns the alignment constraint associated with this layout, expressed in bits. Layout alignment defines a power
     * of two {@code A} which is the bit-wise alignment of the layout. If {@code A <= 8} then {@code A/8} is the number of
     * bytes that must be aligned for any pointer that correctly points to this layout. Thus:
     *
     * <ul>
     * <li>{@code A=8} means unaligned (in the usual sense), which is common in packets.</li>
     * <li>{@code A=64} means word aligned (on LP64), {@code A=32} int aligned, {@code A=16} short aligned, etc.</li>
     * <li>{@code A=512} is the most strict alignment required by the x86/SV ABI (for AVX-512 data).</li>
     * </ul>
     *
     * If no explicit alignment constraint was set on this layout (see {@link #withBitAlignment(long)}),
     * then this method returns the <a href="#layout-align">natural alignment</a> constraint (in bits) associated with this layout.
     *
     * @return the layout alignment constraint, in bits.
     */
    long bitAlignment();

    /**
     * Returns the alignment constraint associated with this layout, expressed in bytes. Layout alignment defines a power
     * of two {@code A} which is the byte-wise alignment of the layout, where {@code A} is the number of bytes that must be aligned
     * for any pointer that correctly points to this layout. Thus:
     *
     * <ul>
     * <li>{@code A=1} means unaligned (in the usual sense), which is common in packets.</li>
     * <li>{@code A=8} means word aligned (on LP64), {@code A=4} int aligned, {@code A=2} short aligned, etc.</li>
     * <li>{@code A=64} is the most strict alignment required by the x86/SV ABI (for AVX-512 data).</li>
     * </ul>
     *
     * If no explicit alignment constraint was set on this layout (see {@link #withBitAlignment(long)}),
     * then this method returns the <a href="#layout-align">natural alignment</a> constraint (in bytes) associated with this layout.
     *
     * @return the layout alignment constraint, in bytes.
     * @throws UnsupportedOperationException if {@code bitAlignment()} is not a multiple of 8.
     */
    default long byteAlignment() {
        return Utils.bitsToBytesOrThrow(bitAlignment(),
                () -> new UnsupportedOperationException("Cannot compute byte alignment; bit alignment is not a multiple of 8"));
    }

    /**
     * Creates a new layout which features the desired alignment constraint.
     *
     * @param bitAlignment the layout alignment constraint, expressed in bits.
     * @return a new layout which is the same as this layout, except for the alignment constraint associated to it.
     * @throws IllegalArgumentException if {@code bitAlignment} is not a power of two, or if it's less than than 8.
     */
    MemoryLayout withBitAlignment(long bitAlignment);

    /**
     * Computes the offset of the layout selected by a given layout path, where the path is considered rooted in this
     * layout.
     *
     * @apiNote if the layout path has one (or more) free dimensions,
     * the offset is computed as if all the indices corresponding to such dimensions were set to {@code 0}.
     *
     * @param elements the layout path elements.
     * @return The offset of layout selected by a the layout path obtained by concatenating the path elements in {@code elements}.
     * @throws IllegalArgumentException if the layout path obtained by concatenating the path elements in {@code elements}
     * does not select a valid layout element.
     */
    default long offset(PathElement... elements) {
        LayoutPath path = LayoutPath.rootPath(this);
        for (PathElement e : elements) {
            path = ((LayoutPath.PathElementImpl)e).apply(path);
        }
        return path.offset();
    }

    /**
     * Creates a memory access var handle that can be used to dereference memory at the layout selected by a given layout path,
     * where the path is considered rooted in this layout.
     *
     * @apiNote the resulting var handle will feature an additional {@code long} access coordinate for every
     * unspecified sequence access component contained in this layout path. Moreover, the resulting var handle
     * features certain <a href="MemoryHandles.html#memaccess-mode">access mode restrictions</a>, which are common to all memory access var handles.
     *
     * @param carrier the var handle carrier type.
     * @param elements the layout path elements.
     * @return a var handle which can be used to dereference memory at the layout denoted by given layout path.
     * @throws UnsupportedOperationException if the layout path has one or more elements with incompatible alignment constraints.
     * @throws IllegalArgumentException if the carrier does not represent a primitive type, if the carrier is {@code void},
     * {@code boolean}, or if the layout path obtained by concatenating the path elements in {@code elements}
     * does not select a value layout (see {@link ValueLayout}), or if the selected value layout has a size that
     * that does not match that of the specified carrier type.
     */
    default VarHandle varHandle(Class<?> carrier, PathElement... elements) {
        LayoutPath path = LayoutPath.rootPath(this);
        for (PathElement e : elements) {
            path = ((LayoutPath.PathElementImpl)e).apply(path);
        }
        return path.dereferenceHandle(carrier);
    }

    /**
     * Instances of this class are used to form <a href="MemoryLayout.html#layout-paths"><em>layout paths</em></a>. There
     * are two kinds of path elements: <em>group path elements</em> and <em>sequence path elements</em>. Group
     * path elements are used to select a given named member layout within a {@link GroupLayout}. Sequence
     * path elements are used to select a sequence element layout within a {@link SequenceLayout}; selection
     * of sequence element layout can be <em>explicit</em> (see {@link PathElement#sequenceElement(long)}) or
     * <em>implicit</em> (see {@link PathElement#sequenceElement()}). When a path uses one or more implicit
     * sequence path elements, it acquires additional <em>free dimensions</em>.
     * <p>
     * Non-platform classes should not implement {@linkplain PathElement} directly.
     *
     * @apiNote In the future, if the Java language permits, {@link PathElement}
     * may become a {@code sealed} interface, which would prohibit subclassing except by
     * explicitly permitted types.
     *
     * @implSpec
     * Implementations of this interface are immutable and thread-safe.
     */
    interface PathElement {

        /**
         * Returns a path element which selects a member layout with given name from a given group layout.
         * The path element returned by this method does not alter the number of free dimensions of any path
         * that is combined with such element.
         *
         * @implSpec in case multiple group elements with a matching name exist, the path element returned by this
         * method will select the first one; that is, the group element with lowest offset from current path is selected.
         *
         * @param name the name of the group element to be selected.
         * @return a path element which selects the group element with given name.
         * @throws NullPointerException if the specified group element name is {@code null}.
         */
        static PathElement groupElement(String name) {
            Objects.requireNonNull(name);
            return new LayoutPath.PathElementImpl(path -> path.groupElement(name));
        }

        /**
         * Returns a path element which selects the element layout at the specified position in a given the sequence layout.
         * The path element returned by this method does not alter the number of free dimensions of any path
         * that is combined with such element.
         *
         * @param index the index of the sequence element to be selected.
         * @return a path element which selects the sequence element layout with given index.
         * @throws IllegalArgumentException if {@code index < 0}.
         */
        static PathElement sequenceElement(long index) {
            if (index < 0) {
                throw new IllegalArgumentException("Index must be positive: " + index);
            }
            return new LayoutPath.PathElementImpl(path -> path.sequenceElement(index));
        }

        /**
         * Returns a path element which selects the element layout in a <em>range</em> of positions in a given the sequence layout,
         * where the range is expressed as a pair of starting index (inclusive) {@code S} and step factor (which can also be negative)
         * {@code F}.
         * If a path with free dimensions {@code n} is combined with the path element returned by this method,
         * the number of free dimensions of the resulting path will be {@code 1 + n}. If the free dimension associated
         * with this path is bound by an index {@code I}, the resulting accessed offset can be obtained with the following
         * formula:
         * <blockquote><pre>{@code
E * (S + I * F)
         * }</pre></blockquote>
         * where {@code E} is the size (in bytes) of the sequence element layout.
         *
         * @param start the index of the first sequence element to be selected.
         * @param step the step factor at which subsequence sequence elements are to be selected.
         * @return a path element which selects the sequence element layout with given index.
         * @throws IllegalArgumentException if {@code start < 0}, or {@code step == 0}.
         */
        static PathElement sequenceElement(long start, long step) {
            if (start < 0) {
                throw new IllegalArgumentException("Start index must be positive: " + start);
            }
            if (step == 0) {
                throw new IllegalArgumentException("Step must be != 0: " + step);
            }
            return new LayoutPath.PathElementImpl(path -> path.sequenceElement(start, step));
        }

        /**
         * Returns a path element which selects an unspecified element layout from a given sequence layout.
         * If a path with free dimensions {@code n} is combined with the path element returned by this method,
         * the number of free dimensions of the resulting path will be {@code 1 + n}.
         *
         * @return a path element which selects an unspecified sequence element layout.
         */
        static PathElement sequenceElement() {
            return new LayoutPath.PathElementImpl(LayoutPath::sequenceElement);
        }
    }

    /**
     * Compares the specified object with this layout for equality. Returns {@code true} if and only if the specified
     * object is also a layout, and it is equal to this layout. Two layouts are considered equal if they are of
     * the same kind, have the same size, name and alignment constraints. Furthermore, depending on the layout kind, additional
     * conditions must be satisfied:
     * <ul>
     *     <li>two value layouts are considered equal if they have the same endianness (see {@link ValueLayout#order()})</li>
     *     <li>two sequence layouts are considered equal if they have the same element count (see {@link SequenceLayout#elementCount()}), and
     *     if their element layouts (see {@link SequenceLayout#elementLayout()}) are also equal</li>
     *     <li>two group layouts are considered equal if they are of the same kind (see {@link GroupLayout#isStruct()},
     *     {@link GroupLayout#isUnion()}) and if their member layouts (see {@link GroupLayout#memberLayouts()}) are also equal</li>
     * </ul>
     *
     * @param that the object to be compared for equality with this layout.
     * @return {@code true} if the specified object is equal to this layout.
     */
    boolean equals(Object that);

    /**
     * Returns the hash code value for this layout.
     *
     * @return the hash code value for this layout.
     */
    int hashCode();

    /**
     * Returns a string representation of this layout.
     *
     * @return a string representation of this layout.
     */
    @Override
    String toString();

    /**
     * Create a new padding layout with given size.
     *
     * @param size the padding size in bits.
     * @return the new selector layout.
     * @throws IllegalArgumentException if {@code size <= 0}.
     */
    static MemoryLayout ofPaddingBits(long size) {
        AbstractLayout.checkSize(size);
        return new PaddingLayout(size);
    }

    /**
     * Create a value layout of given byte order and size.
     *
     * @param size the value layout size.
     * @param order the value layout's byte order.
     * @return a new value layout.
     * @throws IllegalArgumentException if {@code size <= 0}.
     */
    static ValueLayout ofValueBits(long size, ByteOrder order) {
        AbstractLayout.checkSize(size);
        return new ValueLayout(order, size);
    }

    /**
     * Create a new sequence layout with given element layout and element count.
     *
     * @param elementCount the sequence element count.
     * @param elementLayout the sequence element layout.
     * @return the new sequence layout with given element layout and size.
     * @throws IllegalArgumentException if {@code elementCount < 0}.
     */
    static SequenceLayout ofSequence(long elementCount, MemoryLayout elementLayout) {
        AbstractLayout.checkSize(elementCount, true);
        OptionalLong size = OptionalLong.of(elementCount);
        return new SequenceLayout(size, elementLayout);
    }

    /**
     * Create a new sequence layout, with unbounded element count and given element layout.
     *
     * @param elementLayout the element layout of the sequence layout.
     * @return the new sequence layout with given element layout.
     */
    static SequenceLayout ofSequence(MemoryLayout elementLayout) {
        return new SequenceLayout(OptionalLong.empty(), elementLayout);
    }

    /**
     * Create a new <em>struct</em> group layout with given member layouts.
     *
     * @param elements The member layouts of the <em>struct</em> group layout.
     * @return a new <em>struct</em> group layout with given member layouts.
     */
    static GroupLayout ofStruct(MemoryLayout... elements) {
        return new GroupLayout(GroupLayout.Kind.STRUCT, List.of(elements));
    }

    /**
     * Create a new <em>union</em> group layout with given member layouts.
     *
     * @param elements The member layouts of the <em>union</em> layout.
     * @return a new <em>union</em> group layout with given member layouts.
     */
    static GroupLayout ofUnion(MemoryLayout... elements) {
        return new GroupLayout(GroupLayout.Kind.UNION, List.of(elements));
    }
}
