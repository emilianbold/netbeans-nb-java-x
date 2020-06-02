/*
 * Copyright (c) 2017, 2020, Oracle and/or its affiliates. All rights reserved.
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


/*
 * @test
 * @key stress gc randomness
 *
 * @summary converted from VM Testbase gc/gctests/WeakReference/weak002.
 * VM Testbase keywords: [gc, stress, stressopt, nonconcurrent]
 * VM Testbase readme:
 * DESCRIPTION
 *     The test checks that Garbage Collector correctly works with
 *     WeakReferences. It also checks that no unexpected exceptions and errors
 *     are thrown or the JVM is not crashed.
 *     The test starts a number of threads. Each thread run tests for some time
 *     or serveral iterations.  See javadoc StressOptions for configuration.
 *     First of all each test defines what type to check (there are 10 types
 *     totally). As soon as the type is defined, a WeakReference is created that
 *     refers to an array of tested type and is registered with in a queue. A
 *     WeakReference for NonbranchyTree class does not refer to an array, but to
 *     instances of the class.
 *     After that a thread performs next checks for the reference:
 *         1. The reference is in queue after GC is provoked with
 *            Algorithms.eatMemory() method (a single thread eats the memory).
 *         2. queue.remove() returns reference from the queue.
 *         3. queue.poll() returns null.
 *         4. reference.clear() does not throw any exception.
 *     The test extends ThreadedGCTest and implements GarbageProducerAware and
 *     MemoryStrategyAware interfaces. The corresponding javadoc documentation
 *     for additional test configuration.
 *
 * @library /vmTestbase
 *          /test/lib
 * @run driver jdk.test.lib.FileInstaller . .
 * @run main/othervm gc.gctests.WeakReference.weak001.weak001 -ms high
 */

