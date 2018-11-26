/*
 * Copyright (c) 2014 Oracle and/or its affiliates. All rights reserved.
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
package org.openjdk.bench.java.util.concurrent;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Param;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.Setup;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.annotations.TearDown;
import org.openjdk.jmh.infra.Blackhole;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.PriorityBlockingQueue;
import java.util.concurrent.TimeUnit;

/**
 * Tests the different blocking queues in the java.util.concurrent package.
 * The tests are done with a single producer and a variable number of consumers.
 * The tests are created from Doug Lea's concurrent test suite.
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Benchmark)
public class ProducerConsumer {

    @Param("100")
    private int capacity;

    @Param
    private QueueType type;

    private BlockingQueue<Integer> q;
    private Producer prod;

    @Setup
    public void prepare() {
        switch (type) {
            case ABQ_F:
                q = new ArrayBlockingQueue<>(capacity, true);
                break;
            case ABQ_NF:
                q = new ArrayBlockingQueue<>(capacity, false);
                break;
            case LBQ:
                q = new LinkedBlockingQueue<>(capacity);
                break;
            case PBQ:
                q = new PriorityBlockingQueue<>(capacity);
                break;
            default:
                throw new RuntimeException();
        }

        prod = new Producer(q);
        prod.start();
    }

    @TearDown
    public void teardown() {
        prod.halt();
    }

    @Benchmark
    public void test() {
        try {
            int last = -1;
            int v = q.take();
            if (v < last) {
                throw new Error("Out-of-Order transfer");
            }
            Blackhole.consumeCPU(10);
        } catch (Exception ie) {
        }
    }

    public enum QueueType {
        LBQ,
        ABQ_NF,
        ABQ_F,
        PBQ,
    }

    private class Producer extends Thread {
        private final BlockingQueue<Integer> queue;
        private int i = 0;
        private volatile boolean running;

        public Producer(BlockingQueue<Integer> queue) {
            this.queue = queue;
        }

        @Override
        public void run() {
            running = true;
            try {
                while (running) {
                    queue.put(i++);
                }
            } catch (Exception ie) {
            }
        }

        public void halt() {
            running = false;
            this.interrupt();
        }
    }
}
