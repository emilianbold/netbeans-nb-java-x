/*
 * Copyright (c) 2002, 2020, Oracle and/or its affiliates. All rights reserved.
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
 *
 * @summary converted from VM Testbase gc/gctests/MTasyncGC.
 * VM Testbase keywords: [gc]
 * VM Testbase readme:
 * This test creates 1000 threads that run in a sequential
 * fashion with each thread in turn generating 1Meg of garbage.
 * The test relies upon the garbage collector asynchrnonously
 * reclaiming garbage.
 * The test fails if an OutOfMemoryError is thrown and passes
 * if the test proceeds to completion without an exception being
 * thrown.
 *
 * @library /vmTestbase
 *          /test/lib
 * @run driver jdk.test.lib.FileInstaller . .
 * @run main/othervm gc.gctests.MTasyncGC.MTasyncGC
 */

package gc.gctests.MTasyncGC;

import java.util.Vector;
import nsk.share.TestFailure;

// Each thread creates 1Meg of garbage in the run() method.

class MemEvil extends Thread {
    static Vector v = new Vector();
    static {
        for(int i = 0; i < 10; i++)
           v.addElement(new char [100000]);
    }

    public void run () {
        int i = 0;
        while(i < 10) {
           v.setElementAt(new char[100000], i);
           i++;
        }
   }
}


public class MTasyncGC {

    public static void main(String args[] ){
       int i;
       int memory_reserve[] = new int [10000];
       Thread threadsHolder[] = new Thread[1000];

       for(i = 0; i < threadsHolder.length; i++)
           threadsHolder[i] = new MemEvil();

       i = 0;
       while(i < threadsHolder.length ){
           try {
              threadsHolder[i].start();
              threadsHolder[i].join();
           } catch ( Exception  e ) {
                memory_reserve = null;
                System.gc();
                throw new TestFailure("Test Failed.", e);
           }
           threadsHolder[i] = null;
           i++;
       }
   }
}
