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


/*
 * @test
 *
 * @summary converted from VM Testbase nsk/jvmti/Agent_OnLoad/agentonload001.
 * VM Testbase keywords: [quick, jpda, jvmti, noras]
 * VM Testbase readme:
 * DESCRIPTION
 *     This JVMTI test exercises JVMTI thread function Agent_OnLoad().
 *     This test checks that Agent_OnLoad() is invoked on startup,
 *     and not NULL values are passed for 'vm' and 'options' parameters.
 *     This test does not create JVMTI environment.
 *     The test uses native method checkLoadStatus() to check value of internal
 *     native variable 'status' set by JVM_OnLoad().
 *     If JVM_OnLoad() was not invoked on startup, then checkLoadStatus()
 *     returns FAILED and test fails with exit status 97.
 *     If JVM_OnLoad() was invoked but NULL values were passed for any
 *     of 'vm' or 'options' parameters, then JVM_OnLoad() returns with
 *     JNI_ERR and the test fails with exit status 1,
 *     Otherwise, the test passes with exit status 95.
 * COMMENTS
 *
 * @library /vmTestbase
 *          /test/lib
 * @run main/othervm/native
 *      -agentlib:agentonload001=-waittime=5
 *      nsk.jvmti.Agent_OnLoad.agentonload001
 */

