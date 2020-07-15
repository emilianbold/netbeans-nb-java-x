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
 * @summary converted from VM Testbase nsk/jvmti/RedefineClasses/redefclass009.
 * VM Testbase keywords: [quick, jpda, jvmti, noras, redefine]
 * VM Testbase readme:
 * DESCRIPTION
 *     This test checks that the JVMTI function RedefineClasses()
 *     updates the attribute LocalVariableTable of the redefined class.
 *     Note, that all tested classes should be compiled with debugging info.
 *     The test creates a dummy instance of tested class redefclass009r.
 *     Then the test redefines this class. Bytes of new version of the class
 *     redefclass009r are taken from the directory ./newclass_g.
 *     Finally, the test checks that LocalVariableTable, one of the two
 *     attributes, accessible in JVMTI (LineNumberTable and LocalVariableTable),
 *     was updated after the redefinition.
 * COMMENTS
 *     The test was fixed due to the following bug:
 *     4762721 TEST_BUG: Some tests for RedefineClasses erroneously change
 *             ctor modifier
 *     Ported from JVMDI.
 *
 * @library /vmTestbase
 *          /test/lib
 * @build nsk.jvmti.RedefineClasses.redefclass009
 *
 * @comment make sure redefclass009r is compiled with full debug info
 * @build nsk.jvmti.RedefineClasses.redefclass009r
 * @clean nsk.jvmti.RedefineClasses.redefclass009r
 * @compile -g:lines,source,vars ../redefclass009r.java
 *
 * @comment compile newclassXX to bin/newclassXX
 *          with full debug info
 * @run driver nsk.share.ExtraClassesBuilder
 *      -g:lines,source,vars
 *      newclass_g
 *
 * @run main/othervm/native -agentlib:redefclass009 nsk.jvmti.RedefineClasses.redefclass009 ./bin
 */

