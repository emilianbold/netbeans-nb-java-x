/*
 * Copyright (c) 2016, Oracle and/or its affiliates. All rights reserved.
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
 * @summary Test of method selection and resolution cases that
 * generate IncompatibleClassChangeError
 * @modules java.base/jdk.internal.org.objectweb.asm
 * @library /runtime/SelectionResolution/classes
 * @build selectionresolution.*
 * @run main/othervm/timeout=500 -XX:+IgnoreUnrecognizedVMOptions -XX:-VerifyDependencies InvokeInterfaceICCE
 */

import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import selectionresolution.ClassData;
import selectionresolution.MethodData;
import selectionresolution.Result;
import selectionresolution.SelectionResolutionTest;
import selectionresolution.SelectionResolutionTestCase;
import selectionresolution.Template;

public class InvokeInterfaceICCE extends SelectionResolutionTest {

    private static final SelectionResolutionTestCase.Builder initBuilder =
        new SelectionResolutionTestCase.Builder();

    static {
        initBuilder.setResult(Result.ICCE);
    }

    private static final Collection<TestGroup> testgroups =
        Arrays.asList(
                /* invokeinterface tests */

                /* resolved method is static*/
                /* Group 168: methodref = expected */
                new TestGroup.Simple(initBuilder,
                        Template.SetInvoke(SelectionResolutionTestCase.InvokeInstruction.INVOKEINTERFACE),
                        Template.ResultCombo(EnumSet.of(Template.Kind.INTERFACE),
                                             EnumSet.of(MethodData.Access.PUBLIC),
                                             EnumSet.of(MethodData.Context.STATIC),
                                             EnumSet.of(ClassData.Package.SAME,
                                                        ClassData.Package.DIFFERENT)),
                        Template.OverrideAbstractExpectedIface,
                        Template.MethodrefEqualsExpected,
                        Template.IgnoredAbstract,
                        Template.AllCallsiteCases,
                        Template.IfaceMethodrefSelection),
                /* Group 169: methodref != expected */
                new TestGroup.Simple(initBuilder,
                        Template.SetInvoke(SelectionResolutionTestCase.InvokeInstruction.INVOKEINTERFACE),
                        Template.ResultCombo(EnumSet.of(Template.Kind.INTERFACE),
                                             EnumSet.of(MethodData.Access.PUBLIC),
                                             EnumSet.of(MethodData.Context.STATIC),
                                             EnumSet.of(ClassData.Package.SAME,
                                                        ClassData.Package.DIFFERENT)),
                        Template.OverrideAbstractExpectedIface,
                        Template.IfaceMethodrefNotEqualsExpected,
                        Template.IgnoredAbstract,
                        Template.AllCallsiteCases,
                        Template.IfaceMethodrefSelection),

                /* methodref is a class */
                /* Group 170: methodref = expected */
                new TestGroup.Simple(initBuilder,
                        Template.SetInvoke(SelectionResolutionTestCase.InvokeInstruction.INVOKEINTERFACE),
                        Template.ResultCombo(EnumSet.of(Template.Kind.CLASS),
                                             EnumSet.of(MethodData.Access.PUBLIC,
                                                        MethodData.Access.PACKAGE,
                                                        MethodData.Access.PROTECTED,
                                                        MethodData.Access.PRIVATE),
                                             EnumSet.of(MethodData.Context.STATIC),
                                             EnumSet.of(ClassData.Package.SAME,
                                                        ClassData.Package.DIFFERENT)),
                        Template.OverrideAbstractExpectedClass,
                        Template.MethodrefEqualsExpected,
                        Template.IgnoredAbstract,
                        Template.AllCallsiteCases,
                        Template.MethodrefSelectionResolvedIsClass),
                /* Group 171: methodref != expected, expected is class */
                new TestGroup.Simple(initBuilder,
                        Template.SetInvoke(SelectionResolutionTestCase.InvokeInstruction.INVOKEINTERFACE),
                        Template.ResultCombo(EnumSet.of(Template.Kind.CLASS),
                                             EnumSet.of(MethodData.Access.PUBLIC,
                                                        MethodData.Access.PACKAGE,
                                                        MethodData.Access.PROTECTED,
                                                        MethodData.Access.PRIVATE),
                                             EnumSet.of(MethodData.Context.STATIC),
                                             EnumSet.of(ClassData.Package.SAME,
                                                        ClassData.Package.DIFFERENT)),
                        Template.OverrideAbstractExpectedClass,
                        Template.MethodrefNotEqualsExpectedClass,
                        Template.IgnoredAbstract,
                        Template.AllCallsiteCases,
                        Template.MethodrefSelectionResolvedIsClass),
                /* Group 172: methodref != expected expected is interface */
                new TestGroup.Simple(initBuilder,
                        Template.SetInvoke(SelectionResolutionTestCase.InvokeInstruction.INVOKEINTERFACE),
                        Template.ResultCombo(EnumSet.of(Template.Kind.INTERFACE),
                                             EnumSet.of(MethodData.Access.PUBLIC,
                                                        MethodData.Access.PRIVATE),
                                             EnumSet.of(MethodData.Context.STATIC),
                                             EnumSet.of(ClassData.Package.SAME,
                                                        ClassData.Package.DIFFERENT)),
                        Template.OverrideAbstractExpectedIface,
                        Template.MethodrefNotEqualsExpectedIface,
                        Template.IgnoredAbstract,
                        Template.AllCallsiteCases,
                        Template.MethodrefSelectionResolvedIsIface),
                /* Group 173: ambiguous resolution */
                new TestGroup.Simple(initBuilder,
                        Template.SetInvoke(SelectionResolutionTestCase.InvokeInstruction.INVOKEINTERFACE),
                        Template.ResultCombo(EnumSet.of(Template.Kind.INTERFACE),
                                             EnumSet.of(MethodData.Access.PUBLIC),
                                             EnumSet.of(MethodData.Context.INSTANCE),
                                             EnumSet.of(ClassData.Package.SAME,
                                                        ClassData.Package.DIFFERENT)),
                        Template.OverrideAbstractExpectedIface,
                        Template.IfaceMethodrefAmbiguous,
                        Template.IgnoredAbstract,
                        Template.AllCallsiteCases,
                        Template.IfaceMethodrefSelectionNoOverride),
                /* Group 174: ambiguous selection */
                new TestGroup.Simple(initBuilder,
                        Template.SetInvoke(SelectionResolutionTestCase.InvokeInstruction.INVOKEINTERFACE),
                        Template.ResultCombo(EnumSet.of(Template.Kind.INTERFACE),
                                             EnumSet.of(MethodData.Access.PUBLIC),
                                             EnumSet.of(MethodData.Context.INSTANCE),
                                             EnumSet.of(ClassData.Package.SAME,
                                                        ClassData.Package.DIFFERENT)),
                        Template.OverrideAbstractExpectedIface,
                        Template.IfaceMethodrefNotEqualsExpected,
                        Template.IgnoredAbstract,
                        Template.AllCallsiteCases,
                        Template.IfaceMethodrefAmbiguousResolvedIsIface),

                /* Group 175: private method in interface */
                new TestGroup.Simple(initBuilder,
                        Template.SetInvoke(SelectionResolutionTestCase.InvokeInstruction.INVOKEINTERFACE),
                        Template.ResultCombo(EnumSet.of(Template.Kind.INTERFACE),
                                             EnumSet.of(MethodData.Access.PRIVATE),
                                             EnumSet.of(MethodData.Context.INSTANCE),
                                             EnumSet.of(ClassData.Package.SAME)),
                        Template.OverrideAbstractExpectedIface,
                        Template.MethodrefEqualsExpected,
                        Template.IgnoredAbstract,
                        Template.CallsiteEqualsMethodref,
                        Template.IfaceMethodrefSelection)
            );

    private InvokeInterfaceICCE() {
        super(testgroups);
    }

    public static void main(final String... args) {
        new InvokeInterfaceICCE().run();
    }
}
