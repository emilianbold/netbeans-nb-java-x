/*
 * Copyright (c) 2020, Oracle and/or its affiliates. All rights reserved.
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
 * @bug 8242293
 * @summary allow for local interfaces and enums plus nested records, interfaces and enums
 * @library /tools/javac/lib
 * @modules jdk.compiler/com.sun.tools.javac.api
 *          jdk.compiler/com.sun.tools.javac.file
 *          jdk.compiler/com.sun.tools.javac.util
 * @build combo.ComboTestHelper
 * @compile --enable-preview -source ${jdk.version} LocalStaticDeclarations.java
 * @run main/othervm --enable-preview LocalStaticDeclarations
 */

import javax.lang.model.element.Element;
import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;

import com.sun.tools.javac.util.Assert;

import com.sun.tools.javac.api.ClientCodeWrapper;
import com.sun.tools.javac.util.JCDiagnostic;
import com.sun.tools.javac.util.List;
import combo.ComboInstance;
import combo.ComboParameter;
import combo.ComboTask;
import combo.ComboTask.Result;
import combo.ComboTestHelper;

public class LocalStaticDeclarations extends ComboInstance<LocalStaticDeclarations> {

    static final String sourceTemplate =
            """
            import java.lang.annotation.*;
            class Test {
                int INSTANCE_FIELD = 0;
                static int STATIC_FIELD = 0;
                // instance initializer
                { int LOCAL_VARIABLE = 0;
                    #{CONTAINER}
                }
                Test() {
                    #{CONTAINER}
                }
                void m() {
                    int LOCAL_VARIABLE = 0;
                    #{CONTAINER}
                }
                static void foo() {
                    int LOCAL_VARIABLE = 0;
                    #{CONTAINER}
                }
            }
            """;

    enum Container implements ComboParameter {
        NO_CONTAINER("#{STATIC_LOCAL}"),
        INTERFACE("interface CI { #{STATIC_LOCAL} }"),
        ANNOTATION("@interface CA { #{STATIC_LOCAL} }"),
        ANONYMOUS(
                """
                    new Object() {
                        // instance initializer
                        {
                            #{STATIC_LOCAL}
                        }

                        void m() {
                            #{STATIC_LOCAL}
                        }
                    };
                """
        ),
        RECORD("record CR() { #{STATIC_LOCAL} }"),
        CLASS("class CC { #{STATIC_LOCAL} }"),
        ENUM("enum CE { #{STATIC_LOCAL} }"),
        LAMBDA("Runnable run = () -> { #{STATIC_LOCAL} };");

        String container;

        Container(String container) {
            this.container = container;
        }

        public String expand(String optParameter) {
            return container;
        }
    }

    enum StaticLocalDecl implements ComboParameter {
        ENUM("enum E { E1; #{MEMBER} }"),
        RECORD("record R() { #{MEMBER} }"),
        ANNOTATION("@interface A { #{MEMBER} }"),
        INTERFACE("interface I { #{MEMBER} }");

        String localDecl;

        StaticLocalDecl(String localDecl) {
            this.localDecl = localDecl;
        }

        public String expand(String optParameter) {
            return localDecl;
        }
    }

    enum Member implements ComboParameter {
        NONE(""),
        METHOD("int foo() { return #{EXPR}; }"),
        DEFAULT_METHOD("default int foo() { return #{EXPR}; }");

        String member;

        Member(String member) {
            this.member = member;
        }

        public String expand(String optParameter) {
            return member;
        }
    }

    enum Expression implements ComboParameter {
         LITERAL("1"),
         STATIC_FIELD("STATIC_FIELD"),
         LOCAL_VARIABLE("LOCAL_VARIABLE"),
         INSTANCE_FIELD("INSTANCE_FIELD");

         String expr;

         Expression(String expr) {
             this.expr = expr;
         }

        public String expand(String optParameter) {
            return expr;
        }
    }

    public static void main(String... args) throws Exception {
        new combo.ComboTestHelper<LocalStaticDeclarations>()
                .withFilter(LocalStaticDeclarations::notTriviallyIncorrect)
                .withDimension("CONTAINER", (x, t) -> { x.container = t; }, Container.values())
                .withDimension("STATIC_LOCAL", (x, t) -> { x.decl = t; }, StaticLocalDecl.values())
                .withDimension("MEMBER", (x, t) -> { x.member = t; }, Member.values())
                .withDimension("EXPR", (x, expr) -> x.expr = expr, Expression.values())
                .run(LocalStaticDeclarations::new);
    }

    Container container;
    StaticLocalDecl decl;
    Member member;
    Expression expr;

    @Override
    public void doWork() throws Throwable {
        newCompilationTask()
                .withOptions(new String[]{"--enable-preview", "-source", Integer.toString(Runtime.version().feature())})
                .withSourceFromTemplate("Test", sourceTemplate)
                .generate(this::check);
    }

    boolean notTriviallyIncorrect() {
        return decl == StaticLocalDecl.INTERFACE && (member == Member.DEFAULT_METHOD || member == Member.NONE) ||
               decl != StaticLocalDecl.INTERFACE && (member == Member.METHOD || member == Member.NONE) &&
               ((decl != StaticLocalDecl.ANNOTATION) ||
               (decl == StaticLocalDecl.ANNOTATION && member == Member.NONE));
    }

    void check(ComboTask.Result<Iterable<? extends JavaFileObject>> result) {
        if (shouldFail()) {
            Assert.check(result.hasErrors(), result.compilationInfo());
            if (!expectedDiagFound(result)) {
                fail("test failing with unexpected error message\n" + result.compilationInfo());
            }
        } else {
            Assert.check(!result.hasErrors(), result.compilationInfo());
        }
    }

    boolean shouldFail() {
        return ((container != Container.NO_CONTAINER &&
                container != Container.LAMBDA &&
                container != Container.ANONYMOUS)) ||
                (member != Member.NONE && !acceptableExpr());
    }

    boolean acceptableExpr() {
        return (expr == Expression.LITERAL || expr == Expression.STATIC_FIELD);
    }

    boolean expectedDiagFound(ComboTask.Result<Iterable<? extends JavaFileObject>> result) {
        if ((container == Container.NO_CONTAINER ||
                container == Container.LAMBDA ||
                container == Container.ANONYMOUS) &&
                !acceptableExpr()) {
            return result.containsKey("compiler.err.non-static.cant.be.ref");
        } else if (container == Container.ENUM) {
            if (decl == StaticLocalDecl.ANNOTATION) {
                return result.containsKey("compiler.err.expected");
            } else {
                return result.containsKey("compiler.err.enum.constant.expected" );
            }
        }
        return result.containsKey("compiler.err.static.declaration.not.allowed.in.inner.classes" );
    }
}
