/*
 * Copyright 2003-2004 Sun Microsystems, Inc.  All Rights Reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Sun designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Sun in the LICENSE file that accompanied this code.
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
 * Please contact Sun Microsystems, Inc., 4150 Network Circle, Santa Clara,
 * CA 95054 USA or visit www.sun.com if you need additional information or
 * have any questions.
 */

package com.sun.tools.javac.parser;

import com.sun.source.tree.BinaryTree;
import com.sun.source.tree.BlockTree;
import com.sun.source.tree.ClassTree;
import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.ExpressionStatementTree;
import com.sun.source.tree.ExpressionTree;
import com.sun.source.tree.MethodInvocationTree;
import com.sun.source.tree.MethodTree;
import com.sun.source.tree.ModifiersTree;
import com.sun.source.tree.StatementTree;
import com.sun.source.tree.VariableTree;
import com.sun.source.tree.WhileLoopTree;
import com.sun.source.util.SourcePositions;
import com.sun.source.util.Trees;
import com.sun.tools.javac.api.JavacTaskImpl;
import com.sun.tools.javac.tree.JCTree;
import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.List;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;
import junit.framework.TestCase;

public class ParserTest extends TestCase {

    public ParserTest(String testName) {
        super(testName);
    }

    static class MyFileObject extends SimpleJavaFileObject {
        private String text;
        public MyFileObject(String text) {
            super(URI.create("myfo:/Test.java"), JavaFileObject.Kind.SOURCE);
            this.text = text;
        }
        @Override
        public CharSequence getCharContent(boolean ignoreEncodingErrors) {
            return text;
        }
    }

    public void testPositionForSuperConstructorCalls() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String code = "package test; public class Test {public Test() {super();}}";

        JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, null, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject(code)));
        CompilationUnitTree cut = ct.parse().iterator().next();
        SourcePositions pos = Trees.instance(ct).getSourcePositions();

        MethodTree method = (MethodTree) ((ClassTree) cut.getTypeDecls().get(0)).getMembers().get(0);
        ExpressionStatementTree es = (ExpressionStatementTree) method.getBody().getStatements().get(0);

        assertEquals(72 - 24, pos.getStartPosition(cut, es));
        assertEquals(80 - 24, pos.getEndPosition(cut, es));

        MethodInvocationTree mit = (MethodInvocationTree) es.getExpression();

        assertEquals(72 - 24, pos.getStartPosition(cut, mit));
        assertEquals(79 - 24, pos.getEndPosition(cut, mit));

        assertEquals(72 - 24, pos.getStartPosition(cut, mit.getMethodSelect()));
        assertEquals(77 - 24, pos.getEndPosition(cut, mit.getMethodSelect()));

    }

    public void testPositionForEnumModifiers() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String code = "package test; public enum Test {A;}";

        JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, null, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject(code)));
        CompilationUnitTree cut = ct.parse().iterator().next();
        SourcePositions pos = Trees.instance(ct).getSourcePositions();

        ClassTree clazz = (ClassTree) cut.getTypeDecls().get(0);
        ModifiersTree mt = clazz.getModifiers();

        assertEquals(38 - 24, pos.getStartPosition(cut, mt));
        assertEquals(44 - 24, pos.getEndPosition(cut, mt));
    }

//    public void testErroneousMemberSelectPositions() throws IOException {
//        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
//        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
//        assert tool != null;
//
//        String code = "package test; public class Test { public void test() { new Runnable() {}.   } public Test() {}}";
//
//        JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, null, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject(code)));
//        CompilationUnitTree cut = ct.parse().iterator().next();
//        SourcePositions pos = Trees.instance(ct).getSourcePositions();
//
//        ClassTree clazz = (ClassTree) cut.getTypeDecls().get(0);
//        ExpressionStatementTree est = (ExpressionStatementTree) ((MethodTree) clazz.getMembers().get(0)).getBody().getStatements().get(0);
//
//        assertEquals(79 - 24, pos.getStartPosition(cut, est));
//        assertEquals(97 - 24, pos.getEndPosition(cut, est));
//    }

    public void testNewClassWithEnclosing() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String code = "package test; class Test { class d {} private void method() { Object o = Test.this.new d(); } }";

        JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, null, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject(code)));
        CompilationUnitTree cut = ct.parse().iterator().next();
        SourcePositions pos = Trees.instance(ct).getSourcePositions();

        ClassTree clazz = (ClassTree) cut.getTypeDecls().get(0);
        ExpressionTree est = ((VariableTree) ((MethodTree) clazz.getMembers().get(1)).getBody().getStatements().get(0)).getInitializer();

        assertEquals(97 - 24, pos.getStartPosition(cut, est));
        assertEquals(114 - 24, pos.getEndPosition(cut, est));
    }

    public void testPreferredPositionForBinaryOp() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String code = "package test; public class Test {private void test() {Object o = null; boolean b = o != null && o instanceof String;} private Test() {}}";

        JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, null, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject(code)));
        CompilationUnitTree cut = ct.parse().iterator().next();

        ClassTree clazz = (ClassTree) cut.getTypeDecls().get(0);
        MethodTree method = (MethodTree) clazz.getMembers().get(0);
        VariableTree condSt = (VariableTree) method.getBody().getStatements().get(1);
        BinaryTree cond = (BinaryTree) condSt.getInitializer();

        JCTree condJC = (JCTree) cond;

        assertEquals(117 - 24, condJC.pos);
    }

    public void testPositionBrokenSource126732a() throws IOException {
        String[] commands = new String[] {
            "return Runnable()",
            "do { } while (true)",
            "throw UnsupportedOperationException()",
            "assert true",
            "1 + 1",
        };

        for (String command : commands) {
            final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
            final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
            assert tool != null;

            String code = "package test;\n" +
                    "public class Test {\n" +
                    "    public static void test() {\n" +
                    "        " + command + " {\n" +
                    "                new Runnable() {\n" +
                    "        };\n" +
                    "    }\n" +
                    "}";

            JavacTaskImpl ct = (JavacTaskImpl) tool.getTask(null, null, null, Arrays.asList("-bootclasspath", bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject(code)));
            CompilationUnitTree cut = ct.parse().iterator().next();

            ClassTree clazz = (ClassTree) cut.getTypeDecls().get(0);
            MethodTree method = (MethodTree) clazz.getMembers().get(0);
            List<? extends StatementTree> statements = method.getBody().getStatements();

            StatementTree ret = statements.get(0);
            StatementTree block = statements.get(1);

            Trees t = Trees.instance(ct);

            assertEquals(command, code.indexOf(command + " {") + (command + " ").length(), t.getSourcePositions().getEndPosition(cut, ret));
            assertEquals(command, code.indexOf(command + " {") + (command + " ").length(), t.getSourcePositions().getStartPosition(cut, block));
        }
    }

    public void testPositionBrokenSource126732b() throws IOException {
        String[] commands = new String[] {
            "break",
            "break A",
            "continue ",
            "continue A",
        };

        for (String command : commands) {
            final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
            final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
            assert tool != null;

            String code = "package test;\n" +
                    "public class Test {\n" +
                    "    public static void test() {\n" +
                    "        while (true) {\n" +
                    "            " + command + " {\n" +
                    "                new Runnable() {\n" +
                    "        };\n" +
                    "        }\n" +
                    "    }\n" +
                    "}";

            JavacTaskImpl ct = (JavacTaskImpl) tool.getTask(null, null, null, Arrays.asList("-bootclasspath", bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject(code)));
            CompilationUnitTree cut = ct.parse().iterator().next();

            ClassTree clazz = (ClassTree) cut.getTypeDecls().get(0);
            MethodTree method = (MethodTree) clazz.getMembers().get(0);
            List<? extends StatementTree> statements = ((BlockTree) ((WhileLoopTree) method.getBody().getStatements().get(0)).getStatement()).getStatements();

            StatementTree ret = statements.get(0);
            StatementTree block = statements.get(1);

            Trees t = Trees.instance(ct);

            assertEquals(command, code.indexOf(command + " {") + (command + " ").length(), t.getSourcePositions().getEndPosition(cut, ret));
            assertEquals(command, code.indexOf(command + " {") + (command + " ").length(), t.getSourcePositions().getStartPosition(cut, block));
        }
    }

}
