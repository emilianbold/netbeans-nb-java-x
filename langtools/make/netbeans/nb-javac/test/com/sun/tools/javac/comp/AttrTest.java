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
package com.sun.tools.javac.comp;

import com.sun.source.tree.CompilationUnitTree;
import com.sun.source.tree.IdentifierTree;
import com.sun.source.tree.VariableTree;
import com.sun.source.util.TreePathScanner;
import com.sun.source.util.Trees;
import com.sun.tools.javac.api.JavacTaskImpl;
import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;
import javax.lang.model.element.Element;
import javax.lang.model.element.ElementKind;
import javax.tools.Diagnostic;
import javax.tools.DiagnosticCollector;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileObject;
import javax.tools.SimpleJavaFileObject;
import javax.tools.ToolProvider;
import junit.framework.TestCase;

/**
 *
 * @author Jan Lahoda
 */
public class AttrTest extends TestCase {

    public AttrTest(String testName) {
        super(testName);
    }

    static class MyFileObject extends SimpleJavaFileObject {
        private String text;
        public MyFileObject(String text) {
            this("Test", text);
        }
        public MyFileObject(String name, String text) {
            super(URI.create("myfo:/" + name + ".java"), JavaFileObject.Kind.SOURCE);
            this.text = text;
        }
        @Override
        public CharSequence getCharContent(boolean ignoreEncodingErrors) {
            return text;
        }
    }

    public void testExceptionParameterCorrectKind() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String code = "package test; public class Test { { try { } catch (NullPointerException ex) {} } }";

        final JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, null, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject(code)));
        CompilationUnitTree cut = ct.parse().iterator().next();

        ct.analyze();

        new TreePathScanner<Void, Void>() {
            @Override
            public Void visitVariable(VariableTree node, Void p) {
                Element el = Trees.instance(ct).getElement(getCurrentPath());

                assertNotNull(el);
                assertEquals(ElementKind.EXCEPTION_PARAMETER, el.getKind());

                return super.visitVariable(node, p);
            }
        }.scan(cut, null);
    }

    public void testNPEFromNCTWithUnboundWildcard() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String code = "package test; public class Test { { new java.util.ArrayList<java.util.List<?>>() {}; } }";

        final JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, null, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject(code)));
        ct.analyze();
    }
    
    public void testErrorReturnType1() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String api = "package test; public class API { public static Undef call() { return null; } }";
        String use = "package test; public class Use { public void t() { Object str = API.call(); } }";
        DiagnosticCollector<JavaFileObject> dc = new DiagnosticCollector<JavaFileObject>();
        final JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, dc, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject("API", api), new MyFileObject("Use", use)));
        
        ct.analyze();
        
        Set<String> diagnostics = new HashSet<String>();
        
        for (Diagnostic<? extends JavaFileObject> d : dc.getDiagnostics()) {
            diagnostics.add(d.getSource().getName() + ":" + d.getStartPosition() + "-" + d.getEndPosition() + ":" + d.getCode());
        }
        
        assertEquals(new HashSet<String>(Arrays.asList("/API.java:47-52:compiler.err.cant.resolve.location", "/Use.java:64-72:compiler.err.type.error")), diagnostics);
    }
    
    public void testErrorReturnType2() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String api = "package test; public class API { public static String call() { return null; } }";
        String use = "package test; public class Use { public void t() { Object str = API.; } }";
        DiagnosticCollector<JavaFileObject> dc = new DiagnosticCollector<JavaFileObject>();
        final JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, dc, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject("API", api), new MyFileObject("Use", use)));
        
        ct.analyze();
        
        Set<String> diagnostics = new HashSet<String>();
        
        for (Diagnostic<? extends JavaFileObject> d : dc.getDiagnostics()) {
            diagnostics.add(d.getSource().getName() + ":" + d.getStartPosition() + "-" + d.getEndPosition() + ":" + d.getCode());
        }
        
        assertEquals(new HashSet<String>(Arrays.asList("/Use.java:68-68:compiler.err.expected")), diagnostics);
    }
    
    public void testErrorReturnType3() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String api = "package test; public class API { public static String call() { return null; } }";
        String use = "package test; public class Use { public void t() { Object str = API.undef(1); } }";
        DiagnosticCollector<JavaFileObject> dc = new DiagnosticCollector<JavaFileObject>();
        final JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, dc, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject("API", api), new MyFileObject("Use", use)));
        
        ct.analyze();
        
        Set<String> diagnostics = new HashSet<String>();
        
        for (Diagnostic<? extends JavaFileObject> d : dc.getDiagnostics()) {
            diagnostics.add(d.getSource().getName() + ":" + d.getStartPosition() + "-" + d.getEndPosition() + ":" + d.getCode());
        }
        
        assertEquals(new HashSet<String>(Arrays.asList("/Use.java:64-73:compiler.err.cant.resolve.location.args")), diagnostics);
    }

    public void testErrorReturnType4() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String api = "package test; public class API { public static Undef call() { return null; } }";
        String use = "package test; import static test.API.*; public class Use { public void t() { Object str = call(); } }";
        DiagnosticCollector<JavaFileObject> dc = new DiagnosticCollector<JavaFileObject>();
        final JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, dc, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject("API", api), new MyFileObject("Use", use)));
        
        ct.analyze();
        
        Set<String> diagnostics = new HashSet<String>();
        
        for (Diagnostic<? extends JavaFileObject> d : dc.getDiagnostics()) {
            diagnostics.add(d.getSource().getName() + ":" + d.getStartPosition() + "-" + d.getEndPosition() + ":" + d.getCode());
        }
        
        assertEquals(new HashSet<String>(Arrays.asList("/API.java:47-52:compiler.err.cant.resolve.location", "/Use.java:90-94:compiler.err.type.error")), diagnostics);
    }

    public void testErrorReturnType5() throws IOException {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        String use = "package test; public class Use { public static Undef call() { return null; } public void t() { Object str = call(); } }";
        DiagnosticCollector<JavaFileObject> dc = new DiagnosticCollector<JavaFileObject>();
        final JavacTaskImpl ct = (JavacTaskImpl)tool.getTask(null, null, dc, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject("Use", use)));
        
        ct.analyze();
        
        Set<String> diagnostics = new HashSet<String>();
        
        for (Diagnostic<? extends JavaFileObject> d : dc.getDiagnostics()) {
            diagnostics.add(d.getSource().getName() + ":" + d.getStartPosition() + "-" + d.getEndPosition() + ":" + d.getCode());
        }
        
        assertEquals(new HashSet<String>(Arrays.<String>asList("/Use.java:47-52:compiler.err.cant.resolve.location")), diagnostics);
    }
}
