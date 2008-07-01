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

package global;

import com.sun.source.util.JavacTask;
import com.sun.tools.javac.util.BaseFileObject;
import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import javax.tools.FileObject;
import javax.tools.ForwardingJavaFileManager;
import javax.tools.JavaCompiler;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileManager.Location;
import javax.tools.JavaFileObject;
import javax.tools.JavaFileObject.Kind;
import javax.tools.SimpleJavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.ToolProvider;
import junit.framework.TestCase;
import org.apache.bcel.classfile.ClassParser;
import org.apache.bcel.classfile.Field;
import org.apache.bcel.classfile.JavaClass;
import org.apache.bcel.classfile.Method;
import org.apache.bcel.generic.Type;

/**
 *
 * @author Jan Lahoda
 */
public class ErrorToleranceTest extends TestCase {

    public void testSimple1() throws Exception {
        final String code = "package test;\n" +
                      "public class Test {\n" +
                      "    private void method(Unknown u) {\n" +
                      "    }\n" +
                      "}\n";

        final String golden = "package test;\n" +
                      "public class Test {\n" +
                      "    private void method(Unknown u) {\n" +
                      "        throw new RuntimeException(\"Uncompilable source code\");" +
                      "    }\n" +
                      "}\n";
        
        compareResults(golden, code);
    }

    public void testSimple2() throws Exception {
        final String code = "package test;\n" +
                      "public class Test {\n" +
                      "    private void method(Object u) {\n" +
                      "        bflmpsvz" +
                      "    }\n" +
                      "}\n";

        final String golden = "package test;\n" +
                      "public class Test {\n" +
                      "    private void method(Object u) {\n" +
                      "        throw new RuntimeException(\"Uncompilable source code\");" +
                      "    }\n" +
                      "}\n";
        
        compareResults(golden, code);
    }

    public void testSimple3() throws Exception {
        final String code = "package test;\n" +
                      "public class Test {\n" +
                      "    private void method(Object o) {\n" +
                      "    }\n" +
                      "    private void method(Unknown u) {\n" +
                      "    }\n" +
                      "}\n";

        final String golden = "package test;\n" +
                      "public class Test {\n" +
                      "    private void method(Object o) {\n" +
                      "    }\n" +
                      "    private void method(Unknown u) {\n" +
                      "        throw new RuntimeException(\"Uncompilable source code\");" +
                      "    }\n" +
                      "}\n";
        
        compareResults(golden, code);
    }

    public void testInvalidFieldInit() throws Exception {
        final String code = "package test;\n" +
                      "public class Test {\n" +
                      "    public Test() {\n" +
                      "    }\n" +
                      "    public Test(Object o) {\n" +
                      "    }\n" +
                      "    private String s = bflmpsvz;\n" +
                      "}\n";

        final String golden = "package test;\n" +
                      "public class Test {\n" +
                      "    public Test() {\n" +
                      "        throw new RuntimeException(\"Uncompilable source code\");" +
                      "    }\n" +
                      "    public Test(Object o) {\n" +
                      "        throw new RuntimeException(\"Uncompilable source code\");" +
                      "    }\n" +
                      "    private String s;\n" +
                      "}\n";
        
        compareResults(golden, code);
    }

    public void testInvalidStaticFieldInit() throws Exception {
        final String code = "package test;\n" +
                      "public class Test {\n" +
                      "    private static String s = bflmpsvz;\n" +
                      "}\n";

        final String golden = "package test;\n" +
                      "public class Test {\n" +
                      "    static {\n" +
                      "        throw new RuntimeException(\"Uncompilable source code\");" +
                      "    }\n" +
                      "    private static String s;\n" +
                      "}\n";
        
        compareResults(golden, code);
    }

    public void testInvalidCase() throws Exception {
        final String code = "package test;\n" +
                      "public class Test {\n" +
                      "    private void method(int i) {\n" +
                      "        switch(i) {\n" +
                      "            case Unknown.CONSTANT:\n" +
                      "                break;\n" +                      
                      "    }\n" +
                      "}\n";

        final String golden = "package test;\n" +
                      "public class Test {\n" +
                      "    private void method(int i) {\n" +
                      "        throw new RuntimeException(\"Uncompilable source code\");" +
                      "    }\n" +
                      "}\n";
        
        compareResults(golden, code);
    }

    public void testInvalidImport() throws Exception {
        final String code = "package test;\n" +
                      "import a.b.c.List;\n" +
                      "public class Test {\n" +
                      "}\n";

        final String golden = "package test;\n" +
                      "public class Test {\n" +
                      "    static {\n" +
                      "        throw new RuntimeException(\"Uncompilable source code\");\n" +
                      "    }\n" +
                      "}\n";

        compareResults(golden, code);
    }

    public void testInvalidImportWithStaticInit() throws Exception {
        final String code = "package test;\n" +
                      "import a.b.c.List;\n" +
                      "public class Test {\n" +
                      "    static {\n" +
                      "        System.out.println();\n" +
                      "    }\n" +
                      "}\n";

        final String golden = "package test;\n" +
                      "public class Test {\n" +
                      "    static {\n" +
                      "        throw new RuntimeException(\"Uncompilable source code\");\n" +
                      "    }\n" +
                      "}\n";

        compareResults(golden, code);
    }

    public void testInvalidCodeBeforePackage() throws Exception {
        final String code = "xyz\n" +
                      "package test;\n" +
                      "public class Test {\n" +
                      "}\n";

        final String golden = "package test;\n" +
                      "public class Test {\n" +
                      "}\n";

        compareResults(golden, code);
    }

    public void testInvalidCodeAfterClass() throws Exception {
        final String code = "package test;\n" +
                      "public class Test {\n" +
                      "}\n" +
                      "xyz\n";

        final String golden = "package test;\n" +
                      "public class Test {\n" +
                      "}\n";

        compareResults(golden, code);
    }

    //<editor-fold defaultstate="collapsed" desc=" Test Infrastructure ">
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

    private File workingDir;

    @Override
    protected void setUp() throws Exception {
        super.setUp();

        workingDir = File.createTempFile("ErrorToleranceTest", "");

        workingDir.delete();
        workingDir.mkdirs();
    }

    @Override
    protected void tearDown() throws Exception {
        deleteRecursively(workingDir);
        super.tearDown();
    }

    private File[] compile(String code) throws Exception {
        final String bootPath = System.getProperty("sun.boot.class.path"); //NOI18N
        final JavaCompiler tool = ToolProvider.getSystemJavaCompiler();
        assert tool != null;

        StandardJavaFileManager std = tool.getStandardFileManager(null, null, null);
        MemoryJavaFileManager mjfm = new MemoryJavaFileManager(std);

        std.setLocation(StandardLocation.CLASS_OUTPUT, Collections.singleton(workingDir));
        
        JavacTask ct = (JavacTask)tool.getTask(null, mjfm, null, Arrays.asList("-bootclasspath",  bootPath, "-Xjcov"), null, Arrays.asList(new MyFileObject(code)));
        
        ct.parse();
        ct.analyze();
        
        List<File> result = new LinkedList<File>();
        
        for (JavaFileObject jfo : ct.generate()) {
            assertTrue(jfo instanceof BaseFileObject);
            
            BaseFileObject bfo = (BaseFileObject) jfo;
            File file = new File(bfo.getPath());
            
            assertTrue(file.getAbsolutePath(), file.canRead());
            
            result.add(file);
        }
        
        return result.toArray(new File[0]);
    }
    
    private Collection<String> dumpSignatures(File[] fqns) throws Exception {
        List<String> result = new LinkedList<String>();
        
        for (File f : fqns) {
            result.add(toString(new ClassParser(f.getAbsolutePath()).parse()));
        }
        
        return result;
    }
    
    private String toString(JavaClass clazz) throws ClassNotFoundException {
        StringBuilder sb = new StringBuilder();
        
        sb.append(Integer.toHexString(clazz.getAccessFlags()));
        sb.append(" class ");
        sb.append(clazz.getClassName());
        sb.append(" extends ");
        sb.append(clazz.getSuperClass().getClassName());
        sb.append(" implements ");
        for (JavaClass c : clazz.getInterfaces()) {
            sb.append(c.getSuperClass().getClassName());
            sb.append(", ");
        }
        sb.append("{\n");
        for (Field f: clazz.getFields()) {
            sb.append(toString(f));
            sb.append("\n");
        }
        for (Method m: clazz.getMethods()) {
            sb.append(toString(m));
            sb.append("\n");
        }
        sb.append("\n");
        sb.append("}");
        return sb.toString();
    }

    private String toString(Field clazz) throws ClassNotFoundException {
        StringBuilder sb = new StringBuilder();
        
        sb.append(Integer.toHexString(clazz.getAccessFlags()));
        sb.append(" ");
        sb.append(clazz.getType().toString());
        sb.append(" ");
        sb.append(clazz.getName());
        sb.append(" = ");
        sb.append(clazz.getConstantValue());
        
        return sb.toString();
    }
    
    private String toString(Method clazz) throws ClassNotFoundException {
        StringBuilder sb = new StringBuilder();

        sb.append(Integer.toHexString(clazz.getAccessFlags()));
        sb.append(" ");
        sb.append(clazz.getReturnType().toString());
        sb.append(" ");
        sb.append(clazz.getName());
        sb.append("(");
        for (Type t : clazz.getArgumentTypes()) {
            sb.append(t.toString());
            sb.append(", ");
        }
        sb.append(") {\n");
        clazz.getCode().setAttributes(null);
        sb.append(clazz.getCode().toString());
        sb.append("\n}");

        return sb.toString();
    }
    
    private void compareResults(String golden, String code) throws Exception {
        assertEquals(dumpSignatures(compile(golden)), dumpSignatures(compile(code)));
    }

    private void deleteRecursively(File f) {
        if (f.isDirectory()) {
            for (File c : f.listFiles()) {
                deleteRecursively(c);
            }
        }
        
        f.delete();
    }
    
    private static final class MemoryJavaFileManager extends ForwardingJavaFileManager {

        public MemoryJavaFileManager(JavaFileManager jfm) {
            super(jfm);
        }

        @Override
        public JavaFileObject getJavaFileForOutput(Location location, String className, Kind kind, FileObject sibling) throws IOException {
            JavaFileObject jfo = super.getJavaFileForOutput(location, className, kind, sibling);
            
            System.err.println("output=" + jfo);
            
            return jfo;
        }
        
    }
    //</editor-fold>
}
