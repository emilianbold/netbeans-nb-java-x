/*
 * Copyright (c) 2001, 2016, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
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

package jdk.javadoc.internal.tool;

import java.util.EnumSet;

import javax.tools.JavaFileObject;

import com.sun.tools.javac.api.JavacTrees;
import com.sun.tools.javac.code.Symbol.PackageSymbol;
import com.sun.tools.javac.code.ClassFinder;
import com.sun.tools.javac.util.Context;

/** Javadoc uses an extended class finder that records package.html entries
 *
 *  <p><b>This is NOT part of any supported API.
 *  If you write code that depends on this, you do so at your own risk.
 *  This code and its internal interfaces are subject to change or
 *  deletion without notice.</b>
 *
 *  @author Neal Gafter
 */
public class JavadocClassFinder extends ClassFinder {

    public static JavadocClassFinder instance(Context context) {
        ClassFinder instance = context.get(classFinderKey);
        if (instance == null)
            instance = new JavadocClassFinder(context, true);
        return (JavadocClassFinder)instance;
    }

    public static void preRegister(Context context) {
        preRegister(context, true);
    }
    
    public static void preRegister(Context context, final boolean loadToolEnv) {
        context.put(classFinderKey, new Context.Factory<ClassFinder>() {
            public ClassFinder make(Context c) {
                return new JavadocClassFinder(c, loadToolEnv);
            }
        });
    }

    private ToolEnvironment toolEnv;
    private EnumSet<JavaFileObject.Kind> all = EnumSet.of(JavaFileObject.Kind.CLASS,
                                                          JavaFileObject.Kind.SOURCE,
                                                          JavaFileObject.Kind.HTML);
    private EnumSet<JavaFileObject.Kind> noSource = EnumSet.of(JavaFileObject.Kind.CLASS,
                                                               JavaFileObject.Kind.HTML);
    private final JavacTrees trees;

    public JavadocClassFinder(Context context, boolean loadToolEnv) {
        super(context);
        if (loadToolEnv)
            toolEnv = ToolEnvironment.instance(context);
        preferSource = true;
        trees = JavacTrees.instance(context);
    }

    /**
     * Override getPackageFileKinds to include search for package.html
     */
    @Override
    protected EnumSet<JavaFileObject.Kind> getPackageFileKinds() {
        if (toolEnv == null)
            return super.getPackageFileKinds();
        else
            return toolEnv.docClasses ? noSource : all;
    }

    /**
     * Override extraFileActions to check for package documentation
     */
    @Override
    protected void extraFileActions(PackageSymbol pack, JavaFileObject fo) {
        if (fo.isNameCompatible("package", JavaFileObject.Kind.HTML)) {
            toolEnv.pkgToJavaFOMap.put(pack, fo);
            trees.putJavaFileObject(pack, fo);
        }
    }
}
