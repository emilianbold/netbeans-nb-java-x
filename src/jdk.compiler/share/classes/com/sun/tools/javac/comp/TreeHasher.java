/*
 * Copyright (c) 2018, Google LLC. All rights reserved.
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

package com.sun.tools.javac.comp;

import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.JCTree.JCFieldAccess;
import com.sun.tools.javac.tree.JCTree.JCIdent;
import com.sun.tools.javac.tree.JCTree.JCLiteral;
import com.sun.tools.javac.tree.TreeInfo;
import com.sun.tools.javac.tree.TreeScanner;
import java.util.Objects;
import java.util.function.Function;

/** A tree visitor that computes a hash code. */
public class TreeHasher extends TreeScanner {

    private final Function<Symbol, Integer> symbolHasher;
    private int result = 17;

    public TreeHasher(Function<Symbol, Integer> symbolHasher) {
        this.symbolHasher = Objects.requireNonNull(symbolHasher);
    }

    public static int hash(JCTree tree, Function<Symbol, Integer> symbolHasher) {
        if (tree == null) {
            return 0;
        }
        TreeHasher hasher = new TreeHasher(symbolHasher);
        tree.accept(hasher);
        return hasher.result;
    }

    private void hash(Object object) {
        result = 31 * result + Objects.hashCode(object);
    }

    @Override
    public void scan(JCTree tree) {
        if (tree == null) {
            return;
        }
        tree = TreeInfo.skipParens(tree);
        if (tree.type != null) {
            Object value = tree.type.constValue();
            if (value != null) {
                hash(value);
                return;
            }
        }
        hash(tree.getTag());
        tree.accept(this);
    }

    @Override
    public void visitLiteral(JCLiteral tree) {
        hash(tree.value);
        super.visitLiteral(tree);
    }

    @Override
    public void visitIdent(JCIdent tree) {
        Symbol sym = tree.sym;
        if (sym != null) {
            Integer hash = symbolHasher.apply(sym);
            if (hash != null) {
                hash(hash);
                return;
            }
        }
        hash(sym);
    }

    @Override
    public void visitSelect(JCFieldAccess tree) {
        hash(tree.sym);
        super.visitSelect(tree);
    }
}
