/*
 * Copyright 1999-2006 Sun Microsystems, Inc.  All Rights Reserved.
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

import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Source;
import com.sun.tools.javac.code.Symbol;
import com.sun.tools.javac.code.Symbol.ClassSymbol;
import com.sun.tools.javac.code.Symtab;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.code.Type.ClassType;
import com.sun.tools.javac.code.TypeTags;
import com.sun.tools.javac.code.Types;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.JCTree.JCBlock;
import com.sun.tools.javac.tree.JCTree.JCCase;
import com.sun.tools.javac.tree.JCTree.JCClassDecl;
import com.sun.tools.javac.tree.JCTree.JCErroneous;
import com.sun.tools.javac.tree.JCTree.JCExpression;
import com.sun.tools.javac.tree.JCTree.JCImport;
import com.sun.tools.javac.tree.JCTree.JCLiteral;
import com.sun.tools.javac.tree.JCTree.JCMethodDecl;
import com.sun.tools.javac.tree.JCTree.JCMethodInvocation;
import com.sun.tools.javac.tree.JCTree.JCNewClass;
import com.sun.tools.javac.tree.JCTree.JCStatement;
import com.sun.tools.javac.tree.JCTree.JCTypeParameter;
import com.sun.tools.javac.tree.JCTree.JCVariableDecl;
import com.sun.tools.javac.tree.TreeInfo;
import com.sun.tools.javac.tree.TreeMaker;
import com.sun.tools.javac.tree.TreeTranslator;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.JCDiagnostic;
import com.sun.tools.javac.util.JCDiagnostic.DiagnosticPosition;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.Log;
import com.sun.tools.javac.util.Name;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

/**
 *
 * @author Dusan Balek
 */
public class Repair extends TreeTranslator {

    /** The context key for the Repair phase. */
    protected static final Context.Key<Repair> repairKey = new Context.Key<Repair>();
    private static final String ERR_MESSAGE = "Uncompilable source code";

    /** Get the instance for this context. */
    public static Repair instance(Context context) {
        Repair instance = context.get(repairKey);
        if (instance == null) {
            instance = new Repair(context);
        }
        return instance;
    }

    private Symtab syms;
    private Resolve rs;
    private Enter enter;
    private Types types;
    private Log log;
    private TreeMaker make;
    private boolean allowEnums;
    
    private Env<AttrContext> attrEnv;
    private boolean hasError;
    private JCDiagnostic err;
    private JCTree classLevelErrTree;
    private String classLevelErrMessage;
    private JCBlock staticInit;
    private List<JCTree> parents;
    private Set<Name> repairedClassNames = new HashSet<Name>();
    
    private Repair(Context context) {
        context.put(repairKey, this);
        syms = Symtab.instance(context);
        rs = Resolve.instance(context);
        enter = Enter.instance(context);
        types = Types.instance(context);
        log = Log.instance(context);
        Source source = Source.instance(context);
        allowEnums = source.allowEnums();
    }

    @Override
    public <T extends JCTree> T translate(T tree) {
        if (tree == null)
            return null;
        parents = parents.prepend(tree);
        try {
            if (hasError)
                return super.translate(tree);
            if ((err = log.getErrDiag(tree)) != null)
                hasError = true;
            tree = super.translate(tree);
        } finally {
            parents = parents.tail;            
        }
        if (!(hasError && tree instanceof JCStatement))
            return tree;
        if (tree.getTag() == JCTree.CASE)
            return tree;
        if (tree.getTag() == JCTree.CLASSDEF || tree.getTag() == JCTree.VARDEF) {
            JCTree parent = parents.head;
            if (parent == null || (parent.getTag() != JCTree.BLOCK && parent.getTag() != JCTree.CASE))
                return tree;
        }
        String msg = err != null ? err.getMessage(null) : null;
        hasError = false;
        err = null;
        if (tree.getTag() == JCTree.BLOCK) {
            ((JCBlock)tree).stats = List.of(generateErrStat(tree.pos(), msg));
            return tree;
        }
        return (T)generateErrStat(tree.pos(), msg);
    }

    @Override
    public void visitImport(JCImport tree) {
        super.visitImport(tree);
        if (hasError && err != null) {
            classLevelErrTree = err.getTree();
            classLevelErrMessage = err.getMessage(null);
        }
    }

    @Override
    public void visitTypeParameter(JCTypeParameter tree) {
        super.visitTypeParameter(tree);
        if (tree.type != null && tree.type.tag == TypeTags.TYPEVAR) {
            Type.TypeVar tv = (Type.TypeVar)tree.type;
            if (tv.bound != null && tv.bound.isErroneous()) {
                tv.bound = syms.objectType;
                hasError = true;
            }
        }
    }



    @Override
    public void visitClassDef(JCClassDecl tree) {
        translateClass(tree.sym);
        result = tree;
    }

    @Override
    public void visitVarDef(JCVariableDecl tree) {
        super.visitVarDef(tree);
        if (hasError) {
            JCTree parent = parents != null ? parents.tail.head : null;
            if (parent != null && parent.getTag() == JCTree.CLASSDEF) {
                tree.init = err != null ? generateErrExpr(err.getTree(), err.getMessage(null)) : generateErrExpr(tree.init, null);
                hasError = false;
                err = null;
            }
        }
    }

    @Override
    public void visitMethodDef(JCMethodDecl tree) {
        tree.mods = translate(tree.mods);
        tree.restype = translate(tree.restype);
        tree.typarams = translateTypeParams(tree.typarams);
        tree.params = translateVarDefs(tree.params);
        tree.thrown = translate(tree.thrown);
        tree.defaultValue = translate(tree.defaultValue);
        tree.body = translate(tree.body);
        result = tree;
        if (hasError) {
            if (tree.sym != null) {
                tree.sym.flags_field &= ~(Flags.ABSTRACT | Flags.NATIVE);
                tree.sym.defaultValue = null;
            }
            tree.defaultValue = null;
            if (tree.body == null) {
                tree.body = make.Block(0, List.<JCStatement>nil());
            }
            tree.body.stats = List.of(generateErrStat(tree.pos(), err != null ? err.getMessage(null) : null));
            hasError = false;
            err = null;
        }
    }

    @Override
    public void visitBlock(JCBlock tree) {
        if (tree.isStatic() && staticInit == null)
            staticInit = tree;
        List<JCStatement> last = null;
        for (List<JCStatement> l = tree.stats; l.nonEmpty(); l = l.tail) {
            l.head = translate(l.head);
            if (last == null && l.head.getTag() == JCTree.THROW)
                last = l;
        }
        if (last != null)
            last.tail = List.nil();
        result = tree;
    }

    @Override
    public void visitApply(JCMethodInvocation tree) {
        Symbol meth = TreeInfo.symbol(tree.meth);
        if (meth == null) {
            Logger.getLogger(Repair.class.getName()).warning("Repair.visitApply tree [" + tree + "] has null symbol."); //NOI18N
            hasError = true;
        } else if (meth.type == null || meth.type.isErroneous()) {
            hasError = true;
        } else {
            List<Type> argtypes = meth.type.getParameterTypes();
            if (allowEnums && meth.name == meth.name.table.init && meth.owner == syms.enumSym)
                argtypes = argtypes.tail.tail;
            if (tree.varargsElement == null && tree.args.length() != argtypes.length()) {
                Logger.getLogger(Repair.class.getName()).warning("Repair.visitApply [" + meth.owner + "]'s method [" + meth + "] of type: [" + meth.type + "]; has different number of parameters than tree [" + tree +"]."); //NOI18N
                hasError = true;
            }
        }
        super.visitApply(tree);
    }

    @Override
    public void visitNewClass(JCNewClass tree) {
        Symbol ctor = tree.constructor;
        if (ctor == null) {
            Logger.getLogger(Repair.class.getName()).warning("Repair.visitNewClass tree [" + tree + "] has null constructor symbol."); //NOI18N
            hasError = true;
        } else if (ctor.type == null || ctor.type.isErroneous()) {
            hasError = true;
        }
        super.visitNewClass(tree);
    }

    @Override
    public void visitCase(JCCase tree) {
        tree.pat = translate(tree.pat);
        List<JCStatement> last = null;
        for (List<JCStatement> l = tree.stats; l.nonEmpty(); l = l.tail) {
            l.head = translate(l.head);
            if (last == null && l.head.getTag() == JCTree.THROW)
                last = l;
        }
        if (last != null)
            last.tail = List.nil();
        result = tree;
    }

    @Override
    public void visitErroneous(JCErroneous tree) {
        hasError = true;
        result = tree;
    }
    
    private JCStatement generateErrStat(DiagnosticPosition pos, String msg) {
        make.at(pos);
        ClassType ctype = (ClassType)syms.runtimeExceptionType;
        JCLiteral literal = make.Literal(msg != null ? ERR_MESSAGE + " - " + msg : ERR_MESSAGE); //NOI18N
        JCNewClass tree = make.NewClass(null, null, make.QualIdent(ctype.tsym), List.<JCExpression>of(literal), null);
        tree.constructor = rs.resolveConstructor(pos, attrEnv, ctype, List.of(literal.type), null, false, false);
        tree.type = ctype;
        return make.Throw(tree);
    }
    
    private JCExpression generateErrExpr(DiagnosticPosition pos, String msg) {
        make.at(pos);
        JCExpression expr = make.Erroneous(List.<JCStatement>of(generateErrStat(pos, msg)));
        expr.type = syms.errType;
        return expr;
    }
    
    private JCBlock generateErrStaticInit(DiagnosticPosition pos, String msg) {
        make.at(pos);
        return make.Block(Flags.STATIC, List.<JCStatement>of(generateErrStat(pos, msg)));
    }

    private void translateClass(ClassSymbol c) {
        if (c == null)
            return;
        Type st = types.supertype(c.type);
        if (st.tag == TypeTags.CLASS)
            translateClass((ClassSymbol)st.tsym);
        if (repairedClassNames.contains(c.flatname))
            return;
        repairedClassNames.add(c.flatname);
        Env<AttrContext> myEnv = enter.typeEnvs.get(c);
        if (myEnv == null)
            return;
        Env<AttrContext> oldEnv = attrEnv;
        try {
            attrEnv = myEnv;
            TreeMaker oldMake = make;
            make = make.forToplevel(attrEnv.toplevel);
            boolean oldHasError = hasError;
            JCDiagnostic oldErr = err;
            JCTree oldClassLevelErrTree = classLevelErrTree;
            String oldClassLevelErrMessage = classLevelErrMessage;
            JCBlock oldStaticinit = staticInit;
            try {
                for (JCImport imp : attrEnv.toplevel.getImports()) {
                    translate(imp);
                    if (classLevelErrTree != null)
                        break;
                }
                hasError = false;
                err = null;
                staticInit = null;
                JCClassDecl tree = (JCClassDecl)attrEnv.tree;
                tree.mods = translate(tree.mods);
                tree.typarams = translateTypeParams(tree.typarams);
                tree.extending = translate(tree.extending);
                tree.implementing = translate(tree.implementing);
                if (hasError && err != null) {
                    classLevelErrTree = err.getTree();
                    classLevelErrMessage = err.getMessage(null);
                }
                if (tree.defs != null) {
                    List<JCTree> last = null;
                    for (List<JCTree> l = tree.defs; l != null && l.nonEmpty(); l = l.tail) {
                        hasError = false;
                        err = null;
                        l.head = translate(l.head);
                        if (hasError) {
                            if (last != null)
                                last.tail = l.tail;
                            else
                                tree.defs = l.tail;
                            if (classLevelErrTree == null) {
                                if (err != null) {
                                    classLevelErrTree = err.getTree();
                                    classLevelErrMessage = err.getMessage(null);
                                } else {
                                    classLevelErrTree = l.head;
                                }
                            }
                        } else {
                            last = l;
                        }
                    }
                    if (classLevelErrTree != null) {
                        if (staticInit != null)
                            staticInit.stats = List.of(generateErrStat(classLevelErrTree, classLevelErrMessage));
                        else
                            tree.defs = tree.defs.prepend(generateErrStaticInit(classLevelErrTree, classLevelErrMessage));
                    }
                }
            } finally {
                staticInit = oldStaticinit;
                classLevelErrTree = oldClassLevelErrTree;
                classLevelErrMessage = oldClassLevelErrMessage;
                err = oldErr;
                hasError = oldHasError;
                make = oldMake;
            }
        } finally {
            attrEnv = oldEnv;
        }
    }

    public JCTree translateTopLevelClass(Env<AttrContext> env, JCTree tree, TreeMaker localMake) {
        try {
            attrEnv = env;
            make = localMake;
            hasError = false;
            parents = List.nil();
            return translate(tree);
        } finally {
            attrEnv = null;
            make = null;
        }
    }
    
    public void flush() {
        repairedClassNames.clear();
    }
}
