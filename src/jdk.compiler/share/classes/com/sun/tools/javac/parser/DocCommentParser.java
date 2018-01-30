/*
 * Copyright (c) 2012, 2017, Oracle and/or its affiliates. All rights reserved.
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

package com.sun.tools.javac.parser;

import java.text.BreakIterator;
import java.util.HashMap;
import java.util.Map;

import com.sun.source.doctree.AttributeTree.ValueKind;
import com.sun.tools.javac.parser.DocCommentParser.TagParser.Kind;
import com.sun.tools.javac.parser.JavacParser.AbstractEndPosTable;
import com.sun.tools.javac.parser.Tokens.Comment;
import com.sun.tools.javac.parser.Tokens.TokenKind;
import com.sun.tools.javac.tree.DCTree;
import com.sun.tools.javac.tree.DCTree.DCAttribute;
import com.sun.tools.javac.tree.DCTree.DCDocComment;
import com.sun.tools.javac.tree.DCTree.DCEndPosTree;
import com.sun.tools.javac.tree.DCTree.DCErroneous;
import com.sun.tools.javac.tree.DCTree.DCIdentifier;
import com.sun.tools.javac.tree.DCTree.DCReference;
import com.sun.tools.javac.tree.DCTree.DCText;
import com.sun.tools.javac.tree.DocTreeMaker;
import com.sun.tools.javac.tree.EndPosTable;
import com.sun.tools.javac.tree.JCTree;
import com.sun.tools.javac.tree.JCTree.JCExpression;
import com.sun.tools.javac.tree.TreeScanner;
import com.sun.tools.javac.util.DiagnosticSource;
import com.sun.tools.javac.util.JCDiagnostic;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.ListBuffer;
import com.sun.tools.javac.util.Log;
import com.sun.tools.javac.util.Name;
import com.sun.tools.javac.util.Names;
import com.sun.tools.javac.util.Position;

import static com.sun.tools.javac.util.LayoutCharacters.*;

/**
 *
 *  <p><b>This is NOT part of any supported API.
 *  If you write code that depends on this, you do so at your own risk.
 *  This code and its internal interfaces are subject to change or
 *  deletion without notice.</b>
 */
public class DocCommentParser {
    static class ParseException extends Exception {
        private static final long serialVersionUID = 0;
        ParseException(String key) {
            super(key);
        }
    }

    final ParserFactory fac;
    private final EndPosTable ept;
    final DiagnosticSource diagSource;
    final Comment comment;
    final DocTreeMaker m;
    final Names names;

    BreakIterator sentenceBreaker;

    /** The input buffer, index of most recent character read,
     *  index of one past last character in buffer.
     */
    protected char[] buf;
    protected int bp;
    protected int buflen;

    /** The current character.
     */
    protected char ch;

    int textStart = -1;
    int lastNonWhite = -1;
    boolean newline = true;

    Map<Name, TagParser> tagParsers;

    private final boolean breakOnError;
    private ListBuffer<JCDiagnostic> errors = new ListBuffer<JCDiagnostic>();

    public DocCommentParser(ParserFactory fac, DiagnosticSource diagSource, Comment comment) {
        this(fac, false, new JavacParser.EmptyEndPosTable(null), diagSource, comment);
    }

    public DocCommentParser(ParserFactory fac, boolean breakOnError, EndPosTable ept, DiagnosticSource diagSource, Comment comment) {
        this.fac = fac;
        this.ept = ept;
        this.breakOnError = breakOnError;
        this.diagSource = diagSource;
        this.comment = comment;
        names = fac.names;
        m = fac.docTreeMaker;
        initTagParsers();
    }

    public DCDocComment parse() {
        errors.clear();

        String c = comment.getText();
        buf = new char[c.length() + 1];
        c.getChars(0, c.length(), buf, 0);
        buf[buf.length - 1] = EOI;
        buflen = buf.length - 1;
        bp = -1;
        nextChar();

        List<DCTree> body = blockContent();
        List<DCTree> tags = blockTags();
        int pos = !body.isEmpty()
                ? body.head.pos
                : !tags.isEmpty() ? tags.head.pos : Position.NOPOS;

        DCDocComment dc = m.at(pos).newDocCommentTree(comment, body, tags);

        dc.errors = errors.toList();

        return dc;
    }

    void nextChar() {
        ch = buf[bp < buflen ? ++bp : buflen];
        switch (ch) {
            case '\f': case '\n': case '\r':
                newline = true;
        }
    }

    char peekNextChar() {
        return buf[bp < buflen ? bp + 1 : buflen];
    }

    /**
     * Read block content, consisting of text, html and inline tags.
     * Terminated by the end of input, or the beginning of the next block tag:
     * i.e. @ as the first non-whitespace character on a line.
     */
    @SuppressWarnings("fallthrough")
    protected List<DCTree> blockContent() {
        ListBuffer<DCTree> trees = new ListBuffer<>();
        textStart = -1;

        loop:
        while (bp < buflen) {
            switch (ch) {
                case '\n': case '\r': case '\f':
                    newline = true;
                    // fallthrough

                case ' ': case '\t':
                    nextChar();
                    break;

                case '&':
                    entity(trees);
                    break;

                case '<':
                    newline = false;
                    addPendingText(trees, bp - 1);
                    trees.add(html());
                    if (textStart == -1) {
                        textStart = bp;
                        lastNonWhite = -1;
                    }
                    break;

                case '>':
                    newline = false;
                    addPendingText(trees, bp - 1);
                    trees.add(m.at(bp).newErroneousTree(newString(bp, bp + 1), diagSource, "dc.bad.gt"));
                    nextChar();
                    if (textStart == -1) {
                        textStart = bp;
                        lastNonWhite = -1;
                    }
                    break;

                case '{':
                    inlineTag(trees);
                    break;

                case '@':
                    if (newline) {
                        addPendingText(trees, lastNonWhite);
                        break loop;
                    }
                    // fallthrough

                default:
                    newline = false;
                    if (textStart == -1)
                        textStart = bp;
                    lastNonWhite = bp;
                    nextChar();
            }
        }

        if (lastNonWhite != -1)
            addPendingText(trees, lastNonWhite);

        return trees.toList();
    }

    /**
     * Read a series of block tags, including their content.
     * Standard tags parse their content appropriately.
     * Non-standard tags are represented by {@link UnknownBlockTag}.
     */
    protected List<DCTree> blockTags() {
        ListBuffer<DCTree> tags = new ListBuffer<>();
        while (ch == '@')
            tags.add(blockTag());
        return tags.toList();
    }

    /**
     * Read a single block tag, including its content.
     * Standard tags parse their content appropriately.
     * Non-standard tags are represented by {@link UnknownBlockTag}.
     */
    protected DCTree blockTag() {
        int p = bp;
        try {
            nextChar();
            if (isIdentifierStart(ch)) {
                Name name = readTagName();
                TagParser tp = tagParsers.get(name);
                if (tp == null) {
                    List<DCTree> content = blockContent();
                    return m.at(p).newUnknownBlockTagTree(name, content);
                } else {
                    switch (tp.getKind()) {
                        case BLOCK:
                            return tp.parse(p);
                        case INLINE:
                            handleError("dc.bad.inline.tag", p);
                            return tp.parse(p);
                    }
                }
            }
            DCErroneous err = erroneous("dc.no.tag.name", p);

            if (breakOnError) {
                return err;
            }

            List<DCTree> content = blockContent();
            return m.at(p).newUnknownBlockTagTree(names.empty, content);
        } catch (ParseException e) {
            if (!breakOnError) throw new IllegalStateException(e);//should not happen
            blockContent();
            return erroneous(e.getMessage(), p);
        }
    }

    protected void inlineTag(ListBuffer<DCTree> list) {
        newline = false;
        nextChar();
        if (ch == '@') {
            addPendingText(list, bp - 2);
            list.add(inlineTag());
            textStart = bp;
            lastNonWhite = -1;
        } else {
            if (textStart == -1)
                textStart = bp - 1;
            lastNonWhite = bp;
        }
    }

    /**
     * Read a single inline tag, including its content.
     * Standard tags parse their content appropriately.
     * Non-standard tags are represented by {@link UnknownBlockTag}.
     * Malformed tags may be returned as {@link Erroneous}.
     */
    protected DCTree inlineTag() {
        int p = bp - 1;
        try {
            nextChar();
            if (isIdentifierStart(ch)) {
                Name name = readTagName();
                TagParser tp = tagParsers.get(name);

                if (tp == null) {
                    skipWhitespace();
                    DCTree text = inlineText(WhitespaceRetentionPolicy.REMOVE_ALL);
                    if (text != null) {
                        return m.at(p).newUnknownInlineTagTree(name, List.of(text)).setEndPos(bp);
                    }
                } else {
                    if (!tp.retainWhiteSpace) {
                        skipWhitespace();
                    }
                    if (tp.getKind() == TagParser.Kind.INLINE) {
                        DCEndPosTree<?> tree = (DCEndPosTree<?>) tp.parse(p);
                        if (tree != null) {
                            return tree.setEndPos(bp);
                        }
                    } else { // handle block tags (ex: @see) in inline content
                        inlineText(WhitespaceRetentionPolicy.REMOVE_ALL); // skip content
                    }
                }
            }

            DCErroneous err = erroneous("dc.no.tag.name", p);

            if (breakOnError) return err;

            List<DCTree> content = inlineContent();
            return m.at(p).newUnknownInlineTagTree(names.empty, content);
        } catch (ParseException e) {
            if (!breakOnError) throw new IllegalStateException(e);
            return erroneous(e.getMessage(), p);
        }
    }

    private static enum WhitespaceRetentionPolicy {
        RETAIN_ALL,
        REMOVE_FIRST_SPACE,
        REMOVE_ALL
    }

    /**
     * Read plain text content of an inline tag.
     * Matching pairs of { } are skipped; the text is terminated by the first
     * unmatched }. It is an error if the beginning of the next tag is detected.
     */
    private DCTree inlineText(WhitespaceRetentionPolicy whitespacePolicy) throws ParseException {
        switch (whitespacePolicy) {
            case REMOVE_ALL:
                skipWhitespace();
                break;
            case REMOVE_FIRST_SPACE:
                if (ch == ' ')
                    nextChar();
                break;
            case RETAIN_ALL:
            default:
                // do nothing
                break;

        }
        int pos = bp;
        int depth = 1;

        loop:
        while (bp < buflen) {
            switch (ch) {
                case '\n': case '\r': case '\f':
                    newline = true;
                    break;

                case ' ': case '\t':
                    break;

                case '{':
                    newline = false;
                    lastNonWhite = bp;
                    depth++;
                    break;

                case '}':
                    if (--depth == 0) {
                        DCText result = m.at(pos).newTextTree(newString(pos, bp));
                        nextChar();
                        return result;
                    }
                    newline = false;
                    lastNonWhite = bp;
                    break;

                case '@':
                    if (newline)
                        break loop;
                    newline = false;
                    lastNonWhite = bp;
                    break;

                default:
                    newline = false;
                    lastNonWhite = bp;
                    break;
            }
            nextChar();
        }

        handleError("dc.unterminated.inline.tag", pos);

        return m.at(pos).newTextTree(newString(pos, bp));
    }

    protected void handleError(String errorKey, int pos) throws ParseException {
        if (breakOnError)
            throw new ParseException(errorKey);
        else {
            DCErroneous err = erroneous(errorKey, pos);
            errors.add(err.diag);
        }
    }

    /**
     * Read Java class name, possibly followed by member
     * Matching pairs of {@literal < >} are skipped. The text is terminated by the first
     * unmatched }. It is an error if the beginning of the next tag is detected.
     */
    // TODO: boolean allowMember should be enum FORBID, ALLOW, REQUIRE
    // TODO: improve quality of parse to forbid bad constructions.
    // TODO: update to use ReferenceParser
    @SuppressWarnings("fallthrough")
    protected DCReference reference(boolean allowMember) throws ParseException {
        int pos = bp;
        int depth = 0;

        // scan to find the end of the signature, by looking for the first
        // whitespace not enclosed in () or <>, or the end of the tag
        loop:
        while (bp < buflen) {
            switch (ch) {
                case '\n': case '\r': case '\f':
                    newline = true;
                    // fallthrough

                case ' ': case '\t':
                    if (depth == 0)
                        break loop;
                    break;

                case '(':
                case '<':
                    newline = false;
                    depth++;
                    break;

                case ')':
                case '>':
                    newline = false;
                    --depth;
                    break;

                case '}':
                    if (bp == pos)
                        return null;
                    newline = false;
                    break loop;

                case '@':
                    if (newline)
                        break loop;
                    // fallthrough

                case '{':
                    break loop;
                default:
                    newline = false;

            }
            nextChar();
        }

        if (depth != 0) {
            handleError("dc.unterminated.signature", pos);
        }

        String sig = newString(pos, bp);

        // Break sig apart into qualifiedExpr member paramTypes.
        JCExpression qualExpr;
        Name member;
        List<JCTree> paramTypes;

        Log.DeferredDiagnosticHandler deferredDiagnosticHandler
                = new Log.DeferredDiagnosticHandler(fac.log);

        try {
            int hash = sig.indexOf("#");
            int lparen = sig.indexOf("(", hash + 1);
            if (hash == -1) {
                if (lparen == -1) {
                    qualExpr = parseType(sig, comment.getSourcePos(pos));
                    member = null;
                } else {
                    qualExpr = null;
                    member = parseMember(sig.substring(0, lparen));
                }
            } else {
                qualExpr = (hash == 0) ? null : parseType(sig.substring(0, hash), comment.getSourcePos(pos));
                if (lparen == -1)
                    member = parseMember(sig.substring(hash + 1));
                else
                    member = parseMember(sig.substring(hash + 1, lparen));
            }

            if (lparen < 0) {
                paramTypes = null;
            } else {
                int rparen = sig.indexOf(")", lparen);
                if (rparen != sig.length() - 1) {
                    handleError("dc.ref.bad.parens", pos);
                    if (rparen == (-1)) {
                        rparen = sig.length();
                    }
                }
                paramTypes = parseParams(sig.substring(lparen + 1, rparen), comment.getSourcePos(pos + lparen + 1));
            }

            if (!deferredDiagnosticHandler.getDiagnostics().isEmpty()) {
                handleError("dc.ref.syntax.error", pos);
            }
        } finally {
            fac.log.popDiagnosticHandler(deferredDiagnosticHandler);
        }

        return m.at(pos).newReferenceTree(sig, qualExpr, member, paramTypes).setEndPos(bp);
    }

    JCExpression parseType(String s, int startPos) throws ParseException {
        JavacParser p = fac.newParser(s, false, true, false);
        JCExpression tree = p.parseType();
        moveTree(startPos, tree, p, ept);
        if (p.token().kind != TokenKind.EOF) {
            handleError("dc.ref.unexpected.input", p.token().pos);
        }
        return tree;
    }

    private <T extends JCTree> T moveTree(final int offset, T toMove, final JavacParser parser, final EndPosTable targetEndPos) {
        new TreeScanner() {
            @Override public void scan(JCTree tree) {
                if (tree != null) {
                    int endPos = parser.getEndPos(tree) + offset;
                    tree.pos += offset;
                    ((AbstractEndPosTable) targetEndPos).storeEnd(tree, endPos);
                }
                super.scan(tree);
            }
        }.scan(toMove);

        return toMove;
    }

    Name parseMember(String s) throws ParseException {
        JavacParser p = fac.newParser(s, false, false, false);
        Name name = p.ident();
        if (p.token().kind != TokenKind.EOF) {
            handleError("dc.ref.unexpected.input", bp);
            return names.error;
        }
        return name;
    }

    List<JCTree> parseParams(String s, int startPos) throws ParseException {
        if (s.trim().isEmpty())
            return List.nil();

        JavacParser p = fac.newParser(s.replace("...", "[ ]"), false, true, false);
        ListBuffer<JCTree> paramTypes = new ListBuffer<>();
        paramTypes.add(moveTree(startPos, p.parseType(), p, ept));

        if (p.token().kind == TokenKind.IDENTIFIER)
            p.nextToken();

        while (p.token().kind == TokenKind.COMMA) {
            p.nextToken();
            paramTypes.add(moveTree(startPos, p.parseType(), p, ept));

            if (p.token().kind == TokenKind.IDENTIFIER)
                p.nextToken();
        }

        if (p.token().kind != TokenKind.EOF) {
            handleError("dc.ref.unexpected.input", bp);
        }

        return paramTypes.toList();
    }

    /**
     * Read Java identifier
     * Matching pairs of { } are skipped; the text is terminated by the first
     * unmatched }. It is an error if the beginning of the next tag is detected.
     */
    @SuppressWarnings("fallthrough")
    protected DCIdentifier identifier() throws ParseException {
        skipWhitespace();
        int pos = bp;

        if (isJavaIdentifierStart(ch)) {
            Name name = readJavaIdentifier();
            return m.at(pos).newIdentifierTree(name);
        }

        handleError("dc.identifier.expected", pos);
        return m.at(pos).newIdentifierTree(names.error);
    }

    /**
     * Read a quoted string.
     * It is an error if the beginning of the next tag is detected.
     */
    @SuppressWarnings("fallthrough")
    protected DCText quotedString() {
        int pos = bp;
        nextChar();

        loop:
        while (bp < buflen) {
            switch (ch) {
                case '\n': case '\r': case '\f':
                    newline = true;
                    break;

                case ' ': case '\t':
                    break;

                case '"':
                    nextChar();
                    // trim trailing white-space?
                    return m.at(pos).newTextTree(newString(pos, bp));

                case '@':
                    if (newline)
                        break loop;

            }
            nextChar();
        }
        return null;
    }

    /**
     * Read a term ie. one word.
     * It is an error if the beginning of the next tag is detected.
     */
    @SuppressWarnings("fallthrough")
    protected DCText inlineWord() {
        int pos = bp;
        int depth = 0;
        loop:
        while (bp < buflen) {
            switch (ch) {
                case '\n':
                    newline = true;
                    // fallthrough

                case '\r': case '\f': case ' ': case '\t':
                    return m.at(pos).newTextTree(newString(pos, bp));

                case '@':
                    if (newline)
                        break loop;

                case '{':
                    depth++;
                    break;

                case '}':
                    if (depth == 0 || --depth == 0)
                        return m.at(pos).newTextTree(newString(pos, bp));
                    break;
            }
            newline = false;
            nextChar();
        }
        return null;
    }

    /**
     * Read general text content of an inline tag, including HTML entities and elements.
     * Matching pairs of { } are skipped; the text is terminated by the first
     * unmatched }. It is an error if the beginning of the next tag is detected.
     */
    @SuppressWarnings("fallthrough")
    private List<DCTree> inlineContent() {
        ListBuffer<DCTree> trees = new ListBuffer<>();

        skipWhitespace();
        int pos = bp;
        int depth = 1;
        textStart = -1;

        loop:
        while (bp < buflen) {

            switch (ch) {
                case '\n': case '\r': case '\f':
                    newline = true;
                    // fall through

                case ' ': case '\t':
                    nextChar();
                    break;

                case '&':
                    entity(trees);
                    break;

                case '<':
                    newline = false;
                    addPendingText(trees, bp - 1);
                    trees.add(html());
                    break;

                case '{':
                    if (textStart == -1)
                        textStart = bp;
                    newline = false;
                    if (peekNextChar() == '@') {
                        addPendingText(trees, bp - 1);
                        pos = bp;
                        break loop;
                    }
                    nextChar();
                    depth++;
                    break;

                case '}':
                    newline = false;
                    if (--depth == 0) {
                        addPendingText(trees, bp - 1);
                        nextChar();
                        return trees.toList();
                    }
                    nextChar();
                    break;

                case '@':
                    if (newline)
                        break loop;
                    // fallthrough

                default:
                    if (textStart == -1)
                        textStart = bp;
                    nextChar();
                    break;
            }
        }

        DCErroneous err = erroneous("dc.unterminated.inline.tag", pos);

        if (!breakOnError) {
            return trees.toList();
        }

        return List.of(err);
    }

    protected void entity(ListBuffer<DCTree> list) {
        newline = false;
        addPendingText(list, bp - 1);
        list.add(entity());
        if (textStart == -1) {
            textStart = bp;
            lastNonWhite = -1;
        }
    }

    /**
     * Read an HTML entity.
     * {@literal &identifier; } or {@literal &#digits; } or {@literal &#xhex-digits; }
     */
    protected DCTree entity() {
        int p = bp;
        nextChar();
        Name name = null;
        if (ch == '#') {
            int namep = bp;
            nextChar();
            if (isDecimalDigit(ch)) {
                nextChar();
                while (isDecimalDigit(ch))
                    nextChar();
                name = names.fromChars(buf, namep, bp - namep);
            } else if (ch == 'x' || ch == 'X') {
                nextChar();
                if (isHexDigit(ch)) {
                    nextChar();
                    while (isHexDigit(ch))
                        nextChar();
                    name = names.fromChars(buf, namep, bp - namep);
                }
            }
        } else if (isIdentifierStart(ch)) {
            name = readIdentifier();
        }

        if (name == null) {
            DCErroneous err = erroneous("dc.bad.entity", p);

            if (breakOnError) {
                return err;
            } else {
                name = names.empty;
            }
        } else {
            if (ch != ';') {
                DCErroneous err = erroneous("dc.missing.semicolon", p);

                if (breakOnError) {
                    return err;
                }
            } else {
                nextChar();
            }
        }
        return m.at(p).newEntityTree(name);
    }

    /**
     * Read the start or end of an HTML tag, or an HTML comment
     * {@literal <identifier attrs> } or {@literal </identifier> }
     */
    protected DCTree html() {
        int p = bp;
        nextChar();
        if (isIdentifierStart(ch)) {
            Name name = readIdentifier();
            List<DCTree> attrs = htmlAttrs();
            if (attrs != null) {
                boolean selfClosing = false;
                if (ch == '/') {
                    nextChar();
                    selfClosing = true;
                }
                if (ch == '>') {
                    nextChar();
                    DCTree dctree = m.at(p).newStartElementTree(name, attrs, selfClosing).setEndPos(bp);
                    return dctree;
                }
            }
        } else if (ch == '/') {
            nextChar();
            if (isIdentifierStart(ch)) {
                Name name = readIdentifier();
                skipWhitespace();
                if (ch == '>') {
                    nextChar();
                    return m.at(p).newEndElementTree(name);
                }
            }
        } else if (ch == '!') {
            nextChar();
            if (ch == '-') {
                nextChar();
                if (ch == '-') {
                    nextChar();
                    while (bp < buflen) {
                        int dash = 0;
                        while (ch == '-') {
                            dash++;
                            nextChar();
                        }
                        // Strictly speaking, a comment should not contain "--"
                        // so dash > 2 is an error, dash == 2 implies ch == '>'
                        // See http://www.w3.org/TR/html-markup/syntax.html#syntax-comments
                        // for more details.
                        if (dash >= 2 && ch == '>') {
                            nextChar();
                            return m.at(p).newCommentTree(newString(p, bp));
                        }

                        nextChar();
                    }
                }
            }
        }

        bp = p + 1;
        ch = buf[bp];
        return erroneous("dc.malformed.html", p);//XXX: jlahoda: the span is incorrect
    }

    /**
     * Read a series of HTML attributes, terminated by {@literal > }.
     * Each attribute is of the form {@literal identifier[=value] }.
     * "value" may be unquoted, single-quoted, or double-quoted.
     */
    protected List<DCTree> htmlAttrs() {
        ListBuffer<DCTree> attrs = new ListBuffer<>();
        skipWhitespace();

        loop:
        while (isIdentifierStart(ch)) {
            int namePos = bp;
            Name name = readAttributeName();
            skipWhitespace();
            List<DCTree> value = null;
            ValueKind vkind = ValueKind.EMPTY;
            if (ch == '=') {
                ListBuffer<DCTree> v = new ListBuffer<>();
                nextChar();
                skipWhitespace();
                if (ch == '\'' || ch == '"') {
                    vkind = (ch == '\'') ? ValueKind.SINGLE : ValueKind.DOUBLE;
                    char quote = ch;
                    nextChar();
                    textStart = bp;
                    while (bp < buflen && ch != quote) {
                        if (newline && ch == '@') {
                            DCErroneous err = erroneous("dc.unterminated.string", namePos);
                            if (breakOnError) {
                                attrs.add(err);
                            } else {
                                attrValueChar(v);
                                attrs.add(m.at(namePos).newAttributeTree(name, vkind, v.toList()));
                            }
                            // No point trying to read more.
                            // In fact, all attrs get discarded by the caller
                            // and superseded by a malformed.html node because
                            // the html tag itself is not terminated correctly.
                            break loop;
                        }
                        attrValueChar(v);
                    }
                    addPendingText(v, bp - 1);
                    nextChar();
                } else {
                    vkind = ValueKind.UNQUOTED;
                    textStart = bp;
                    while (bp < buflen && !isUnquotedAttrValueTerminator(ch)) {
                        attrValueChar(v);
                    }
                    addPendingText(v, bp - 1);
                }
                skipWhitespace();
                value = v.toList();
            }
            DCAttribute attr = m.at(namePos).newAttributeTree(name, vkind, value);
            attrs.add(attr);
        }

        return attrs.toList();
    }

    protected void attrValueChar(ListBuffer<DCTree> list) {
        switch (ch) {
            case '&':
                entity(list);
                break;

            case '{':
                inlineTag(list);
                break;

            default:
                nextChar();
        }
    }

    protected void addPendingText(ListBuffer<DCTree> list, int textEnd) {
        if (textStart != -1) {
            if (textStart <= textEnd) {
                list.add(m.at(textStart).newTextTree(newString(textStart, textEnd + 1)));
            }
            textStart = -1;
        }
    }

    protected DCErroneous erroneous(String code, int pos) {
        int i = bp - 1;
        loop:
        while (i > pos) {
            switch (buf[i]) {
                case '\f': case '\n': case '\r':
                    newline = true;
                    break;
                case '\t': case ' ':
                    break;
                default:
                    break loop;
            }
            i--;
        }
        textStart = -1;
        DCErroneous result = m.at(pos).newErroneousTree(newString(pos, i + 1), diagSource, code);
        if (!breakOnError) errors.add(result.diag);
        return result;
    }

    protected boolean isIdentifierStart(char ch) {
        return Character.isUnicodeIdentifierStart(ch);
    }

    protected Name readIdentifier() {
        int start = bp;
        nextChar();
        while (bp < buflen && Character.isUnicodeIdentifierPart(ch))
            nextChar();
        return names.fromChars(buf, start, bp - start);
    }

    protected Name readAttributeName() {
        int start = bp;
        nextChar();
        while (bp < buflen && (Character.isUnicodeIdentifierPart(ch) || ch == '-'))
            nextChar();
        return names.fromChars(buf, start, bp - start);
    }

    protected Name readTagName() {
        int start = bp;
        nextChar();
        while (bp < buflen
                && (Character.isUnicodeIdentifierPart(ch) || ch == '.'
                || ch == '-' || ch == ':')) {
            nextChar();
        }
        return names.fromChars(buf, start, bp - start);
    }

    protected boolean isJavaIdentifierStart(char ch) {
        return Character.isJavaIdentifierStart(ch);
    }

    protected Name readJavaIdentifier() {
        int start = bp;
        nextChar();
        while (bp < buflen && Character.isJavaIdentifierPart(ch))
            nextChar();
        return names.fromChars(buf, start, bp - start);
    }

    protected boolean isDecimalDigit(char ch) {
        return ('0' <= ch && ch <= '9');
    }

    protected boolean isHexDigit(char ch) {
        return ('0' <= ch && ch <= '9')
                || ('a' <= ch && ch <= 'f')
                || ('A' <= ch && ch <= 'F');
    }

    protected boolean isUnquotedAttrValueTerminator(char ch) {
        switch (ch) {
            case '\f': case '\n': case '\r': case '\t':
            case ' ':
            case '"': case '\'': case '`':
            case '=': case '<': case '>':
                return true;
            default:
                return false;
        }
    }

    protected boolean isWhitespace(char ch) {
        return Character.isWhitespace(ch);
    }

    protected void skipWhitespace() {
        while (isWhitespace(ch)) {
            nextChar();
        }
    }

    /**
     * @param start position of first character of string
     * @param end position of character beyond last character to be included
     */
    String newString(int start, int end) {
        return new String(buf, start, end - start);
    }

    static abstract class TagParser {
        enum Kind { INLINE, BLOCK }

        final Kind kind;
        final DCTree.Kind treeKind;
        final boolean retainWhiteSpace;


        TagParser(Kind k, DCTree.Kind tk) {
            kind = k;
            treeKind = tk;
            retainWhiteSpace = false;
        }

        TagParser(Kind k, DCTree.Kind tk, boolean retainWhiteSpace) {
            kind = k;
            treeKind = tk;
            this.retainWhiteSpace = retainWhiteSpace;
        }

        Kind getKind() {
            return kind;
        }

        DCTree.Kind getTreeKind() {
            return treeKind;
        }

        abstract DCTree parse(int pos) throws ParseException;
    }

    /**
     * @see <a href="http://docs.oracle.com/javase/8/docs/technotes/tools/unix/javadoc.html#CHDJGIJB">Javadoc Tags</a>
     */
    private void initTagParsers() {
        TagParser[] parsers = {
            // @author name-text
            new TagParser(Kind.BLOCK, DCTree.Kind.AUTHOR) {
                public DCTree parse(int pos) {
                    List<DCTree> name = blockContent();
                    return m.at(pos).newAuthorTree(name);
                }
            },

            // {@code text}
            new TagParser(Kind.INLINE, DCTree.Kind.CODE, true) {
                public DCTree parse(int pos) throws ParseException {
                    DCTree text = inlineText(WhitespaceRetentionPolicy.REMOVE_FIRST_SPACE);
                    return m.at(pos).newCodeTree((DCText) text);
                }
            },

            // @deprecated deprecated-text
            new TagParser(Kind.BLOCK, DCTree.Kind.DEPRECATED) {
                public DCTree parse(int pos) {
                    List<DCTree> reason = blockContent();
                    return m.at(pos).newDeprecatedTree(reason);
                }
            },

            // {@docRoot}
            new TagParser(Kind.INLINE, DCTree.Kind.DOC_ROOT) {
                public DCTree parse(int pos) throws ParseException {
                    if (ch == '}' || !breakOnError) {
                        if (ch == '}') {
                            nextChar();
                        } else {
                            erroneous("dc.unexpected.content", bp);//TODO: should rather be '}' expected
                        }
                        return m.at(pos).newDocRootTree();
                    }
                    inlineText(WhitespaceRetentionPolicy.REMOVE_ALL); // skip unexpected content
                    throw new ParseException("dc.unexpected.content");
                }
            },

            // @exception class-name description
            new TagParser(Kind.BLOCK, DCTree.Kind.EXCEPTION) {
                public DCTree parse(int pos) throws ParseException {
                    skipWhitespace();
                    DCReference ref = reference(false);
                    List<DCTree> description = blockContent();
                    return m.at(pos).newExceptionTree(ref, description);
                }
            },

            // @hidden hidden-text
            new TagParser(Kind.BLOCK, DCTree.Kind.HIDDEN) {
                public DCTree parse(int pos) {
                    List<DCTree> reason = blockContent();
                    return m.at(pos).newHiddenTree(reason);
                }
            },

            // @index search-term options-description
            new TagParser(Kind.INLINE, DCTree.Kind.INDEX) {
                public DCTree parse(int pos) throws ParseException {
                    skipWhitespace();
                    if (ch == '}') {
                        throw new ParseException("dc.no.content");
                    }
                    DCTree term = ch == '"' ? quotedString() : inlineWord();
                    if (term == null) {
                        throw new ParseException("dc.no.content");
                    }
                    skipWhitespace();
                    List<DCTree> description = List.nil();
                    if (ch != '}') {
                        description = inlineContent();
                    } else {
                        nextChar();
                    }
                    return m.at(pos).newIndexTree(term, description);
                }
            },

            // {@inheritDoc}
            new TagParser(Kind.INLINE, DCTree.Kind.INHERIT_DOC) {
                public DCTree parse(int pos) throws ParseException {
                    if (ch == '}' || !breakOnError) {
                        if (ch == '}') {
                            nextChar();
                        } else {
                            erroneous("dc.unexpected.content", bp);//TODO: should rather be '}' expected
                        }
                        return m.at(pos). newInheritDocTree();
                    }
                    inlineText(WhitespaceRetentionPolicy.REMOVE_ALL); // skip unexpected content
                    throw new ParseException("dc.unexpected.content");
                }
            },

            // {@link package.class#member label}
            new TagParser(Kind.INLINE, DCTree.Kind.LINK) {
                public DCTree parse(int pos) throws ParseException {
                    DCReference ref = reference(true);
                    List<DCTree> label = inlineContent();
                    return m.at(pos).newLinkTree(ref, label);
                }
            },

            // {@linkplain package.class#member label}
            new TagParser(Kind.INLINE, DCTree.Kind.LINK_PLAIN) {
                public DCTree parse(int pos) throws ParseException {
                    DCReference ref = reference(true);
                    List<DCTree> label = inlineContent();
                    return m.at(pos).newLinkPlainTree(ref, label);
                }
            },

            // {@literal text}
            new TagParser(Kind.INLINE, DCTree.Kind.LITERAL, true) {
                public DCTree parse(int pos) throws ParseException {
                    DCTree text = inlineText(WhitespaceRetentionPolicy.REMOVE_FIRST_SPACE);
                    return m.at(pos).newLiteralTree((DCText) text);
                }
            },

            // @param parameter-name description
            new TagParser(Kind.BLOCK, DCTree.Kind.PARAM) {
                public DCTree parse(int pos) throws ParseException {
                    skipWhitespace();

                    boolean typaram = false;
                    if (ch == '<') {
                        typaram = true;
                        nextChar();
                    }

                    DCIdentifier id = identifier();

                    if (typaram) {
                        if (ch != '>') {
                            handleError("dc.gt.expected", bp);
                        }
                        nextChar();
                    }

                    skipWhitespace();
                    List<DCTree> desc = blockContent();
                    return m.at(pos).newParamTree(typaram, id, desc);
                }
            },

            // @provides service-name description
            new TagParser(Kind.BLOCK, DCTree.Kind.PROVIDES) {
                public DCTree parse(int pos) throws ParseException {
                    skipWhitespace();
                    DCReference ref = reference(true);
                    List<DCTree> description = blockContent();
                    return m.at(pos).newProvidesTree(ref, description);
                }
            },

            // @return description
            new TagParser(Kind.BLOCK, DCTree.Kind.RETURN) {
                public DCTree parse(int pos) {
                    List<DCTree> description = blockContent();
                    return m.at(pos).newReturnTree(description);
                }
            },

            // @see reference | quoted-string | HTML
            new TagParser(Kind.BLOCK, DCTree.Kind.SEE) {
                public DCTree parse(int pos) throws ParseException {
                    skipWhitespace();
                    String errorKey = "dc.unexpected.content";
                    switch (ch) {
                        case '"':
                            DCText string = quotedString();
                            if (string != null) {
                                skipWhitespace();
                                if (ch == '@'
                                        || ch == EOI && bp == buf.length - 1) {
                                    return m.at(pos).newSeeTree(List.<DCTree>of(string));
                                }
                            }
                            break;

                        case '<':
                            List<DCTree> html = blockContent();
                            if (html != null)
                                return m.at(pos).newSeeTree(html);
                            break;

                        case '@':
                            if (newline) {
                                handleError("dc.no.content", bp);
                            }
                            break;

                        case EOI:
                            if (bp == buf.length - 1)
                                errorKey = "dc.no.content";
                            break;

                        default:
                            if (isJavaIdentifierStart(ch) || ch == '#') {
                                DCReference ref = reference(true);
                                List<DCTree> description = blockContent();
                                return m.at(pos).newSeeTree(description.prepend(ref));
                            }
                    }

                    handleError(errorKey, bp);

                    return m.at(pos).newSeeTree(List.<DCTree>nil());
                }
            },

            // @serialData data-description
            new TagParser(Kind.BLOCK, DCTree.Kind.SERIAL_DATA) {
                public DCTree parse(int pos) {
                    List<DCTree> description = blockContent();
                    return m.at(pos).newSerialDataTree(description);
                }
            },

            // @serialField field-name field-type description
            new TagParser(Kind.BLOCK, DCTree.Kind.SERIAL_FIELD) {
                public DCTree parse(int pos) throws ParseException {
                    skipWhitespace();
                    DCIdentifier name = identifier();
                    skipWhitespace();
                    DCReference type = reference(false);
                    List<DCTree> description = null;
                    if (isWhitespace(ch)) {
                        skipWhitespace();
                        description = blockContent();
                    }
                    return m.at(pos).newSerialFieldTree(name, type, description);
                }
            },

            // @serial field-description | include | exclude
            new TagParser(Kind.BLOCK, DCTree.Kind.SERIAL) {
                public DCTree parse(int pos) {
                    List<DCTree> description = blockContent();
                    return m.at(pos).newSerialTree(description);
                }
            },

            // @since since-text
            new TagParser(Kind.BLOCK, DCTree.Kind.SINCE) {
                public DCTree parse(int pos) {
                    List<DCTree> description = blockContent();
                    return m.at(pos).newSinceTree(description);
                }
            },

            // @summary summary-text
            new TagParser(Kind.INLINE, DCTree.Kind.SUMMARY) {
                public DCTree parse(int pos) throws ParseException {
                    List<DCTree> summary = inlineContent();
                    return m.at(pos).newSummaryTree(summary);
                }
            },

            // @throws class-name description
            new TagParser(Kind.BLOCK, DCTree.Kind.THROWS) {
                public DCTree parse(int pos) throws ParseException {
                    skipWhitespace();
                    DCReference ref = reference(false);
                    List<DCTree> description = blockContent();
                    return m.at(pos).newThrowsTree(ref, description);
                }
            },

            // @uses service-name description
            new TagParser(Kind.BLOCK, DCTree.Kind.USES) {
                public DCTree parse(int pos) throws ParseException {
                    skipWhitespace();
                    DCReference ref = reference(true);
                    List<DCTree> description = blockContent();
                    return m.at(pos).newUsesTree(ref, description);
                }
            },

            // {@value package.class#field}
            new TagParser(Kind.INLINE, DCTree.Kind.VALUE) {
                public DCTree parse(int pos) throws ParseException {
                    DCReference ref = reference(true);
                    skipWhitespace();
                    if (ch == '}') {
                        nextChar();
                    } else {
                        handleError("dc.unexpected.content", pos);//TODO: should rather be '}' expected
                    }
                    return m.at(pos).newValueTree(ref);
                }
            },

            // @version version-text
            new TagParser(Kind.BLOCK, DCTree.Kind.VERSION) {
                public DCTree parse(int pos) {
                    List<DCTree> description = blockContent();
                    return m.at(pos).newVersionTree(description);
                }
            },
        };

        tagParsers = new HashMap<>();
        for (TagParser p: parsers)
            tagParsers.put(names.fromString(p.getTreeKind().tagName), p);

    }
}
