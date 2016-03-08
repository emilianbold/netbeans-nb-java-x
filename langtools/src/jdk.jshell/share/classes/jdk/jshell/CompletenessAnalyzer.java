/*
 * Copyright (c) 2015, 2016, Oracle and/or its affiliates. All rights reserved.
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

package jdk.jshell;

import com.sun.tools.javac.code.Source;
import com.sun.tools.javac.parser.Scanner;
import com.sun.tools.javac.parser.ScannerFactory;
import com.sun.tools.javac.parser.Tokens.Token;
import com.sun.tools.javac.parser.Tokens.TokenKind;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.JCDiagnostic;
import com.sun.tools.javac.util.JCDiagnostic.DiagnosticFlag;
import com.sun.tools.javac.util.JCDiagnostic.DiagnosticPosition;
import com.sun.tools.javac.util.Log;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.EnumMap;
import java.util.Iterator;
import jdk.jshell.SourceCodeAnalysis.Completeness;
import com.sun.source.tree.Tree;
import static jdk.jshell.CompletenessAnalyzer.TK.*;
import jdk.jshell.TaskFactory.ParseTask;
import java.util.List;

/**
 * Low level scanner to determine completeness of input.
 * @author Robert Field
 */
class CompletenessAnalyzer {

    private final ScannerFactory scannerFactory;
    private final JShell proc;

    private static Completeness error() {
        return Completeness.UNKNOWN;  // For breakpointing
    }

    static class CaInfo {

        CaInfo(Completeness status, int unitEndPos) {
            this.status = status;
            this.unitEndPos = unitEndPos;
        }
        final int unitEndPos;
        final Completeness status;
    }

    CompletenessAnalyzer(JShell proc) {
        this.proc = proc;
        Context context = new Context();
        Log log = CaLog.createLog(context);
        context.put(Log.class, log);
        context.put(Source.class, Source.JDK1_9);
        scannerFactory = ScannerFactory.instance(context);
    }

    CaInfo scan(String s) {
        try {
            Scanner scanner = scannerFactory.newScanner(s, false);
            Matched in = new Matched(scanner);
            Parser parser = new Parser(in, proc, s);
            Completeness stat = parser.parseUnit();
            int endPos = stat == Completeness.UNKNOWN
                    ? s.length()
                    : in.prevCT.endPos;
            return new CaInfo(stat, endPos);
        } catch (SyntaxException ex) {
            return new CaInfo(error(), s.length());
        }
    }

    @SuppressWarnings("serial")             // serialVersionUID intentionally omitted
    private static class SyntaxException extends RuntimeException {
    }

    private static void die() {
        throw new SyntaxException();
    }

    /**
     * Subclass of Log used by compiler API to die on error and ignore
     * other messages
     */
    private static class CaLog extends Log {

        private static CaLog createLog(Context context) {
            PrintWriter pw = new PrintWriter(new StringWriter());
            CaLog log = new CaLog(context, pw, pw, pw);
            context.put(outKey, pw);
            context.put(logKey, log);
            return log;
        }

        private CaLog(Context context, PrintWriter errWriter, PrintWriter warnWriter, PrintWriter noticeWriter) {
            super(context, errWriter, warnWriter, noticeWriter);
        }

        @Override
        public void error(String key, Object... args) {
            die();
        }

        @Override
        public void error(DiagnosticPosition pos, String key, Object... args) {
            die();
        }

        @Override
        public void error(DiagnosticFlag flag, DiagnosticPosition pos, String key, Object... args) {
            die();
        }

        @Override
        public void error(int pos, String key, Object... args) {
            die();
        }

        @Override
        public void error(DiagnosticFlag flag, int pos, String key, Object... args) {
            die();
        }

        @Override
        public void report(JCDiagnostic diagnostic) {
            // Ignore
        }
    }

    // Location position kinds -- a token is ...
    private static final int XEXPR         = 0b1;                       // OK in expression (not first)
    private static final int XDECL         = 0b10;                      // OK in declaration (not first)
    private static final int XSTMT         = 0b100;                     // OK in statement framework (not first)
    private static final int XEXPR1o       = 0b1000;                    // OK first in expression
    private static final int XDECL1o       = 0b10000;                   // OK first in declaration
    private static final int XSTMT1o       = 0b100000;                  // OK first or only in statement framework
    private static final int XEXPR1        = XEXPR1o | XEXPR;           // OK in expression (anywhere)
    private static final int XDECL1        = XDECL1o | XDECL;           // OK in declaration (anywhere)
    private static final int XSTMT1        = XSTMT1o | XSTMT;           // OK in statement framework (anywhere)
    private static final int XANY1         = XEXPR1o | XDECL1o | XSTMT1o;  // Mask: first in statement, declaration, or expression
    private static final int XTERM         = 0b100000000;               // Can terminate (last before EOF)
    private static final int XSTART        = 0b1000000000;              // Boundary, must be XTERM before
    private static final int XERRO         = 0b10000000000;             // Is an error

    /**
     * An extension of the compiler's TokenKind which adds our combined/processed
     * kinds. Also associates each TK with a union of acceptable kinds of code
     * position it can occupy.  For example: IDENTIFER is XEXPR1|XDECL1|XTERM,
     * meaning it can occur in expressions or declarations (but not in the
     * framework of a statement and that can be the final (terminating) token
     * in a snippet.
     * <P>
     * There must be a TK defined for each compiler TokenKind, an exception
     * will
     * be thrown if a TokenKind is defined and a corresponding TK is not. Add a
     * new TK in the appropriate category. If it is like an existing category
     * (e.g. a new modifier or type this may be all that is needed.  If it
     * is bracketing or modifies the acceptable positions of other tokens,
     * please closely examine the needed changes to this scanner.
     */
    static enum TK {

        // Special
        EOF(TokenKind.EOF, 0),  //
        ERROR(TokenKind.ERROR, XERRO),  //
        IDENTIFIER(TokenKind.IDENTIFIER, XEXPR1|XDECL1|XTERM),  //
        UNDERSCORE(TokenKind.UNDERSCORE, XERRO),  //  _
        CLASS(TokenKind.CLASS, XEXPR|XDECL1|XTERM),  //  class decl and .class
        MONKEYS_AT(TokenKind.MONKEYS_AT, XEXPR|XDECL1),  //  @
        IMPORT(TokenKind.IMPORT, XDECL1|XSTART),  //  import -- consider declaration
        SEMI(TokenKind.SEMI, XSTMT1|XTERM|XSTART),  //  ;

        // Shouldn't see -- error
        PACKAGE(TokenKind.PACKAGE, XERRO),  //  package
        CONST(TokenKind.CONST, XERRO),  //  reserved keyword -- const
        GOTO(TokenKind.GOTO, XERRO),  //  reserved keyword -- goto
        CUSTOM(TokenKind.CUSTOM, XERRO),  // No uses

        // Declarations
        ENUM(TokenKind.ENUM, XDECL1),  //  enum
        IMPLEMENTS(TokenKind.IMPLEMENTS, XDECL),  //  implements
        INTERFACE(TokenKind.INTERFACE, XDECL1),  //  interface
        THROWS(TokenKind.THROWS, XDECL),  //  throws

        // Primarive type names
        BOOLEAN(TokenKind.BOOLEAN, XEXPR|XDECL1),  //  boolean
        BYTE(TokenKind.BYTE, XEXPR|XDECL1),  //  byte
        CHAR(TokenKind.CHAR, XEXPR|XDECL1),  //  char
        DOUBLE(TokenKind.DOUBLE, XEXPR|XDECL1),  //  double
        FLOAT(TokenKind.FLOAT, XEXPR|XDECL1),  //  float
        INT(TokenKind.INT, XEXPR|XDECL1),  //  int
        LONG(TokenKind.LONG, XEXPR|XDECL1),  //  long
        SHORT(TokenKind.SHORT, XEXPR|XDECL1),  //  short
        VOID(TokenKind.VOID, XEXPR|XDECL1),  //  void

        // Modifiers keywords
        ABSTRACT(TokenKind.ABSTRACT, XDECL1),  //  abstract
        FINAL(TokenKind.FINAL, XDECL1),  //  final
        NATIVE(TokenKind.NATIVE, XDECL1),  //  native
        STATIC(TokenKind.STATIC, XDECL1),  //  static
        STRICTFP(TokenKind.STRICTFP, XDECL1),  //  strictfp
        PRIVATE(TokenKind.PRIVATE, XDECL1),  //  private
        PROTECTED(TokenKind.PROTECTED, XDECL1),  //  protected
        PUBLIC(TokenKind.PUBLIC, XDECL1),  //  public
        TRANSIENT(TokenKind.TRANSIENT, XDECL1),  //  transient
        VOLATILE(TokenKind.VOLATILE, XDECL1),  //  volatile

        // Declarations and type parameters (thus expressions)
        EXTENDS(TokenKind.EXTENDS, XEXPR|XDECL),  //  extends
        COMMA(TokenKind.COMMA, XEXPR|XDECL),  //  ,
        AMP(TokenKind.AMP, XEXPR|XDECL),  //  &
        GT(TokenKind.GT, XEXPR|XDECL),  //  >
        LT(TokenKind.LT, XEXPR|XDECL1),  //  <
        LTLT(TokenKind.LTLT, XEXPR|XDECL1),  //  <<
        GTGT(TokenKind.GTGT, XEXPR|XDECL),  //  >>
        GTGTGT(TokenKind.GTGTGT, XEXPR|XDECL),  //  >>>
        QUES(TokenKind.QUES, XEXPR|XDECL),  //  ?
        DOT(TokenKind.DOT, XEXPR|XDECL),  //  .
        STAR(TokenKind.STAR, XEXPR|XDECL|XTERM),  //  * -- import foo.* //TODO handle these case separately, XTERM

        // Statement keywords
        ASSERT(TokenKind.ASSERT, XSTMT1|XSTART),  //  assert
        BREAK(TokenKind.BREAK, XSTMT1|XTERM|XSTART),  //  break
        CATCH(TokenKind.CATCH, XSTMT1|XSTART),  //  catch
        CONTINUE(TokenKind.CONTINUE, XSTMT1|XTERM|XSTART),  //  continue
        DO(TokenKind.DO, XSTMT1|XSTART),  //  do
        ELSE(TokenKind.ELSE, XSTMT1|XTERM|XSTART),  //  else
        FINALLY(TokenKind.FINALLY, XSTMT1|XSTART),  //  finally
        FOR(TokenKind.FOR, XSTMT1|XSTART),  //  for
        IF(TokenKind.IF, XSTMT1|XSTART),  //  if
        RETURN(TokenKind.RETURN, XSTMT1|XTERM|XSTART),  //  return
        SWITCH(TokenKind.SWITCH, XSTMT1|XSTART),  //  switch
        SYNCHRONIZED(TokenKind.SYNCHRONIZED, XSTMT1|XDECL),  //  synchronized
        THROW(TokenKind.THROW, XSTMT1|XSTART),  //  throw
        TRY(TokenKind.TRY, XSTMT1|XSTART),  //  try
        WHILE(TokenKind.WHILE, XSTMT1|XSTART),  //  while

        // Statement keywords that we shouldn't see -- inside braces
        CASE(TokenKind.CASE, XSTMT|XSTART),  //  case
        DEFAULT(TokenKind.DEFAULT, XSTMT|XSTART),  //  default method, default case -- neither we should see

        // Expressions (can terminate)
        INTLITERAL(TokenKind.INTLITERAL, XEXPR1|XTERM),  //
        LONGLITERAL(TokenKind.LONGLITERAL, XEXPR1|XTERM),  //
        FLOATLITERAL(TokenKind.FLOATLITERAL, XEXPR1|XTERM),  //
        DOUBLELITERAL(TokenKind.DOUBLELITERAL, XEXPR1|XTERM),  //
        CHARLITERAL(TokenKind.CHARLITERAL, XEXPR1|XTERM),  //
        STRINGLITERAL(TokenKind.STRINGLITERAL, XEXPR1|XTERM),  //
        TRUE(TokenKind.TRUE, XEXPR1|XTERM),  //  true
        FALSE(TokenKind.FALSE, XEXPR1|XTERM),  //  false
        NULL(TokenKind.NULL, XEXPR1|XTERM),  //  null
        THIS(TokenKind.THIS, XEXPR1|XTERM),  //  this  -- shouldn't see

        // Expressions maybe terminate  //TODO handle these case separately
        PLUSPLUS(TokenKind.PLUSPLUS, XEXPR1|XTERM),  //  ++
        SUBSUB(TokenKind.SUBSUB, XEXPR1|XTERM),  //  --

        // Expressions cannot terminate
        INSTANCEOF(TokenKind.INSTANCEOF, XEXPR),  //  instanceof
        NEW(TokenKind.NEW, XEXPR1),  //  new
        SUPER(TokenKind.SUPER, XEXPR1|XDECL),  //  super -- shouldn't see as rec. But in type parameters
        ARROW(TokenKind.ARROW, XEXPR),  //  ->
        COLCOL(TokenKind.COLCOL, XEXPR),  //  ::
        LPAREN(TokenKind.LPAREN, XEXPR),  //  (
        RPAREN(TokenKind.RPAREN, XEXPR),  //  )
        LBRACE(TokenKind.LBRACE, XEXPR),  //  {
        RBRACE(TokenKind.RBRACE, XEXPR),  //  }
        LBRACKET(TokenKind.LBRACKET, XEXPR),  //  [
        RBRACKET(TokenKind.RBRACKET, XEXPR),  //  ]
        ELLIPSIS(TokenKind.ELLIPSIS, XEXPR),  //  ...
        EQ(TokenKind.EQ, XEXPR),  //  =
        BANG(TokenKind.BANG, XEXPR1),  //  !
        TILDE(TokenKind.TILDE, XEXPR1),  //  ~
        COLON(TokenKind.COLON, XEXPR|XTERM),  //  :
        EQEQ(TokenKind.EQEQ, XEXPR),  //  ==
        LTEQ(TokenKind.LTEQ, XEXPR),  //  <=
        GTEQ(TokenKind.GTEQ, XEXPR),  //  >=
        BANGEQ(TokenKind.BANGEQ, XEXPR),  //  !=
        AMPAMP(TokenKind.AMPAMP, XEXPR),  //  &&
        BARBAR(TokenKind.BARBAR, XEXPR),  //  ||
        PLUS(TokenKind.PLUS, XEXPR1),  //  +
        SUB(TokenKind.SUB, XEXPR1),  //  -
        SLASH(TokenKind.SLASH, XEXPR),  //  /
        BAR(TokenKind.BAR, XEXPR),  //  |
        CARET(TokenKind.CARET, XEXPR),  //  ^
        PERCENT(TokenKind.PERCENT, XEXPR),  //  %
        PLUSEQ(TokenKind.PLUSEQ, XEXPR),  //  +=
        SUBEQ(TokenKind.SUBEQ, XEXPR),  //  -=
        STAREQ(TokenKind.STAREQ, XEXPR),  //  *=
        SLASHEQ(TokenKind.SLASHEQ, XEXPR),  //  /=
        AMPEQ(TokenKind.AMPEQ, XEXPR),  //  &=
        BAREQ(TokenKind.BAREQ, XEXPR),  //  |=
        CARETEQ(TokenKind.CARETEQ, XEXPR),  //  ^=
        PERCENTEQ(TokenKind.PERCENTEQ, XEXPR),  //  %=
        LTLTEQ(TokenKind.LTLTEQ, XEXPR),  //  <<=
        GTGTEQ(TokenKind.GTGTEQ, XEXPR),  //  >>=
        GTGTGTEQ(TokenKind.GTGTGTEQ, XEXPR),  //  >>>=

        // combined/processed kinds
        UNMATCHED(XERRO),
        PARENS(XEXPR1|XDECL|XSTMT|XTERM),
        BRACKETS(XEXPR|XDECL|XTERM),
        BRACES(XSTMT1|XEXPR|XTERM);

        static final EnumMap<TokenKind,TK> tokenKindToTKMap = new EnumMap<>(TokenKind.class);

        final TokenKind tokenKind;
        final int belongs;

        TK(int b) {
            this.tokenKind = null;
            this.belongs = b;
        }

        TK(TokenKind tokenKind, int b) {
            this.tokenKind = tokenKind;
            this.belongs = b;
        }

        private static TK tokenKindToTK(TokenKind kind) {
            TK tk = tokenKindToTKMap.get(kind);
            if (tk == null) {
                System.err.printf("No corresponding %s for %s: %s\n",
                        TK.class.getCanonicalName(),
                        TokenKind.class.getCanonicalName(),
                        kind);
                throw new InternalError("No corresponding TK for TokenKind: " + kind);
            }
            return tk;
        }

        boolean isOkToTerminate() {
            return (belongs & XTERM) != 0;
        }

        boolean isExpression() {
            return (belongs & XEXPR) != 0;
        }

        boolean isDeclaration() {
            return (belongs & XDECL) != 0;
        }

        boolean isError() {
            return (belongs & XERRO) != 0;
        }

        boolean isStart() {
            return (belongs & XSTART) != 0;
        }

        /**
         * After construction, check that all compiler TokenKind values have
         * corresponding TK values.
         */
        static {
            for (TK tk : TK.values()) {
                if (tk.tokenKind != null) {
                    tokenKindToTKMap.put(tk.tokenKind, tk);
                }
            }
            for (TokenKind kind : TokenKind.values()) {
                tokenKindToTK(kind); // assure they can be retrieved without error
            }
        }
    }

    /**
     * A completeness scanner token.
     */
    private static class CT {

        /** The token kind */
        public final TK kind;

        /** The end position of this token */
        public final int endPos;

        /** The error message **/
        public final String message;

        private CT(TK tk, Token tok, String msg) {
            this.kind = tk;
            this.endPos = tok.endPos;
            this.message = msg;
            //throw new InternalError(msg); /* for debugging */
        }

        private CT(TK tk, Token tok) {
            this.kind = tk;
            this.endPos = tok.endPos;
            this.message = null;
        }

        private CT(TK tk, int endPos) {
            this.kind = tk;
            this.endPos = endPos;
            this.message = null;
        }
    }

    /**
     * Look for matching tokens (like parens) and other special cases, like "new"
     */
    private static class Matched implements Iterator<CT> {

        private final Scanner scanner;
        private Token current;
        private CT prevCT;
        private CT currentCT;
        private final Deque<Token> stack = new ArrayDeque<>();

        Matched(Scanner scanner) {
            this.scanner = scanner;
            advance();
            prevCT = currentCT = new CT(SEMI, 0); // So is valid for testing
        }

        @Override
        public boolean hasNext() {
            return currentCT.kind != EOF;
        }

        private Token advance() {
            Token prev = current;
            scanner.nextToken();
            current = scanner.token();
            return prev;
        }

        @Override
        public CT next() {
            prevCT = currentCT;
            currentCT = nextCT();
            return currentCT;
        }

        private CT match(TK tk, TokenKind open) {
            Token tok = advance();
            db("match desired-tk=%s, open=%s, seen-tok=%s", tk, open, tok.kind);
            if (stack.isEmpty()) {
                return new CT(ERROR, tok, "Encountered '" + tok + "' with no opening '" + open + "'");
            }
            Token p = stack.pop();
            if (p.kind != open) {
                return new CT(ERROR, tok, "No match for '" + p + "' instead encountered '" + tok + "'");
            }
            return new CT(tk, tok);
        }

        private void db(String format, Object ... args) {
//            System.err.printf(format, args);
//            System.err.printf(" -- stack(");
//            if (stack.isEmpty()) {
//
//            } else {
//                for (Token tok : stack) {
//                    System.err.printf("%s ", tok.kind);
//                }
//            }
//            System.err.printf(") current=%s / currentCT=%s\n", current.kind, currentCT.kind);
        }

        /**
         * @return the next scanner token
         */
        private CT nextCT() {
            // TODO Annotations?
            TK prevTK = currentCT.kind;
            while (true) {
                db("nextCT");
                CT ct;
                switch (current.kind) {
                    case EOF:
                        db("eof");
                        if (stack.isEmpty()) {
                            ct = new CT(EOF, current);
                        } else {
                            TokenKind unmatched = stack.pop().kind;
                            stack.clear(); // So we will get EOF next time
                            ct = new CT(UNMATCHED, current, "Unmatched " + unmatched);
                        }
                        break;
                    case LPAREN:
                    case LBRACE:
                    case LBRACKET:
                        stack.push(advance());
                        prevTK = SEMI; // new start
                        continue;
                    case RPAREN:
                        ct = match(PARENS, TokenKind.LPAREN);
                        break;
                    case RBRACE:
                        ct = match(BRACES, TokenKind.LBRACE);
                        break;
                    case RBRACKET:
                        ct = match(BRACKETS, TokenKind.LBRACKET);
                        break;
                    default:
                        ct = new CT(TK.tokenKindToTK(current.kind), advance());
                        break;
                }
                if (ct.kind.isStart() && !prevTK.isOkToTerminate()) {
                    return new CT(ERROR, current, "No '" + prevTK + "' before '" + ct.kind + "'");
                }
                if (stack.isEmpty() || ct.kind.isError()) {
                    return ct;
                }
                prevTK = ct.kind;
            }
        }
    }

    /**
     * Fuzzy parser based on token kinds
     */
    private static class Parser {

        final Matched in;
        CT token;
        Completeness checkResult;

        final JShell proc;
        final String scannedInput;



        Parser(Matched in, JShell proc, String scannedInput) {
            this.in = in;
            nextToken();

            this.proc = proc;
            this.scannedInput = scannedInput;
        }

        final void nextToken() {
            in.next();
            token = in.currentCT;
        }

        boolean shouldAbort(TK tk) {
            if (token.kind == tk) {
                nextToken();
                return false;
            }
            switch (token.kind) {
                case EOF:
                    checkResult = ((tk == SEMI) && in.prevCT.kind.isOkToTerminate())
                            ? Completeness.COMPLETE_WITH_SEMI
                            : Completeness.DEFINITELY_INCOMPLETE;
                    return true;
                case UNMATCHED:
                    checkResult = Completeness.DEFINITELY_INCOMPLETE;
                    return true;
                default:
                    checkResult = error();
                    return true;

            }
        }

        Completeness lastly(TK tk) {
            if (shouldAbort(tk))  return checkResult;
            return Completeness.COMPLETE;
        }

        Completeness optionalFinalSemi() {
            if (!shouldAbort(SEMI)) return Completeness.COMPLETE;
            if (checkResult == Completeness.COMPLETE_WITH_SEMI) return Completeness.COMPLETE;
            return checkResult;
        }

        boolean shouldAbort(Completeness flags) {
            checkResult = flags;
            return flags != Completeness.COMPLETE;
        }

        public Completeness parseUnit() {
            //System.err.printf("%s:  belongs %o  XANY1 %o\n", token.kind, token.kind.belongs, token.kind.belongs & XANY1);
            switch (token.kind.belongs & XANY1) {
                case XEXPR1o:
                    return parseExpressionOptionalSemi();
                case XSTMT1o: {
                    Completeness stat = parseSimpleStatement();
                    return stat==null? error() : stat;
                }
                case XDECL1o:
                    return parseDeclaration();
                case XSTMT1o | XDECL1o:
                case XEXPR1o | XDECL1o:
                    return disambiguateDeclarationVsExpression();
                case 0:
                    if ((token.kind.belongs & XERRO) != 0) {
                        return parseExpressionStatement(); // Let this gen the status
                    }
                    return error();
                default:
                    throw new InternalError("Case not covered " + token.kind.belongs + " in " + token.kind);
            }
        }

        public Completeness parseDeclaration() {
            boolean isImport = token.kind == IMPORT;
            while (token.kind.isDeclaration()) {
                nextToken();
            }
            switch (token.kind) {
                case EQ:
                    // Check for array initializer
                    nextToken();
                    if (token.kind == BRACES) {
                        nextToken();
                        return lastly(SEMI);
                    }
                    return parseExpressionStatement();
                case BRACES:
                case SEMI:
                    nextToken();
                    return Completeness.COMPLETE;
                case UNMATCHED:
                    nextToken();
                    return Completeness.DEFINITELY_INCOMPLETE;
                case EOF:
                    switch (in.prevCT.kind) {
                        case BRACES:
                        case SEMI:
                            return Completeness.COMPLETE;
                        case IDENTIFIER:
                        case BRACKETS:
                            return Completeness.COMPLETE_WITH_SEMI;
                        case STAR:
                            if (isImport) {
                                return Completeness.COMPLETE_WITH_SEMI;
                            } else {
                                return Completeness.DEFINITELY_INCOMPLETE;
                            }
                        default:
                            return Completeness.DEFINITELY_INCOMPLETE;
                    }
                default:
                    return error();
            }
        }

        public Completeness disambiguateDeclarationVsExpression() {
            // String folding messes up position information.
            ParseTask pt = proc.taskFactory.new ParseTask(scannedInput);
            List<? extends Tree> units = pt.units();
            if (units.isEmpty()) {
                return error();
            }
            Tree unitTree = units.get(0);
            switch (unitTree.getKind()) {
                case EXPRESSION_STATEMENT:
                    return parseExpressionOptionalSemi();
                case LABELED_STATEMENT:
                    if (shouldAbort(IDENTIFIER))  return checkResult;
                    if (shouldAbort(COLON))  return checkResult;
                    return parseStatement();
                case VARIABLE:
                case IMPORT:
                case CLASS:
                case ENUM:
                case ANNOTATION_TYPE:
                case INTERFACE:
                case METHOD:
                    return parseDeclaration();
                default:
                    return error();
            }
        }

//        public Status parseExpressionOrDeclaration() {
//            if (token.kind == IDENTIFIER) {
//                nextToken();
//                switch (token.kind) {
//                    case IDENTIFIER:
//                        return parseDeclaration();
//                }
//            }
//            while (token.kind.isExpressionOrDeclaration()) {
//                if (!token.kind.isExpression()) {
//                    return parseDeclaration();
//                }
//                if (!token.kind.isDeclaration()) {
//                    // Expression not declaration
//                    if (token.kind == EQ) {
//                        // Check for array initializer
//                        nextToken();
//                        if (token.kind == BRACES) {
//                            nextToken();
//                            return lastly(SEMI);
//                        }
//                    }
//                    return parseExpressionStatement();
//                }
//                nextToken();
//            }
//            switch (token.kind) {
//                case BRACES:
//                case SEMI:
//                    nextToken();
//                    return Status.COMPLETE;
//                case UNMATCHED:
//                    nextToken();
//                    return Status.DEFINITELY_INCOMPLETE;
//                case EOF:
//                    if (in.prevCT.kind.isOkToTerminate()) {
//                        return Status.COMPLETE_WITH_SEMI;
//                    } else {
//                        return Status.DEFINITELY_INCOMPLETE;
//                    }
//                default:
//                    return error();
//            }
//        }

        public Completeness parseExpressionStatement() {
            if (shouldAbort(parseExpression()))  return checkResult;
            return lastly(SEMI);
        }

        public Completeness parseExpressionOptionalSemi() {
            if (shouldAbort(parseExpression())) return checkResult;
            return optionalFinalSemi();
        }

        public Completeness parseExpression() {
            while (token.kind.isExpression())
                nextToken();
            return Completeness.COMPLETE;
        }

        public Completeness parseStatement() {
            Completeness stat = parseSimpleStatement();
            if (stat == null) {
                return parseExpressionStatement();
            }
            return stat;
        }

        /**
         * Statement = Block | IF ParExpression Statement [ELSE Statement] | FOR
         * "(" ForInitOpt ";" [Expression] ";" ForUpdateOpt ")" Statement | FOR
         * "(" FormalParameter : Expression ")" Statement | WHILE ParExpression
         * Statement | DO Statement WHILE ParExpression ";" | TRY Block (
         * Catches | [Catches] FinallyPart ) | TRY "(" ResourceSpecification
         * ";"opt ")" Block [Catches] [FinallyPart] | SWITCH ParExpression "{"
         * SwitchBlockStatementGroups "}" | SYNCHRONIZED ParExpression Block |
         * RETURN [Expression] ";" | THROW Expression ";" | BREAK [Ident] ";" |
         * CONTINUE [Ident] ";" | ASSERT Expression [ ":" Expression ] ";" | ";"
         */
        public Completeness parseSimpleStatement() {
            switch (token.kind) {
                case BRACES:
                    return lastly(BRACES);
                case IF: {
                    nextToken();
                    if (shouldAbort(PARENS))  return checkResult;
                    Completeness thenpart = parseStatement();
                    if (shouldAbort(thenpart)) return thenpart;
                    if (token.kind == ELSE) {
                        nextToken();
                        return parseStatement();
                    }
                    return thenpart;

                }
                case FOR: {
                    nextToken();
                    if (shouldAbort(PARENS))  return checkResult;
                    if (shouldAbort(parseStatement()))  return checkResult;
                    return Completeness.COMPLETE;
                }
                case WHILE: {
                    nextToken();
                    if (shouldAbort(PARENS))  return error();
                    return parseStatement();
                }
                case DO: {
                    nextToken();
                    switch (parseStatement()) {
                        case DEFINITELY_INCOMPLETE:
                        case CONSIDERED_INCOMPLETE:
                        case COMPLETE_WITH_SEMI:
                            return Completeness.DEFINITELY_INCOMPLETE;
                        case UNKNOWN:
                            return error();
                        case COMPLETE:
                            break;
                    }
                    if (shouldAbort(WHILE))  return checkResult;
                    if (shouldAbort(PARENS)) return checkResult;
                    return lastly(SEMI);
                }
                case TRY: {
                    boolean hasResources = false;
                    nextToken();
                    if (token.kind == PARENS) {
                        nextToken();
                        hasResources = true;
                    }
                    if (shouldAbort(BRACES))  return checkResult;
                    if (token.kind == CATCH || token.kind == FINALLY) {
                        while (token.kind == CATCH) {
                            if (shouldAbort(CATCH))  return checkResult;
                            if (shouldAbort(PARENS)) return checkResult;
                            if (shouldAbort(BRACES)) return checkResult;
                        }
                        if (token.kind == FINALLY) {
                            if (shouldAbort(FINALLY))  return checkResult;
                            if (shouldAbort(BRACES)) return checkResult;
                        }
                    } else if (!hasResources) {
                        if (token.kind == EOF) {
                            return Completeness.DEFINITELY_INCOMPLETE;
                        } else {
                            return error();
                        }
                    }
                    return Completeness.COMPLETE;
                }
                case SWITCH: {
                    nextToken();
                    if (shouldAbort(PARENS))  return checkResult;
                    return lastly(BRACES);
                }
                case SYNCHRONIZED: {
                    nextToken();
                    if (shouldAbort(PARENS))  return checkResult;
                    return lastly(BRACES);
                }
                case THROW: {
                    nextToken();
                    if (shouldAbort(parseExpression()))  return checkResult;
                    return lastly(SEMI);
                }
                case SEMI:
                    return lastly(SEMI);
                case ASSERT:
                    nextToken();
                    // Crude expression parsing just happily eats the optional colon
                    return parseExpressionStatement();
                case RETURN:
                case BREAK:
                case CONTINUE:
                    nextToken();
                    return parseExpressionStatement();
                // What are these doing here?
                case ELSE:
                case FINALLY:
                case CATCH:
                    return error();
                case EOF:
                    return Completeness.CONSIDERED_INCOMPLETE;
                default:
                    return null;
            }
        }
    }
}
