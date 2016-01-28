/*
 * Copyright (c) 2009, 2015, Oracle and/or its affiliates. All rights reserved.
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

/*
 * This file is available under and governed by the GNU General Public
 * License version 2 only, as published by the Free Software Foundation.
 * However, the following notice accompanied the original version of this
 * file:
 *
 * The MIT License
 *
 * Copyright (c) 2004-2014 Paul R. Holser, Jr.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package jdk.internal.joptsimple;

import java.util.Collection;

import static java.lang.Character.*;

/**
 * Can tell whether or not options are well-formed.
 *
 * @author <a href="mailto:pholser@alumni.rice.edu">Paul Holser</a>
 */
final class ParserRules {
    static final char HYPHEN_CHAR = '-';
    static final String HYPHEN = String.valueOf( HYPHEN_CHAR );
    static final String DOUBLE_HYPHEN = "--";
    static final String OPTION_TERMINATOR = DOUBLE_HYPHEN;
    static final String RESERVED_FOR_EXTENSIONS = "W";

    private ParserRules() {
        throw new UnsupportedOperationException();
    }

    static boolean isShortOptionToken( String argument ) {
        return argument.startsWith( HYPHEN )
            && !HYPHEN.equals( argument )
            && !isLongOptionToken( argument );
    }

    static boolean isLongOptionToken( String argument ) {
        return argument.startsWith( DOUBLE_HYPHEN ) && !isOptionTerminator( argument );
    }

    static boolean isOptionTerminator( String argument ) {
        return OPTION_TERMINATOR.equals( argument );
    }

    static void ensureLegalOption( String option ) {
        if ( option.startsWith( HYPHEN ) )
            throw new IllegalOptionSpecificationException( String.valueOf( option ) );

        for ( int i = 0; i < option.length(); ++i )
            ensureLegalOptionCharacter( option.charAt( i ) );
    }

    static void ensureLegalOptions( Collection<String> options ) {
        for ( String each : options )
            ensureLegalOption( each );
    }

    private static void ensureLegalOptionCharacter( char option ) {
        if ( !( isLetterOrDigit( option ) || isAllowedPunctuation( option ) ) )
            throw new IllegalOptionSpecificationException( String.valueOf( option ) );
    }

    private static boolean isAllowedPunctuation( char option ) {
        String allowedPunctuation = "?." + HYPHEN_CHAR;
        return allowedPunctuation.indexOf( option ) != -1;
    }
}
