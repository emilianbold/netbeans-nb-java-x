/*
 * Copyright (c) 2003, 2019, Oracle and/or its affiliates. All rights reserved.
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

package jdk.javadoc.internal.doclets.toolkit.taglets;

import java.util.Set;
import javax.lang.model.element.Element;

import com.sun.source.doctree.DocTree;
import jdk.javadoc.doclet.Taglet.Location;
import jdk.javadoc.internal.doclets.toolkit.Content;

/**
 * This is the Taglet interface used internally within the doclet.
 */

public interface Taglet {
    /**
     * Returns the set of allowed locations for block tags
     * handled by this taglet.
     *
     * @return the set of allowable locations
     */
    Set<Location> getAllowedLocations();

    /**
     * Return true if this <code>Taglet</code>
     * is used in field documentation.
     * @return true if this <code>Taglet</code>
     * is used in field documentation and false
     * otherwise.
     */
    boolean inField();

    /**
     * Return true if this <code>Taglet</code>
     * is used in constructor documentation.
     * @return true if this <code>Taglet</code>
     * is used in constructor documentation and false
     * otherwise.
     */
    boolean inConstructor();

    /**
     * Return true if this <code>Taglet</code>
     * is used in method documentation.
     * @return true if this <code>Taglet</code>
     * is used in method documentation and false
     * otherwise.
     */
    boolean inMethod();

    /**
     * Return true if this <code>Taglet</code>
     * is used in overview documentation.
     * @return true if this <code>Taglet</code>
     * is used in method documentation and false
     * otherwise.
     */
    boolean inOverview();

    /**
     * Return true if this <code>Taglet</code>
     * is used in module documentation.
     * @return true if this <code>Taglet</code>
     * is used in module documentation and false
     * otherwise.
     */
    boolean inModule();

    /**
     * Return true if this <code>Taglet</code>
     * is used in package documentation.
     * @return true if this <code>Taglet</code>
     * is used in package documentation and false
     * otherwise.
     */
    boolean inPackage();

    /**
     * Return true if this <code>Taglet</code>
     * is used in type documentation (classes or
     * interfaces).
     * @return true if this <code>Taglet</code>
     * is used in type documentation and false
     * otherwise.
     */
    boolean inType();

    /**
     * Return true if this <code>Taglet</code>
     * is an inline tag. Return false otherwise.
     * @return true if this <code>Taglet</code>
     * is an inline tag and false otherwise.
     */
    boolean isInlineTag();

    /**
     * Indicates whether this taglet supports block tags.
     *
     * @return true if this taglet supports block tags
     * @implSpec This implementation returns the inverse
     * result to {@code isInlineTag}.
     */
    default boolean isBlockTag() {
        return !isInlineTag();
    }

    /**
     * Return the name of this custom tag.
     * @return the name of this custom tag.
     */
    String getName();

    /**
     * Given the <code>Tag</code> representation of this custom
     * tag, return its Content representation, which is output
     * to the generated page.
     * @param holder the element holding the tag
     * @param tag the <code>Tag</code> representation of this custom tag.
     * @param writer a {@link TagletWriter} Taglet writer.
     * @throws UnsupportedOperationException thrown when the method is not supported by the taglet.
     * @return the Content representation of this <code>Tag</code>.
     */
    Content getTagletOutput(Element holder, DocTree tag, TagletWriter writer) throws
            UnsupportedOperationException;

    /**
     * Given an element object, check if it holds any tags of
     * this type.  If it does, return the content representing the output.
     * If it does not, return null.
     * @param holder an element holding the custom tag.
     * @param writer a {@link TagletWriter} Taglet writer.
     * @throws UnsupportedTagletOperationException thrown when the method is not
     *         supported by the taglet.
     * @return the content representation of this <code>Tag</code>.
     */
    Content getTagletOutput(Element holder, TagletWriter writer) throws
            UnsupportedTagletOperationException;

    class UnsupportedTagletOperationException extends UnsupportedOperationException {
        private static final long serialVersionUID = -3530273193380250271L;
        public UnsupportedTagletOperationException(String message) {
            super(message);
        }
    };
}
