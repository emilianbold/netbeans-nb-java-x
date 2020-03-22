/*
 * Copyright (c) 1997, 2020, Oracle and/or its affiliates. All rights reserved.
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

package jdk.javadoc.internal.doclets.formats.html;

import java.util.*;

import javax.lang.model.element.Element;
import javax.lang.model.element.TypeElement;

import com.sun.source.doctree.DocTree;
import jdk.javadoc.internal.doclets.formats.html.markup.BodyContents;
import jdk.javadoc.internal.doclets.formats.html.markup.ContentBuilder;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlStyle;
import jdk.javadoc.internal.doclets.formats.html.markup.TagName;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree;
import jdk.javadoc.internal.doclets.toolkit.Content;
import jdk.javadoc.internal.doclets.toolkit.util.DocPath;

/**
 * This abstract class exists to provide functionality needed in the
 * the formatting of member information.  Since AbstractSubWriter and its
 * subclasses control this, they would be the logical place to put this.
 * However, because each member type has its own subclass, subclassing
 * can not be used effectively to change formatting.  The concrete
 * class subclass of this class can be subclassed to change formatting.
 *
 *  <p><b>This is NOT part of any supported API.
 *  If you write code that depends on this, you do so at your own risk.
 *  This code and its internal interfaces are subject to change or
 *  deletion without notice.</b>
 *
 * @see AbstractMemberWriter
 * @see ClassWriterImpl
 */
public abstract class SubWriterHolderWriter extends HtmlDocletWriter {

    /**
     * The HTML builder for the body contents.
     */
    protected BodyContents bodyContents = new BodyContents();

    public SubWriterHolderWriter(HtmlConfiguration configuration, DocPath filename) {
        super(configuration, filename);
    }

    /**
     * Add the summary header.
     *
     * @param mw the writer for the member being documented
     * @param memberTree the content tree to which the summary header will be added
     */
    public void addSummaryHeader(AbstractMemberWriter mw, Content memberTree) {
        mw.addSummaryLabel(memberTree);
    }

    /**
     * Add the inherited summary header.
     *
     * @param mw the writer for the member being documented
     * @param typeElement the te to be documented
     * @param inheritedTree the content tree to which the inherited summary header will be added
     */
    public void addInheritedSummaryHeader(AbstractMemberWriter mw, TypeElement typeElement,
            Content inheritedTree) {
        mw.addInheritedSummaryLabel(typeElement, inheritedTree);
    }

    /**
     * Add the index comment.
     *
     * @param member the member being documented
     * @param contentTree the content tree to which the comment will be added
     */
    protected void addIndexComment(Element member, Content contentTree) {
        List<? extends DocTree> tags = utils.getFirstSentenceTrees(member);
        addIndexComment(member, tags, contentTree);
    }

    /**
     * Add the index comment.
     *
     * @param member the member being documented
     * @param firstSentenceTags the first sentence tags for the member to be documented
     * @param tdSummary the content tree to which the comment will be added
     */
    protected void addIndexComment(Element member, List<? extends DocTree> firstSentenceTags,
            Content tdSummary) {
        List<? extends DocTree> deprs = utils.getBlockTags(member, DocTree.Kind.DEPRECATED);
        Content div;
        if (utils.isDeprecated(member)) {
            Content deprLabel = HtmlTree.SPAN(HtmlStyle.deprecatedLabel, getDeprecatedPhrase(member));
            div = HtmlTree.DIV(HtmlStyle.block, deprLabel);
            if (!deprs.isEmpty()) {
                addSummaryDeprecatedComment(member, deprs.get(0), div);
            }
            tdSummary.add(div);
            return;
        } else {
            Element te = member.getEnclosingElement();
            if (te != null &&  utils.isTypeElement(te) && utils.isDeprecated(te)) {
                Content deprLabel = HtmlTree.SPAN(HtmlStyle.deprecatedLabel, getDeprecatedPhrase(te));
                div = HtmlTree.DIV(HtmlStyle.block, deprLabel);
                tdSummary.add(div);
            }
        }
        addSummaryComment(member, firstSentenceTags, tdSummary);
    }

    /**
     * Add the summary link for the member.
     *
     * @param mw the writer for the member being documented
     * @param member the member to be documented
     * @param contentTree the content tree to which the link will be added
     */
    public void addSummaryLinkComment(AbstractMemberWriter mw, Element member, Content contentTree) {
        List<? extends DocTree> tags = utils.getFirstSentenceTrees(member);
        addSummaryLinkComment(mw, member, tags, contentTree);
    }

    /**
     * Add the summary link comment.
     *
     * @param mw the writer for the member being documented
     * @param member the member being documented
     * @param firstSentenceTags the first sentence tags for the member to be documented
     * @param tdSummary the content tree to which the comment will be added
     */
    public void addSummaryLinkComment(AbstractMemberWriter mw,
            Element member, List<? extends DocTree> firstSentenceTags, Content tdSummary) {
        addIndexComment(member, firstSentenceTags, tdSummary);
    }

    /**
     * Add the inherited member summary.
     *
     * @param mw the writer for the member being documented
     * @param typeElement the class being documented
     * @param member the member being documented
     * @param isFirst true if its the first link being documented
     * @param linksTree the content tree to which the summary will be added
     */
    public void addInheritedMemberSummary(AbstractMemberWriter mw,
                                          TypeElement typeElement,
                                          Element member,
                                          boolean isFirst,
                                          Content linksTree) {
        if (!isFirst) {
            linksTree.add(", ");
        }
        mw.addInheritedSummaryLink(typeElement, member, linksTree);
    }

    /**
     * Get the document content header tree
     *
     * @return a content tree the document content header
     */
    public Content getContentHeader() {
        return new ContentBuilder();
    }

    /**
     * Add the class content tree.
     *
     * @param classContentTree class content tree which will be added to the content tree
     */
    public void addClassContentTree(Content classContentTree) {
        bodyContents.addMainContent(classContentTree);
    }

    /**
     * Add the annotation content tree.
     *
     * @param annotationContentTree annotation content tree which will be added to the content tree
     */
    public void addAnnotationContentTree(Content annotationContentTree) {
        addClassContentTree(annotationContentTree);
    }

    /**
     * Get the member header tree
     *
     * @return a content tree for the member header
     */
    public Content getMemberTreeHeader() {
        HtmlTree ul = new HtmlTree(TagName.UL);
        ul.setStyle(HtmlStyle.blockList);
        return ul;
    }

    public Content getMemberInheritedTree() {
        HtmlTree div = new HtmlTree(TagName.DIV);
        div.setStyle(HtmlStyle.inheritedList);
        return div;
    }

    /**
     * Adds the member tree with css style and id attribute.
     * @param style the css style to be applied to member tree
     * @param sectionName the section name to use for the section id attribute
     * @param memberSummaryTree the content tree representing the member summary
     * @param memberTree the content tree representing the member
     */
    public void addMemberTree(HtmlStyle style, SectionName sectionName, Content memberSummaryTree, Content memberTree) {
        HtmlTree htmlTree = HtmlTree.SECTION(style, memberTree)
                .setId(sectionName.getName());
        memberSummaryTree.add(getMemberTree(htmlTree));
    }

    /**
     * Get the member tree
     *
     * @param contentTree the tree used to generate the complete member tree
     * @return a content tree for the member
     */
    public Content getMemberTree(Content contentTree) {
        return HtmlTree.LI(HtmlStyle.blockList, contentTree);
    }

    /**
     * Get the member summary tree
     *
     * @param contentTree the tree used to generate the member summary tree
     * @return a content tree for the member summary
     */
    public Content getMemberSummaryTree(Content contentTree) {
        return HtmlTree.SECTION(HtmlStyle.summary, contentTree);
    }

    /**
     * Get the member details tree
     *
     * @param contentTree the tree used to generate the member details tree
     * @return a content tree for the member details
     */
    public Content getMemberDetailsTree(Content contentTree) {
        return HtmlTree.SECTION(HtmlStyle.details, contentTree);
    }

    /**
     * Get the member tree
     *
     * @param style the style class to be added to the content tree
     * @param contentTree the tree used to generate the complete member tree
     * @return the member tree
     */
    public Content getMemberTree(HtmlStyle style, Content contentTree) {
        return HtmlTree.SECTION(style, contentTree);
    }
}
