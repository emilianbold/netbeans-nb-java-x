/*
 * Copyright (c) 2017, 2019, Oracle and/or its affiliates. All rights reserved.
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

import com.sun.source.doctree.DocTree;
import com.sun.source.doctree.EndElementTree;
import com.sun.source.doctree.StartElementTree;
import com.sun.source.doctree.TextTree;
import com.sun.source.util.DocTreeFactory;
import com.sun.tools.doclint.HtmlTag;
import jdk.javadoc.internal.doclets.formats.html.markup.HtmlTree;
import jdk.javadoc.internal.doclets.formats.html.markup.Navigation;
import jdk.javadoc.internal.doclets.toolkit.Content;
import jdk.javadoc.internal.doclets.toolkit.DocFileElement;
import jdk.javadoc.internal.doclets.toolkit.DocFilesHandler;
import jdk.javadoc.internal.doclets.toolkit.util.DocFile;
import jdk.javadoc.internal.doclets.toolkit.util.DocFileIOException;
import jdk.javadoc.internal.doclets.toolkit.util.DocPath;
import jdk.javadoc.internal.doclets.toolkit.util.DocPaths;
import jdk.javadoc.internal.doclets.toolkit.util.DocletConstants;
import jdk.javadoc.internal.doclets.toolkit.util.Utils;

import javax.lang.model.element.Element;
import javax.lang.model.element.ModuleElement;
import javax.lang.model.element.PackageElement;
import javax.tools.FileObject;
import javax.tools.JavaFileManager.Location;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import jdk.javadoc.internal.doclets.formats.html.markup.Navigation.PageMode;

public class DocFilesHandlerImpl implements DocFilesHandler {

    public final Element element;
    public final Location location;
    public final DocPath  source;
    public final HtmlConfiguration configuration;
    private Navigation navBar;

    /**
     * Constructor to construct the DocFilesWriter object.
     *
     * @param configuration the configuration of this doclet.
     * @param element the containing element of the doc-files.
     *
     */
    public DocFilesHandlerImpl(HtmlConfiguration configuration, Element element) {
        this.configuration = configuration;
        this.element = element;

        switch (element.getKind()) {
            case MODULE:
                ModuleElement mdle = (ModuleElement)element;
                location = configuration.utils.getLocationForModule(mdle);
                source = DocPaths.DOC_FILES;
                break;
            case PACKAGE:
                PackageElement pkg = (PackageElement)element;
                location = configuration.utils.getLocationForPackage(pkg);
                // Note, given that we have a module-specific location,
                // we want a module-relative path for the source, and not the
                // standard path that may include the module directory
                source = DocPath.create(pkg.getQualifiedName().toString().replace('.', '/'))
                        .resolve(DocPaths.DOC_FILES);
                break;
            default:
                throw new AssertionError("unsupported element " + element);
        }
    }

    /**
     * Copy doc-files directory and its contents from the source
     * elements directory to the generated documentation directory.
     *
     * @throws DocFileIOException if there is a problem while copying
     *         the documentation files
     */

    public void copyDocFiles()  throws DocFileIOException {
        boolean first = true;
        for (DocFile srcdir : DocFile.list(configuration, location, source)) {
            if (!srcdir.isDirectory()) {
                continue;
            }
            DocPath path = null;
            switch (this.element.getKind()) {
                case MODULE:
                    path = DocPaths.forModule((ModuleElement)this.element);
                    break;
                case PACKAGE:
                    path = configuration.docPaths.forPackage((PackageElement)this.element);
                    break;
                default:
                    throw new AssertionError("unknown kind:" + this.element.getKind());
            }
            copyDirectory(srcdir, path.resolve(DocPaths.DOC_FILES), first);
            first = false;
        }
    }

    public List<DocPath> getStylesheets() throws DocFileIOException {
        List<DocPath> stylesheets = new ArrayList<DocPath>();
        for (DocFile srcdir : DocFile.list(configuration, location, source)) {
            for (DocFile srcFile : srcdir.list()) {
                if (srcFile.getName().endsWith(".css"))
                    stylesheets.add(DocPaths.DOC_FILES.resolve(srcFile.getName()));
            }
        }
        return stylesheets;
    }

    private void copyDirectory(DocFile srcdir, final DocPath dstDocPath,
                               boolean first) throws DocFileIOException {
        DocFile dstdir = DocFile.createFileForOutput(configuration, dstDocPath);
        if (srcdir.isSameFile(dstdir)) {
            return;
        }
        for (DocFile srcfile: srcdir.list()) {
            DocFile destfile = dstdir.resolve(srcfile.getName());
            if (srcfile.isFile()) {
                if (destfile.exists() && !first) {
                    configuration.messages.warning("doclet.Copy_Overwrite_warning",
                            srcfile.getPath(), dstdir.getPath());
                } else {
                    if (Utils.toLowerCase(srcfile.getPath()).endsWith(".html")) {
                        handleHtmlFile(srcfile, dstDocPath);
                    } else {
                        configuration.messages.notice("doclet.Copying_File_0_To_Dir_1",
                                srcfile.getPath(), dstdir.getPath());
                        destfile.copyFile(srcfile);
                    }
                }
            } else if (srcfile.isDirectory()) {
                if (configuration.copydocfilesubdirs
                        && !configuration.shouldExcludeDocFileDir(srcfile.getName())) {
                    DocPath dirDocPath = dstDocPath.resolve(srcfile.getName());
                    copyDirectory(srcfile, dirDocPath, first);
                }
            }
        }
    }

    private void handleHtmlFile(DocFile srcfile, DocPath dstPath) throws DocFileIOException {
        Utils utils = configuration.utils;
        FileObject fileObject = srcfile.getFileObject();
        DocFileElement dfElement = new DocFileElement(element, fileObject);

        DocPath dfilePath = dstPath.resolve(srcfile.getName());
        HtmlDocletWriter docletWriter = new DocFileWriter(configuration, dfilePath, element);
        configuration.messages.notice("doclet.Generating_0", docletWriter.filename.getPath());

        List<? extends DocTree> localTags = getLocalHeaderTags(utils.getPreamble(dfElement));
        Content localTagsContent = docletWriter.commentTagsToContent(null, dfElement, localTags, false);

        String title = getWindowTitle(docletWriter, dfElement).trim();
        HtmlTree htmlContent = docletWriter.getBody(true, title);
        docletWriter.addTop(htmlContent);
        PackageElement pkg = (PackageElement) element;
        this.navBar = new Navigation(pkg, configuration, docletWriter.fixedNavDiv,
                PageMode.DOCFILE, docletWriter.path);
        Content mdleLinkContent = docletWriter.getModuleLink(utils.elementUtils.getModuleOf(pkg),
                docletWriter.contents.moduleLabel);
        navBar.setNavLinkModule(mdleLinkContent);
        Content pkgLinkContent = docletWriter.getPackageLink(pkg, docletWriter.contents.packageLabel);
        navBar.setNavLinkPackage(pkgLinkContent);
        navBar.setUserHeader(docletWriter.getUserHeaderFooter(true));
        Content header = HtmlTree.HEADER();
        header.addContent(navBar.getContent(true));
        htmlContent.addContent(header);

        List<? extends DocTree> fullBody = utils.getFullBody(dfElement);
        Content bodyContent = docletWriter.commentTagsToContent(null, dfElement, fullBody, false);
        docletWriter.addTagsInfo(dfElement, bodyContent);
        Content main = HtmlTree.MAIN();
        main.addContent(bodyContent);
        htmlContent.addContent(main);

        navBar.setUserFooter(docletWriter.getUserHeaderFooter(false));
        Content footer = HtmlTree.FOOTER();
        footer.addContent(navBar.getContent(false));
        docletWriter.addBottom(footer);
        htmlContent.addContent(footer);
        docletWriter.printHtmlDocument(Collections.emptyList(), null, false, localTagsContent, htmlContent);
    }


    private List<? extends DocTree> getLocalHeaderTags(List<? extends DocTree> dtrees) {
        List<DocTree> localTags = new ArrayList<>();
        DocTreeFactory docTreeFactory = configuration.docEnv.getDocTrees().getDocTreeFactory();
        boolean inHead = false;
        boolean inTitle = false;
        loop:
        for (DocTree dt : dtrees) {
            switch (dt.getKind()) {
                case START_ELEMENT:
                    StartElementTree startElem = (StartElementTree)dt;
                    switch (HtmlTag.get(startElem.getName())) {
                        case HEAD:
                            inHead = true;
                            break;
                        case META:
                            break;
                        case TITLE:
                            inTitle = true;
                            break;
                        default:
                            if (inHead) {
                                localTags.add(startElem);
                                localTags.add(docTreeFactory.newTextTree(DocletConstants.NL));
                            }
                    }
                    break;
                case END_ELEMENT:
                    EndElementTree endElem = (EndElementTree)dt;
                    switch (HtmlTag.get(endElem.getName())) {
                        case HEAD:
                            inHead = false;
                            break loop;
                        case TITLE:
                            inTitle = false;
                            break;
                        default:
                            if (inHead) {
                                localTags.add(endElem);
                                localTags.add(docTreeFactory.newTextTree(DocletConstants.NL));
                            }
                    }
                    break;
                case ENTITY:
                case TEXT:
                    if (inHead && !inTitle) {
                        localTags.add(dt);
                    }
                    break;
            }
        }
        return localTags;
    }

    private String getWindowTitle(HtmlDocletWriter docletWriter, Element element) {
        List<? extends DocTree> preamble = configuration.utils.getPreamble(element);
        StringBuilder sb = new StringBuilder();
        boolean titleFound = false;
        loop:
        for (DocTree dt : preamble) {
            switch (dt.getKind()) {
                case START_ELEMENT:
                    StartElementTree nodeStart = (StartElementTree)dt;
                    if (Utils.toLowerCase(nodeStart.getName().toString()).equals("title")) {
                        titleFound = true;
                    }
                    break;

                case END_ELEMENT:
                    EndElementTree nodeEnd = (EndElementTree)dt;
                    if (Utils.toLowerCase(nodeEnd.getName().toString()).equals("title")) {
                        break loop;
                    }
                    break;

                case TEXT:
                    TextTree nodeText = (TextTree)dt;
                    if (titleFound)
                        sb.append(nodeText.getBody());
                    break;

                default:
                    // do nothing
            }
        }
        return docletWriter.getWindowTitle(sb.toString().trim());
    }

    private static class DocFileWriter extends HtmlDocletWriter {

        final PackageElement pkg;

        /**
         * Constructor to construct the HtmlDocletWriter object.
         *
         * @param configuration the configuruation of this doclet.
         * @param path          the file to be generated.
         * @param e             the anchoring element.
         */
        public DocFileWriter(HtmlConfiguration configuration, DocPath path, Element e) {
            super(configuration, path);
            switch (e.getKind()) {
                case PACKAGE:
                    pkg = (PackageElement)e;
                    break;
                default:
                    throw new AssertionError("unsupported element: " + e.getKind());
            }
        }
    }
}
