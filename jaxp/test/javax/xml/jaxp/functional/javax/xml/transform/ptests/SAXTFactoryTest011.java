/*
 * Copyright (c) 2014, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
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
package javax.xml.transform.ptests;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import static javax.xml.transform.ptests.TransformerTestConst.CLASS_DIR;
import static javax.xml.transform.ptests.TransformerTestConst.GOLDEN_DIR;
import static javax.xml.transform.ptests.TransformerTestConst.XML_DIR;
import javax.xml.transform.sax.SAXTransformerFactory;
import static jaxp.library.JAXPTestUtilities.compareWithGold;
import static jaxp.library.JAXPTestUtilities.failCleanup;
import static jaxp.library.JAXPTestUtilities.failUnexpected;
import static org.testng.Assert.assertTrue;
import org.testng.annotations.Test;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLFilter;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * Test XMLFilter parse InputSource along with customized ContentHandler by
 * using SAX parser as it's reader.
 */
public class SAXTFactoryTest011 {
    /**
     * Unit test for contentHandler setter/getter with parent.
     */
    @Test
    public void testcase01() {
        String outputFile = CLASS_DIR + "saxtf011.out";
        String goldFile = GOLDEN_DIR + "saxtf011GF.out";
        String xsltFile = XML_DIR + "cities.xsl";
        String xmlFile = XML_DIR + "cities.xml";

        try {
            // The transformer will use a SAX parser as it's reader.
            XMLReader reader = XMLReaderFactory.createXMLReader();
            DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
            dbf.setNamespaceAware(true);
            DocumentBuilder docBuilder = dbf.newDocumentBuilder();
            Document document = docBuilder.parse(new File(xsltFile));
            Node node = (Node)document;
            DOMSource domSource= new DOMSource(node);

            SAXTransformerFactory saxTFactory
                    = (SAXTransformerFactory)TransformerFactory.newInstance();
            XMLFilter filter = saxTFactory.newXMLFilter(domSource);

            filter.setParent(reader);
            filter.setContentHandler(new MyContentHandler(outputFile));

            // Now, when you call transformer.parse, it will set itself as
            // the content handler for the parser object (it's "parent"), and
            // will then call the parse method on the parser.
            filter.parse(new InputSource(xmlFile));
            assertTrue(compareWithGold(goldFile, outputFile));
        } catch (SAXException | IOException | TransformerConfigurationException
                | ParserConfigurationException ex) {
            failUnexpected(ex);
        } finally {
            try {
                Path outputPath = Paths.get(outputFile);
                if(Files.exists(outputPath))
                    Files.delete(outputPath);
            } catch (IOException ex) {
                failCleanup(ex, outputFile);
            }
        }
    }
}
