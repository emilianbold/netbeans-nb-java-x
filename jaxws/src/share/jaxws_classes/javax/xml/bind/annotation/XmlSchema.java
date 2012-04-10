/*
 * Copyright (c) 2004, 2010, Oracle and/or its affiliates. All rights reserved.
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

package javax.xml.bind.annotation;

import java.lang.annotation.Retention;
import java.lang.annotation.Target;

import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

/**
 * <p> Maps a package name to a XML namespace. </p>
 *
 * <h3>Usage</h3>
 * <p>
 * The XmlSchema annotation can be used with the following program
 * elements:
 * <ul>
 *   <li>package</li>
 * </ul>
 *
 * <p>
 * This is a package level annotation and follows the recommendations
 * and restrictions contained in JSR 175, section III, "Annotations".
 * Thus the usage is subject to the following constraints and
 * recommendations.
 * <ul>
 *   <li> There can only be one package declaration as noted in JSR
 *        175, section III, "Annotations". </li>
 *   <li> JSR 175 recommends package-info.java for package level
 *        annotations. JAXB Providers that follow this recommendation
 *        will allow the package level annotations to be defined in
 *        package-info.java.
 * </ul>
 * <p>
 *
 * <p><b>Example 1:</b> Customize name of XML namespace to which
 * package is mapped.</p>
 *
 * <pre>
 *    &#64;javax.xml.bind.annotation.XmlSchema (
 *      namespace = "http://www.example.com/MYPO1"
 *    )
 *
 *    &lt;!-- XML Schema fragment -->
 *    &lt;schema
 *      xmlns=...
 *      xmlns:po=....
 *      targetNamespace="http://www.example.com/MYPO1"
 *    >
 *    &lt;!-- prefixes generated by default are implementation
 *            depedenent -->
 * </pre>
 *
 * <p><b>Example 2:</b> Customize namespace prefix, namespace URI
 * mapping</p>
 *
 * <pre>
 *    // Package level annotation
 *    &#64;javax.xml.bind.annotation.XmlSchema (
 *      xmlns = {
 *        &#64;javax.xml.bind.annotation.XmlNs(prefix = "po",
 *                   namespaceURI="http://www.example.com/myPO1"),
 *
 *        &#64;javax.xml.bind.annotation.XmlNs(prefix="xs",
 *                   namespaceURI="http://www.w3.org/2001/XMLSchema")
 *      )
 *    )
 *
 *    &lt;!-- XML Schema fragment -->
 *    &lt;schema
 *        xmlns:xs="http://www.w3.org/2001/XMLSchema"
 *        xmlns:po="http://www.example.com/PO1"
 *        targetNamespace="http://www.example.com/PO1">
 *
 * </pre>
 *
 * <p><b>Example 3:</b> Customize elementFormDefault</p>
 * <pre>
 *    &#64;javax.xml.bind.annotation.XmlSchema (
 *      elementFormDefault=XmlNsForm.UNQUALIFIED
 *      ...
 *    )
 *
 *    &lt;!-- XML Schema fragment -->
 *    &lt;schema
 *        xmlns="http://www.w3.org/2001/XMLSchema"
 *        xmlns:po="http://www.example.com/PO1"
 *        elementFormDefault="unqualified">
 *
 * </pre>

 * @author Sekhar Vajjhala, Sun Microsystems, Inc.
 * @since JAXB2.0
 */

@Retention(RUNTIME) @Target(PACKAGE)
public @interface XmlSchema {

    /**
     * Customize the namespace URI, prefix associations. By default,
     * the namespace prefixes for a XML namespace are generated by a
     * JAXB Provider in an implementation dependent way.
     */
    XmlNs[]  xmlns() default {};

    /**
     * Name of the XML namespace.
     */
    String namespace() default "";

    /**
     * Namespace qualification for elements. By default, element
     * default attribute will be absent from the XML Schema fragment.
     */
    XmlNsForm elementFormDefault() default XmlNsForm.UNSET;

    /**
     * Namespace qualification for attributes. By default,
     * attributesFormDefault will be absent from the XML Schema fragment.
     */
    XmlNsForm attributeFormDefault() default XmlNsForm.UNSET;

    /**
     * Indicates that this namespace (specified by {@link #namespace()})
     * has a schema already available exeternally, available at this location.
     *
     * <p>
     * This instructs the JAXB schema generators to simply refer to
     * the pointed schema, as opposed to generating components into the schema.
     * This schema is assumed to match what would be otherwise produced
     * by the schema generator (same element names, same type names...)
     *
     * <p>
     * This feature is intended to be used when a set of the Java classes
     * is originally generated from an existing schema, hand-written to
     * match externally defined schema, or the generated schema is modified
     * manually.
     *
     * <p>
     * Value could be any absolute URI, like <tt>http://example.org/some.xsd</tt>.
     * It is also possible to specify the empty string, to indicate
     * that the schema is externally available but the location is
     * unspecified (and thus it's the responsibility of the reader of the generate
     * schema to locate it.) Finally, the default value of this property
     * <tt>"##generate"</tt> indicates that the schema generator is going
     * to generate components for this namespace (as it did in JAXB 2.0.)
     *
     * <p>
     * Multiple {@link XmlSchema} annotations on multiple packages are allowed
     * to govern the same {@link #namespace()}. In such case, all of them
     * must have the same {@link #location()} values.
     *
     *
     * <h3>Note to implementor</h3>
     * <p>
     * More precisely, the value must be either <tt>""</tt>, <tt>"##generate"</tt>, or
     * <a href="http://www.w3.org/TR/xmlschema-2/#anyURI">
     * a valid lexical representation of <tt>xs:anyURI</tt></a> that begins
     * with <tt>&lt;scheme>:</tt>.
     *
     * <p>
     * A schema generator is expected to generate a corresponding
     * <tt>&lt;xs:import namespace="..." schemaLocation="..."/></tt> (or
     * no <tt>schemaLocation</tt> attribute at all if the empty string is specified.)
     * However, the schema generator is allowed to use a different value in
     * the <tt>schemaLocation</tt> attribute (including not generating
     * such attribute), for example so that the user can specify a local
     * copy of the resource through the command line interface.
     *
     * @since JAXB2.1
     */
    String location() default NO_LOCATION;

    /**
     * The default value of the {@link #location()} attribute,
     * which indicates that the schema generator will generate
     * components in this namespace.
     */
    // the actual value is chosen because ## is not a valid
    // sequence in xs:anyURI.
    static final String NO_LOCATION = "##generate";
}
