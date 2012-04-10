/*
 * reserved comment block
 * DO NOT REMOVE OR ALTER!
 */
/*
 * Copyright 2001-2004 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
/*
 * $Id: NodeSortRecordFactory.java,v 1.2.4.1 2005/09/06 09:53:40 pvedula Exp $
 */

package com.sun.org.apache.xalan.internal.xsltc.dom;

import com.sun.org.apache.xalan.internal.xsltc.DOM;
import com.sun.org.apache.xalan.internal.xsltc.Translet;
import com.sun.org.apache.xalan.internal.xsltc.TransletException;
import com.sun.org.apache.xalan.internal.xsltc.runtime.AbstractTranslet;
import com.sun.org.apache.xml.internal.utils.LocaleUtility;
import java.util.Locale;
import java.text.Collator;

public class NodeSortRecordFactory {

    private static int DESCENDING = "descending".length();
    private static int NUMBER     = "number".length();

    private final DOM      _dom;
    private final String   _className;
    private Class _class;
    private SortSettings _sortSettings;

    /**
     *
     */
    protected Collator _collator;

    /**
     * Creates a NodeSortRecord producing object. The DOM specifies which tree
     * to get the nodes to sort from, the class name specifies what auxillary
     * class to use to sort the nodes (this class is generated by the Sort
     * class), and the translet parameter is needed for methods called by
     * this object.
     *
     * @deprecated This constructor is no longer used in generated code.  It
     *             exists only for backwards compatibility.
     */
     public NodeSortRecordFactory(DOM dom, String className, Translet translet,
                 String order[], String type[])
         throws TransletException
     {
         this(dom, className, translet, order, type, null, null);
     }

    /**
     * Creates a NodeSortRecord producing object. The DOM specifies which tree
     * to get the nodes to sort from, the class name specifies what auxillary
     * class to use to sort the nodes (this class is generated by the Sort
     * class), and the translet parameter is needed for methods called by
     * this object.
     */
     public NodeSortRecordFactory(DOM dom, String className, Translet translet,
                 String order[], String type[], String lang[],
                 String caseOrder[])
         throws TransletException
     {
         try {
             _dom = dom;
             _className = className;
             // This should return a Class definition if using TrAX
             _class = translet.getAuxiliaryClass(className);
             // This code is only run when the native API is used
             if (_class == null) {
                 _class = ObjectFactory.findProviderClass(
                      className, ObjectFactory.findClassLoader(), true);
             }

             int levels = order.length;
             int[] iOrder = new int[levels];
             int[] iType = new int[levels];
             for (int i = 0; i < levels; i++) {
                  if (order[i].length() == DESCENDING) {
                      iOrder[i] = NodeSortRecord.COMPARE_DESCENDING;
                  }
                  if (type[i].length() == NUMBER) {
                      iType[i] = NodeSortRecord.COMPARE_NUMERIC;
                  }
             }

             // Old NodeSortRecordFactory constructor had no lang or case_order
             // arguments.  Provide default values in that case for binary
             // compatibility.
             String[] emptyStringArray = null;
             if (lang == null || caseOrder == null) {
                 int numSortKeys = order.length;
                 emptyStringArray = new String[numSortKeys];

                 // Set up array of zero-length strings as default values
                 // of lang and case_order
                 for (int i = 0; i < numSortKeys; i++) {
                     emptyStringArray[i] = "";
                 }
             }

             if (lang == null) {
                 lang = emptyStringArray;
             }
             if (caseOrder == null) {
                 caseOrder = emptyStringArray;
             }

             final int length = lang.length;
             Locale[] locales = new Locale[length];
             Collator[] collators = new Collator[length];
             for (int i = 0; i< length; i++){
                 locales[i] = LocaleUtility.langToLocale(lang[i]);
                 collators[i] = Collator.getInstance(locales[i]);
             }

             _sortSettings = new SortSettings((AbstractTranslet) translet,
                                              iOrder, iType, locales, collators,
                                              caseOrder);
        } catch (ClassNotFoundException e) {
            throw new TransletException(e);
        }
    }



    /**
     * Create an instance of a sub-class of NodeSortRecord. The name of this
     * sub-class is passed to us in the constructor.
     */
    public NodeSortRecord makeNodeSortRecord(int node, int last)
        throws ExceptionInInitializerError,
               LinkageError,
               IllegalAccessException,
               InstantiationException,
               SecurityException,
               TransletException {

        final NodeSortRecord sortRecord =
            (NodeSortRecord)_class.newInstance();
        sortRecord.initialize(node, last, _dom, _sortSettings);
        return sortRecord;
    }

    public String getClassName() {
        return _className;
    }

   private final void setLang(final String lang[]){

    }
}
