/*
 * Copyright (c) 2003, 2020, Oracle and/or its affiliates. All rights reserved.
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

package jdk.javadoc.internal.doclets.formats.html.markup;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.function.IntFunction;
import java.util.function.Predicate;

import javax.lang.model.element.Element;

import jdk.javadoc.internal.doclets.toolkit.Content;

/**
 * An HTML table, such as the summary tables for various kinds of element.
 *
 * <p>The table should be used in three phases:
 * <ol>
 * <li>Configuration: the overall characteristics of the table should be specified
 * <li>Population: the content for the cells in each row should be added
 * <li>Generation: the HTML content and any associated JavaScript can be accessed
 * </ol>
 *
 * Many methods return the current object, to facilitate fluent builder-style usage.
 *
 *  <p><b>This is NOT part of any supported API.
 *  If you write code that depends on this, you do so at your own risk.
 *  This code and its internal interfaces are subject to change or
 *  deletion without notice.</b>
 */
public class Table extends Content {
    private final HtmlStyle tableStyle;
    private Content caption;
    private Map<String, Predicate<Element>> tabMap;
    private String defaultTab;
    private Set<String> tabs;
    private HtmlStyle activeTabStyle = HtmlStyle.activeTableTab;
    private HtmlStyle tabStyle = HtmlStyle.tableTab;
    private HtmlStyle tabEnd = HtmlStyle.tabEnd;
    private IntFunction<String> tabScript;
    private Function<Integer, String> tabId = (i -> "t" + i);
    private TableHeader header;
    private List<HtmlStyle> columnStyles;
    private int rowScopeColumnIndex;
    private List<HtmlStyle> stripedStyles = Arrays.asList(HtmlStyle.altColor, HtmlStyle.rowColor);
    private final List<Content> bodyRows;
    private final List<Integer> bodyRowMasks;
    private String rowIdPrefix = "i";
    private String id;

    /**
     * Creates a builder for an HTML table.
     *
     * @param style     the style class for the {@code <table>} tag
     */
    public Table(HtmlStyle style) {
        this.tableStyle = style;
        bodyRows = new ArrayList<>();
        bodyRowMasks = new ArrayList<>();
    }

    /**
     * Sets the caption for the table.
     * This is ignored if the table is configured to provide tabs to select
     * different subsets of rows within the table.
     * The caption should be suitable for use as the content of a {@code <caption>}
     * element.
     *
     * <b>For compatibility, the code currently accepts a {@code <caption>} element
     * as well. This should be removed when all clients rely on using the {@code <caption>}
     * element being generated by this class.</b>
     *
     * @param captionContent the caption
     * @return this object
     */
    public Table setCaption(Content captionContent) {
        if (captionContent instanceof HtmlTree
                && ((HtmlTree) captionContent).htmlTag == HtmlTag.CAPTION) {
            caption = captionContent;
        } else {
            caption = getCaption(captionContent);
        }
        return this;
    }

    /**
     * Adds a tab to the table.
     * Tabs provide a way to display subsets of rows, as determined by a
     * predicate for the tab, and an element associated with each row.
     * Tabs will appear left-to-right in the order they are added.
     *
     * @param name      the name of the tab
     * @param predicate the predicate
     * @return this object
     */
    public Table addTab(String name, Predicate<Element> predicate) {
        if (tabMap == null) {
            tabMap = new LinkedHashMap<>();     // preserves order that tabs are added
            tabs = new HashSet<>();             // order not significant
        }
        tabMap.put(name, predicate);
        return this;
    }

    /**
     * Sets the name for the default tab, which displays all the rows in the table.
     * This tab will appear first in the left-to-right list of displayed tabs.
     *
     * @param name the name
     * @return this object
     */
    public Table setDefaultTab(String name) {
        defaultTab = name;
        return this;
    }

    /**
     * Sets the function used to generate the JavaScript to be used when a tab is selected.
     * When the function is invoked, the argument will be an integer value containing
     * the bit mask identifying the rows to be selected.
     *
     * @param f the function
     * @return this object
     */
    public Table setTabScript(IntFunction<String> f) {
        tabScript = f;
        return this;
    }

    /**
     * Sets the name of the styles used to display the tabs.
     *
     * @param activeTabStyle    the style for the active tab
     * @param tabStyle          the style for other tabs
     * @param tabEnd            the style for the padding that appears within each tab
     * @return  this object
     */
    public Table setTabStyles(HtmlStyle activeTabStyle, HtmlStyle tabStyle, HtmlStyle tabEnd) {
        this.activeTabStyle = activeTabStyle;
        this.tabStyle = tabStyle;
        this.tabEnd = tabEnd;
        return this;
    }

    /**
     * Sets the JavaScript function used to generate the {@code id} attribute for each tag.
     * The default is to use <code>t</code><i>N</i> where <i>N</i> is the index of the tab,
     * counting from 0 (for the default tab), and then from 1 upwards for additional tabs.
     *
     * @param f the function
     * @return this object
     */
    public Table setTabId(Function<Integer,String> f) {
        tabId = f;
        return this;
    }

    /**
     * Sets the header for the table.
     *
     * <p>Notes:
     * <ul>
     * <li>The column styles are not currently applied to the header, but probably should, eventually
     * </ul>
     *
     * @param header the header
     * @return this object
     */
    public Table setHeader(TableHeader header) {
        this.header = header;
        return this;
    }

    /**
     * Sets the styles used for {@code <tr>} tags, to give a "striped" appearance.
     * The defaults are currently {@code rowColor} and {@code altColor}.
     *
     * @param evenRowStyle  the style to use for even-numbered rows
     * @param oddRowStyle   the style to use for odd-numbered rows
     * @return
     */
    public Table setStripedStyles(HtmlStyle evenRowStyle, HtmlStyle oddRowStyle) {
        stripedStyles = Arrays.asList(evenRowStyle, oddRowStyle);
        return this;
    }

    /**
     * Sets the column used to indicate which cell in a row should be declared
     * as a header cell with the {@code scope} attribute set to {@code row}.
     *
     * @param columnIndex the column index
     * @return this object
     */
    public Table setRowScopeColumn(int columnIndex) {
        rowScopeColumnIndex = columnIndex;
        return this;
    }

    /**
     * Sets the styles for be used for the cells in each row.
     *
     * <p>Note:
     * <ul>
     * <li>The column styles are not currently applied to the header, but probably should, eventually
     * </ul>
     *
     * @param styles the styles
     * @return this object
     */
    public Table setColumnStyles(HtmlStyle... styles) {
        return setColumnStyles(Arrays.asList(styles));
    }

    /**
     * Sets the styles for be used for the cells in each row.
     *
     * <p>Note:
     * <ul>
     * <li>The column styles are not currently applied to the header, but probably should, eventually
     * </ul>
     *
     * @param styles the styles
     * @return this object
     */
    public Table setColumnStyles(List<HtmlStyle> styles) {
        columnStyles = styles;
        return this;
    }

    /**
     * Sets the prefix used for the {@code id} attribute for each row in the table.
     * The default is "i".
     *
     * <p>Note:
     * <ul>
     * <li>The prefix should probably be a value such that the generated ids cannot
     *      clash with any other id, such as those that might be created for fields within
     *      a class.
     * </ul>
     *
     * @param prefix the prefix
     * @return  this object
     */
    public Table setRowIdPrefix(String prefix) {
        rowIdPrefix = prefix;
        return this;
    }

    /**
     * Sets the id attribute of the table.
     *
     * @param id the id
     * @return this object
     */
    public Table setId(String id) {
        this.id = id;
        return this;
    }

    /**
     * Adds a row of data to the table.
     * Each item of content should be suitable for use as the content of a
     * {@code <th>} or {@code <td>} cell.
     * This method should not be used when the table has tabs: use a method
     * that takes an {@code Element} parameter instead.
     *
     * @param contents the contents for the row
     */
    public void addRow(Content... contents) {
        addRow(null, Arrays.asList(contents));
    }

    /**
     * Adds a row of data to the table.
     * Each item of content should be suitable for use as the content of a
     * {@code <th>} or {@code <td> cell}.
     * This method should not be used when the table has tabs: use a method
     * that takes an {@code element} parameter instead.
     *
     * @param contents the contents for the row
     */
    public void addRow(List<Content> contents) {
        addRow(null, contents);
    }

    /**
     * Adds a row of data to the table.
     * Each item of content should be suitable for use as the content of a
     * {@code <th>} or {@code <td>} cell.
     *
     * If tabs have been added to the table, the specified element will be used
     * to determine whether the row should be displayed when any particular tab
     * is selected, using the predicate specified when the tab was
     * {@link #addTab(String,Predicate) added}.
     *
     * @param element the element
     * @param contents the contents for the row
     * @throws NullPointerException if tabs have previously been added to the table
     *      and {@code element} is null
     */
    public void addRow(Element element, Content... contents) {
        addRow(element, Arrays.asList(contents));
    }

    /**
     * Adds a row of data to the table.
     * Each item of content should be suitable for use as the content of a
     * {@code <th>} or {@code <td>} cell.
     *
     * If tabs have been added to the table, the specified element will be used
     * to determine whether the row should be displayed when any particular tab
     * is selected, using the predicate specified when the tab was
     * {@link #addTab(String,Predicate) added}.
     *
     * @param element the element
     * @param contents the contents for the row
     * @throws NullPointerException if tabs have previously been added to the table
     *      and {@code element} is null
     */
    public void addRow(Element element, List<Content> contents) {
        if (tabMap != null && element == null) {
            throw new NullPointerException();
        }

        HtmlTree row = new HtmlTree(HtmlTag.TR);

        if (stripedStyles != null) {
            int rowIndex = bodyRows.size();
            row.put(HtmlAttr.CLASS, stripedStyles.get(rowIndex % 2).name());
        }
        int colIndex = 0;
        for (Content c : contents) {
            HtmlStyle cellStyle = (columnStyles == null || colIndex > columnStyles.size())
                    ? null
                    : columnStyles.get(colIndex);
            HtmlTree cell = (colIndex == rowScopeColumnIndex)
                    ? HtmlTree.TH(cellStyle, "row", c)
                    : HtmlTree.TD(cellStyle, c);
            row.add(cell);
            colIndex++;
        }
        bodyRows.add(row);

        if (tabMap != null) {
            int index = bodyRows.size() - 1;
            row.put(HtmlAttr.ID, (rowIdPrefix + index));
            int mask = 0;
            int maskBit = 1;
            for (Map.Entry<String, Predicate<Element>> e : tabMap.entrySet()) {
                String name = e.getKey();
                Predicate<Element> predicate = e.getValue();
                if (predicate.test(element)) {
                    tabs.add(name);
                    mask |= maskBit;
                }
                maskBit = (maskBit << 1);
            }
            bodyRowMasks.add(mask);
        }
    }

    /**
     * Returns whether or not the table is empty.
     * The table is empty if it has no (body) rows.
     *
     * @return true if the table has no rows
     */
    public boolean isEmpty() {
        return bodyRows.isEmpty();
    }

    @Override
    public boolean write(Writer out, boolean atNewline) throws IOException {
        return toContent().write(out, atNewline);
    }

    /**
     * Returns the HTML for the table.
     *
     * @return the HTML
     */
    private Content toContent() {
        HtmlTree mainDiv = new HtmlTree(HtmlTag.DIV);
        mainDiv.setStyle(tableStyle);
        if (id != null) {
            mainDiv.setId(id);
        }
        HtmlTree table = new HtmlTree(HtmlTag.TABLE);
        if (tabMap == null || tabs.size() == 1) {
            if (tabMap == null) {
                table.add(caption);
            } else if (tabs.size() == 1) {
                String tabName = tabs.iterator().next();
                table.add(getCaption(new StringContent(tabName)));
            }
            table.add(getTableBody());
            mainDiv.add(table);
        } else {
            HtmlTree tablist = new HtmlTree(HtmlTag.DIV)
                    .put(HtmlAttr.ROLE, "tablist")
                    .put(HtmlAttr.ARIA_ORIENTATION, "horizontal");

            int tabIndex = 0;
            tablist.add(createTab(tabId.apply(tabIndex), activeTabStyle, true, defaultTab));
            table.put(HtmlAttr.ARIA_LABELLEDBY, tabId.apply(tabIndex));
            for (String tabName : tabMap.keySet()) {
                tabIndex++;
                if (tabs.contains(tabName)) {
                    String script = tabScript.apply(1 << (tabIndex - 1));
                    HtmlTree tab = createTab(tabId.apply(tabIndex), tabStyle, false, tabName);
                    tab.put(HtmlAttr.ONCLICK, script);
                    tablist.add(tab);
                }
            }
            HtmlTree tabpanel = new HtmlTree(HtmlTag.DIV)
                    .put(HtmlAttr.ID, tableStyle + "_tabpanel")
                    .put(HtmlAttr.ROLE, "tabpanel");
            table.add(getTableBody());
            tabpanel.add(table);
            mainDiv.add(tablist);
            mainDiv.add(tabpanel);
        }
        return mainDiv;
    }

    private HtmlTree createTab(String tabId, HtmlStyle style, boolean defaultTab, String tabName) {
        HtmlTree tab = new HtmlTree(HtmlTag.BUTTON)
                .put(HtmlAttr.ROLE, "tab")
                .put(HtmlAttr.ARIA_SELECTED, defaultTab ? "true" : "false")
                .put(HtmlAttr.ARIA_CONTROLS, tableStyle + "_tabpanel")
                .put(HtmlAttr.TABINDEX, defaultTab ? "0" : "-1")
                .put(HtmlAttr.ONKEYDOWN, "switchTab(event)")
                .put(HtmlAttr.ID, tabId)
                .setStyle(style);
        tab.add(tabName);
        return tab;
    }

    private Content getTableBody() {
        ContentBuilder tableContent = new ContentBuilder();
        Content thead = new HtmlTree(HtmlTag.THEAD);
        thead.add(header);
        tableContent.add(thead);
        Content tbody = new HtmlTree(HtmlTag.TBODY);
        bodyRows.forEach(tbody::add);
        tableContent.add(tbody);
        return tableContent;
    }

    /**
     * Returns whether or not the table needs JavaScript support.
     * It requires such support if tabs have been added.
     *
     * @return true if JavaScript is required
     */
    public boolean needsScript() {
        return (tabs != null) && (tabs.size() > 1);
    }

    /**
     * Returns the script to be used in conjunction with the table.
     *
     * @return the script
     */
    public String getScript() {
        if (tabMap == null)
            throw new IllegalStateException();

        StringBuilder sb = new StringBuilder();

        // Add the variable defining the bitmask for each row
        sb.append("var data").append(" = {");
        int rowIndex = 0;
        for (int mask : bodyRowMasks) {
            if (rowIndex > 0) {
                sb.append(",");
            }
            sb.append("\"").append(rowIdPrefix).append(rowIndex).append("\":").append(mask);
            rowIndex++;
        }
        sb.append("};\n");

        // Add the variable defining the tabs
        sb.append("var tabs = {");
        appendTabInfo(sb, 65535, tabId.apply(0), defaultTab);
        int tabIndex = 1;
        int maskBit = 1;
        for (String tabName: tabMap.keySet()) {
            if (tabs.contains(tabName)) {
                sb.append(",");
                appendTabInfo(sb, maskBit, tabId.apply(tabIndex), tabName);
            }
            tabIndex++;
            maskBit = (maskBit << 1);
        }
        sb.append("};\n");

        // Add the variables defining the stylenames
        appendStyleInfo(sb,
                stripedStyles.get(0), stripedStyles.get(1), tabStyle, activeTabStyle);
        return sb.toString();
    }

    private void appendTabInfo(StringBuilder sb, int value, String id, String name) {
        sb.append(value)
                .append(":[")
                .append(Script.stringLiteral(id))
                .append(",")
                .append(Script.stringLiteral(name))
                .append("]");
    }

    private void appendStyleInfo(StringBuilder sb, HtmlStyle... styles) {
        for (HtmlStyle style : styles) {
            sb.append("var ").append(style).append(" = \"").append(style).append("\";\n");
        }

    }

    private HtmlTree getCaption(Content title) {
        return new HtmlTree(HtmlTag.CAPTION,
                HtmlTree.SPAN(title),
                HtmlTree.SPAN(tabEnd, Entity.NO_BREAK_SPACE));
    }
}
