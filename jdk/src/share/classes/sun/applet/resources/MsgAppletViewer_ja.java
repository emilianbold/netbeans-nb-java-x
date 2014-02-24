/*
 * Copyright (c) 1996, 2013, Oracle and/or its affiliates. All rights reserved.
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
package sun.applet.resources;

import java.util.ListResourceBundle;

public class MsgAppletViewer_ja extends ListResourceBundle {

    public Object[][] getContents() {
        Object[][] temp = new Object[][] {
            {"textframe.button.dismiss", "\u53D6\u6D88"},
            {"appletviewer.tool.title", "\u30A2\u30D7\u30EC\u30C3\u30C8\u30FB\u30D3\u30E5\u30FC\u30A2: {0}"},
            {"appletviewer.menu.applet", "\u30A2\u30D7\u30EC\u30C3\u30C8"},
            {"appletviewer.menuitem.restart", "\u518D\u8D77\u52D5"},
            {"appletviewer.menuitem.reload", "\u518D\u30ED\u30FC\u30C9"},
            {"appletviewer.menuitem.stop", "\u505C\u6B62"},
            {"appletviewer.menuitem.save", "\u4FDD\u5B58..."},
            {"appletviewer.menuitem.start", "\u958B\u59CB"},
            {"appletviewer.menuitem.clone", "\u30AF\u30ED\u30FC\u30F3..."},
            {"appletviewer.menuitem.tag", "\u30BF\u30B0..."},
            {"appletviewer.menuitem.info", "\u60C5\u5831..."},
            {"appletviewer.menuitem.edit", "\u7DE8\u96C6"},
            {"appletviewer.menuitem.encoding", "\u6587\u5B57\u30A8\u30F3\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0"},
            {"appletviewer.menuitem.print", "\u5370\u5237..."},
            {"appletviewer.menuitem.props", "\u30D7\u30ED\u30D1\u30C6\u30A3..."},
            {"appletviewer.menuitem.close", "\u9589\u3058\u308B"},
            {"appletviewer.menuitem.quit", "\u7D42\u4E86"},
            {"appletviewer.label.hello", "Hello..."},
            {"appletviewer.status.start", "\u30A2\u30D7\u30EC\u30C3\u30C8\u3092\u958B\u59CB\u3057\u3066\u3044\u307E\u3059..."},
            {"appletviewer.appletsave.filedialogtitle","\u30A2\u30D7\u30EC\u30C3\u30C8\u3092\u30D5\u30A1\u30A4\u30EB\u306B\u30B7\u30EA\u30A2\u30E9\u30A4\u30BA"},
            {"appletviewer.appletsave.err1", "{0}\u3092{1}\u306B\u30B7\u30EA\u30A2\u30E9\u30A4\u30BA"},
            {"appletviewer.appletsave.err2", "appletSave\u5185: {0}"},
            {"appletviewer.applettag", "\u30BF\u30B0\u306E\u8868\u793A"},
            {"appletviewer.applettag.textframe", "\u30A2\u30D7\u30EC\u30C3\u30C8HTML\u30BF\u30B0"},
            {"appletviewer.appletinfo.applet", "-- \u30A2\u30D7\u30EC\u30C3\u30C8\u60C5\u5831\u306A\u3057 --"},
            {"appletviewer.appletinfo.param", "-- \u30D1\u30E9\u30E1\u30FC\u30BF\u60C5\u5831\u306A\u3057 --"},
            {"appletviewer.appletinfo.textframe", "\u30A2\u30D7\u30EC\u30C3\u30C8\u60C5\u5831"},
            {"appletviewer.appletprint.fail", "\u5370\u5237\u306B\u5931\u6557\u3057\u307E\u3057\u305F\u3002"},
            {"appletviewer.appletprint.finish", "\u5370\u5237\u304C\u5B8C\u4E86\u3057\u307E\u3057\u305F\u3002"},
            {"appletviewer.appletprint.cancel", "\u5370\u5237\u304C\u4E2D\u6B62\u3055\u308C\u307E\u3057\u305F\u3002"},
            {"appletviewer.appletencoding", "\u6587\u5B57\u30A8\u30F3\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0: {0}"},
            {"appletviewer.parse.warning.requiresname", "\u8B66\u544A: <param name=... value=...>\u30BF\u30B0\u306Bname\u5C5E\u6027\u304C\u5FC5\u8981\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.paramoutside", "\u8B66\u544A: <param>\u30BF\u30B0\u304C<applet> ... </applet>\u306E\u5916\u5074\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.applet.requirescode", "\u8B66\u544A: <applet>\u30BF\u30B0\u306Bcode\u5C5E\u6027\u304C\u5FC5\u8981\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.applet.requiresheight", "\u8B66\u544A: <applet>\u30BF\u30B0\u306Bheight\u5C5E\u6027\u304C\u5FC5\u8981\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.applet.requireswidth", "\u8B66\u544A: <applet>\u30BF\u30B0\u306Bwidth\u5C5E\u6027\u304C\u5FC5\u8981\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.object.requirescode", "\u8B66\u544A: <object>\u30BF\u30B0\u306Bcode\u5C5E\u6027\u304C\u5FC5\u8981\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.object.requiresheight", "\u8B66\u544A: <object>\u30BF\u30B0\u306Bheight\u5C5E\u6027\u304C\u5FC5\u8981\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.object.requireswidth", "\u8B66\u544A: <object>\u30BF\u30B0\u306Bwidth\u5C5E\u6027\u304C\u5FC5\u8981\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.embed.requirescode", "\u8B66\u544A: <embed>\u30BF\u30B0\u306Bcode\u5C5E\u6027\u304C\u5FC5\u8981\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.embed.requiresheight", "\u8B66\u544A: <embed>\u30BF\u30B0\u306Bheight\u5C5E\u6027\u304C\u5FC5\u8981\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.embed.requireswidth", "\u8B66\u544A: <embed>\u30BF\u30B0\u306Bwidth\u5C5E\u6027\u304C\u5FC5\u8981\u3067\u3059\u3002"},
            {"appletviewer.parse.warning.appnotLongersupported", "\u8B66\u544A: <app>\u30BF\u30B0\u306F\u73FE\u5728\u306F\u30B5\u30DD\u30FC\u30C8\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3002\u304B\u308F\u308A\u306B<applet>\u3092\u4F7F\u7528\u3057\u3066\u304F\u3060\u3055\u3044\u3002"},
            {"appletviewer.usage", "\u4F7F\u7528\u65B9\u6CD5: appletviewer <options> url(s)\n\n<options>\u306B\u306F\u6B21\u306E\u3082\u306E\u304C\u3042\u308A\u307E\u3059:\n  -debug                  Java\u30C7\u30D0\u30C3\u30AC\u3067\u30A2\u30D7\u30EC\u30C3\u30C8\u30FB\u30D3\u30E5\u30FC\u30A2\u3092\u958B\u59CB\u3059\u308B\n  -encoding <encoding>    HTML\u30D5\u30A1\u30A4\u30EB\u306B\u3088\u3063\u3066\u4F7F\u7528\u3055\u308C\u308B\u6587\u5B57\u30A8\u30F3\u30B3\u30FC\u30C7\u30A3\u30F3\u30B0\u3092\u6307\u5B9A\u3059\u308B\n  -J<runtime flag>        \u5F15\u6570\u3092Java\u30A4\u30F3\u30BF\u30D7\u30EA\u30BF\u306B\u6E21\u3059\n\n-J\u306F\u975E\u6A19\u6E96\u30AA\u30D7\u30B7\u30E7\u30F3\u3067\u3042\u308A\u3001\u4E88\u544A\u306A\u3057\u306B\u5909\u66F4\u3055\u308C\u308B\u53EF\u80FD\u6027\u304C\u3042\u308A\u307E\u3059\u3002"},
            {"appletviewer.main.err.unsupportedopt", "\u30B5\u30DD\u30FC\u30C8\u3055\u308C\u3066\u3044\u306A\u3044\u30AA\u30D7\u30B7\u30E7\u30F3: {0}"},
            {"appletviewer.main.err.unrecognizedarg", "\u8A8D\u8B58\u3055\u308C\u306A\u3044\u5F15\u6570: {0}"},
            {"appletviewer.main.err.dupoption", "\u30AA\u30D7\u30B7\u30E7\u30F3\u306E\u4F7F\u7528\u304C\u91CD\u8907\u3057\u3066\u3044\u307E\u3059: {0}"},
            {"appletviewer.main.err.inputfile", "\u5165\u529B\u30D5\u30A1\u30A4\u30EB\u304C\u6307\u5B9A\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3002"},
            {"appletviewer.main.err.badurl", "\u4E0D\u6B63\u306AURL: {0} ( {1} )"},
            {"appletviewer.main.err.io", "\u8AAD\u8FBC\u307F\u4E2D\u306E\u5165\u51FA\u529B\u4F8B\u5916\u3067\u3059: {0}"},
            {"appletviewer.main.err.readablefile", "{0}\u304C\u30D5\u30A1\u30A4\u30EB\u3067\u3042\u308A\u3001\u8AAD\u8FBC\u307F\u53EF\u80FD\u3067\u3042\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\u3044\u3002"},
            {"appletviewer.main.err.correcturl", "{0}\u306F\u6B63\u3057\u3044URL\u3067\u3059\u304B\u3002"},
            {"appletviewer.main.prop.store", "AppletViewer\u7528\u306E\u30E6\u30FC\u30B6\u30FC\u304C\u6307\u5B9A\u3057\u305F\u30D7\u30ED\u30D1\u30C6\u30A3"},
            {"appletviewer.main.err.prop.cantread", "\u30E6\u30FC\u30B6\u30FC\u30FB\u30D7\u30ED\u30D1\u30C6\u30A3\u30FB\u30D5\u30A1\u30A4\u30EB\u3092\u8AAD\u307F\u8FBC\u3081\u307E\u305B\u3093: {0}"},
            {"appletviewer.main.err.prop.cantsave", "\u30E6\u30FC\u30B6\u30FC\u30FB\u30D7\u30ED\u30D1\u30C6\u30A3\u30FB\u30D5\u30A1\u30A4\u30EB\u3092\u4FDD\u5B58\u3067\u304D\u307E\u305B\u3093: {0}"},
            {"appletviewer.main.warn.nosecmgr", "\u8B66\u544A: \u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u3092\u7121\u52B9\u5316\u3057\u307E\u3059\u3002"},
            {"appletviewer.main.debug.cantfinddebug", "\u30C7\u30D0\u30C3\u30AC\u304C\u898B\u3064\u304B\u308A\u307E\u305B\u3093\u3002"},
            {"appletviewer.main.debug.cantfindmain", "\u30C7\u30D0\u30C3\u30AC\u306E\u30E1\u30A4\u30F3\u30FB\u30E1\u30BD\u30C3\u30C9\u304C\u898B\u3064\u304B\u308A\u307E\u305B\u3093\u3002"},
            {"appletviewer.main.debug.exceptionindebug", "\u30C7\u30D0\u30C3\u30AC\u306B\u4F8B\u5916\u304C\u767A\u751F\u3057\u307E\u3057\u305F\u3002"},
            {"appletviewer.main.debug.cantaccess", "\u30C7\u30D0\u30C3\u30AC\u306B\u30A2\u30AF\u30BB\u30B9\u3067\u304D\u307E\u305B\u3093\u3002"},
            {"appletviewer.main.nosecmgr", "\u8B66\u544A: SecurityManager\u304C\u30A4\u30F3\u30B9\u30C8\u30FC\u30EB\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3002"},
            {"appletviewer.main.warning", "\u8B66\u544A: \u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u958B\u59CB\u3055\u308C\u307E\u305B\u3093\u3067\u3057\u305F\u3002\u5165\u529B\u306B<applet>\u30BF\u30B0\u304C\u3042\u308B\u3053\u3068\u3092\u78BA\u8A8D\u3057\u3066\u304F\u3060\u3055\u3044\u3002"},
            {"appletviewer.main.warn.prop.overwrite", "\u8B66\u544A: \u30E6\u30FC\u30B6\u30FC\u306E\u30EA\u30AF\u30A8\u30B9\u30C8\u3067\u30B7\u30B9\u30C6\u30E0\u30FB\u30D7\u30ED\u30D1\u30C6\u30A3\u3092\u4E00\u6642\u7684\u306B\u4E0A\u66F8\u304D\u3057\u307E\u3059: \u30AD\u30FC: {0} \u53E4\u3044\u5024: {1} \u65B0\u3057\u3044\u5024: {2}"},
            {"appletviewer.main.warn.cantreadprops", "\u8B66\u544A: AppletViewer\u30D7\u30ED\u30D1\u30C6\u30A3\u30FB\u30D5\u30A1\u30A4\u30EB{0}\u3092\u8AAD\u307F\u8FBC\u3081\u307E\u305B\u3093\u3002\u30C7\u30D5\u30A9\u30EB\u30C8\u3092\u4F7F\u7528\u3057\u307E\u3059\u3002"},
            {"appletioexception.loadclass.throw.interrupted", "\u30AF\u30E9\u30B9\u306E\u30ED\u30FC\u30C9\u304C\u4E2D\u65AD\u3057\u307E\u3057\u305F: {0}"},
            {"appletioexception.loadclass.throw.notloaded", "\u30AF\u30E9\u30B9\u304C\u30ED\u30FC\u30C9\u3055\u308C\u307E\u305B\u3093: {0}"},
            {"appletclassloader.loadcode.verbose", "{1}\u3092\u53D6\u5F97\u3059\u308B\u305F\u3081\u306E{0}\u3078\u306E\u30B9\u30C8\u30EA\u30FC\u30E0\u3092\u958B\u304D\u307E\u3059"},
            {"appletclassloader.filenotfound", "{0}\u306E\u691C\u7D22\u4E2D\u306B\u30D5\u30A1\u30A4\u30EB\u304C\u898B\u3064\u304B\u308A\u307E\u305B\u3093"},
            {"appletclassloader.fileformat", "{0}\u306E\u30ED\u30FC\u30C9\u4E2D\u306B\u30D5\u30A1\u30A4\u30EB\u30FB\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u4F8B\u5916\u304C\u767A\u751F\u3057\u307E\u3057\u305F"},
            {"appletclassloader.fileioexception", "{0}\u306E\u30ED\u30FC\u30C9\u4E2D\u306B\u5165\u51FA\u529B\u4F8B\u5916\u304C\u767A\u751F\u3057\u307E\u3057\u305F"},
            {"appletclassloader.fileexception", "{1}\u306E\u30ED\u30FC\u30C9\u4E2D\u306B{0}\u4F8B\u5916\u304C\u767A\u751F\u3057\u307E\u3057\u305F"},
            {"appletclassloader.filedeath", "{1}\u306E\u30ED\u30FC\u30C9\u4E2D\u306B{0}\u304C\u5F37\u5236\u7D42\u4E86\u3057\u307E\u3057\u305F"},
            {"appletclassloader.fileerror", "{1}\u306E\u30ED\u30FC\u30C9\u4E2D\u306B{0}\u30A8\u30E9\u30FC\u304C\u767A\u751F\u3057\u307E\u3057\u305F"},
            {"appletclassloader.findclass.verbose.openstream", "{1}\u3092\u53D6\u5F97\u3059\u308B\u305F\u3081\u306E{0}\u3078\u306E\u30B9\u30C8\u30EA\u30FC\u30E0\u3092\u958B\u304D\u307E\u3059"},
            {"appletclassloader.getresource.verbose.forname", "\u540D\u524D{0}\u306EAppletClassLoader.getResource\u3067\u3059"},
            {"appletclassloader.getresource.verbose.found", "\u30EA\u30BD\u30FC\u30B9{0}\u304C\u30B7\u30B9\u30C6\u30E0\u30FB\u30EA\u30BD\u30FC\u30B9\u3068\u3057\u3066\u691C\u51FA\u3055\u308C\u307E\u3057\u305F"},
            {"appletclassloader.getresourceasstream.verbose", "\u30EA\u30BD\u30FC\u30B9{0}\u304C\u30B7\u30B9\u30C6\u30E0\u30FB\u30EA\u30BD\u30FC\u30B9\u3068\u3057\u3066\u691C\u51FA\u3055\u308C\u307E\u3057\u305F"},
            {"appletpanel.runloader.err", "\u30AA\u30D6\u30B8\u30A7\u30AF\u30C8\u307E\u305F\u306F\u30B3\u30FC\u30C9\u30FB\u30D1\u30E9\u30E1\u30FC\u30BF\u306E\u3044\u305A\u308C\u304B\u3067\u3059\u3002"},
            {"appletpanel.runloader.exception", "{0}\u306E\u30C7\u30B7\u30EA\u30A2\u30E9\u30A4\u30BA\u4E2D\u306B\u4F8B\u5916\u304C\u767A\u751F\u3057\u307E\u3057\u305F"},
            {"appletpanel.destroyed", "\u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u7834\u68C4\u3055\u308C\u307E\u3057\u305F\u3002"},
            {"appletpanel.loaded", "\u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u30ED\u30FC\u30C9\u3055\u308C\u307E\u3057\u305F\u3002"},
            {"appletpanel.started", "\u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u958B\u59CB\u3055\u308C\u307E\u3057\u305F\u3002"},
            {"appletpanel.inited", "\u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u521D\u671F\u5316\u3055\u308C\u307E\u3057\u305F\u3002"},
            {"appletpanel.stopped", "\u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u505C\u6B62\u3055\u308C\u307E\u3057\u305F\u3002"},
            {"appletpanel.disposed", "\u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u7834\u68C4\u3055\u308C\u307E\u3057\u305F\u3002"},
            {"appletpanel.nocode", "APPLET\u30BF\u30B0\u306BCODE\u30D1\u30E9\u30E1\u30FC\u30BF\u304C\u3042\u308A\u307E\u305B\u3093\u3002"},
            {"appletpanel.notfound", "\u30ED\u30FC\u30C9: \u30AF\u30E9\u30B9{0}\u304C\u898B\u3064\u304B\u308A\u307E\u305B\u3093\u3002"},
            {"appletpanel.nocreate", "\u30ED\u30FC\u30C9: {0}\u3092\u30A4\u30F3\u30B9\u30BF\u30F3\u30B9\u5316\u3067\u304D\u307E\u305B\u3093\u3002"},
            {"appletpanel.noconstruct", "\u30ED\u30FC\u30C9: {0}\u306Fpublic\u3067\u306A\u3044\u304B\u3001public\u30B3\u30F3\u30B9\u30C8\u30E9\u30AF\u30BF\u3092\u6301\u3063\u3066\u3044\u307E\u305B\u3093\u3002"},
            {"appletpanel.death", "\u5F37\u5236\u7D42\u4E86\u3055\u308C\u307E\u3057\u305F"},
            {"appletpanel.exception", "\u4F8B\u5916: {0}\u3002"},
            {"appletpanel.exception2", "\u4F8B\u5916: {0}: {1}\u3002"},
            {"appletpanel.error", "\u30A8\u30E9\u30FC: {0}\u3002"},
            {"appletpanel.error2", "\u30A8\u30E9\u30FC: {0}: {1}\u3002"},
            {"appletpanel.notloaded", "\u521D\u671F\u5316: \u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u30ED\u30FC\u30C9\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3002"},
            {"appletpanel.notinited", "\u958B\u59CB: \u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u521D\u671F\u5316\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3002"},
            {"appletpanel.notstarted", "\u505C\u6B62: \u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u958B\u59CB\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3002"},
            {"appletpanel.notstopped", "\u7834\u68C4: \u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u505C\u6B62\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3002"},
            {"appletpanel.notdestroyed", "\u7834\u68C4: \u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u7834\u68C4\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3002"},
            {"appletpanel.notdisposed", "\u30ED\u30FC\u30C9: \u30A2\u30D7\u30EC\u30C3\u30C8\u304C\u7834\u68C4\u3055\u308C\u3066\u3044\u307E\u305B\u3093\u3002"},
            {"appletpanel.bail", "\u4E2D\u65AD\u6E08: \u7D42\u4E86\u3057\u3066\u3044\u307E\u3059\u3002"},
            {"appletpanel.filenotfound", "{0}\u306E\u691C\u7D22\u4E2D\u306B\u30D5\u30A1\u30A4\u30EB\u304C\u898B\u3064\u304B\u308A\u307E\u305B\u3093"},
            {"appletpanel.fileformat", "{0}\u306E\u30ED\u30FC\u30C9\u4E2D\u306B\u30D5\u30A1\u30A4\u30EB\u30FB\u30D5\u30A9\u30FC\u30DE\u30C3\u30C8\u4F8B\u5916\u304C\u767A\u751F\u3057\u307E\u3057\u305F"},
            {"appletpanel.fileioexception", "{0}\u306E\u30ED\u30FC\u30C9\u4E2D\u306B\u5165\u51FA\u529B\u4F8B\u5916\u304C\u767A\u751F\u3057\u307E\u3057\u305F"},
            {"appletpanel.fileexception", "{1}\u306E\u30ED\u30FC\u30C9\u4E2D\u306B{0}\u4F8B\u5916\u304C\u767A\u751F\u3057\u307E\u3057\u305F"},
            {"appletpanel.filedeath", "{1}\u306E\u30ED\u30FC\u30C9\u4E2D\u306B{0}\u304C\u5F37\u5236\u7D42\u4E86\u3057\u307E\u3057\u305F"},
            {"appletpanel.fileerror", "{1}\u306E\u30ED\u30FC\u30C9\u4E2D\u306B{0}\u30A8\u30E9\u30FC\u304C\u767A\u751F\u3057\u307E\u3057\u305F"},
            {"appletpanel.badattribute.exception", "HTML\u89E3\u6790: width\u307E\u305F\u306Fheight\u5C5E\u6027\u306E\u5024\u304C\u4E0D\u6B63\u3067\u3059"},
            {"appletillegalargumentexception.objectinputstream", "AppletObjectInputStream\u306F\u975Enull\u306E\u30ED\u30FC\u30C0\u30FC\u304C\u5FC5\u8981\u3067\u3059"},
            {"appletprops.title", "AppletViewer\u30D7\u30ED\u30D1\u30C6\u30A3"},
            {"appletprops.label.http.server", "Http\u30D7\u30ED\u30AD\u30B7\u30FB\u30B5\u30FC\u30D0\u30FC:"},
            {"appletprops.label.http.proxy", "Http\u30D7\u30ED\u30AD\u30B7\u30FB\u30DD\u30FC\u30C8:"},
            {"appletprops.label.network", "\u30CD\u30C3\u30C8\u30EF\u30FC\u30AF\u30FB\u30A2\u30AF\u30BB\u30B9:"},
            {"appletprops.choice.network.item.none", "\u306A\u3057"},
            {"appletprops.choice.network.item.applethost", "\u30A2\u30D7\u30EC\u30C3\u30C8\u30FB\u30DB\u30B9\u30C8"},
            {"appletprops.choice.network.item.unrestricted", "\u5236\u9650\u306A\u3057"},
            {"appletprops.label.class", "\u30AF\u30E9\u30B9\u30FB\u30A2\u30AF\u30BB\u30B9:"},
            {"appletprops.choice.class.item.restricted", "\u5236\u9650\u4ED8\u304D"},
            {"appletprops.choice.class.item.unrestricted", "\u5236\u9650\u306A\u3057"},
            {"appletprops.label.unsignedapplet", "\u7F72\u540D\u3055\u308C\u3066\u3044\u306A\u3044\u30A2\u30D7\u30EC\u30C3\u30C8\u3092\u8A31\u53EF:"},
            {"appletprops.choice.unsignedapplet.no", "\u3044\u3044\u3048"},
            {"appletprops.choice.unsignedapplet.yes", "\u306F\u3044"},
            {"appletprops.button.apply", "\u9069\u7528"},
            {"appletprops.button.cancel", "\u53D6\u6D88"},
            {"appletprops.button.reset", "\u30EA\u30BB\u30C3\u30C8"},
            {"appletprops.apply.exception", "\u30D7\u30ED\u30D1\u30C6\u30A3{0}\u306E\u4FDD\u5B58\u306B\u5931\u6557\u3057\u307E\u3057\u305F"},
            /* 4066432 */
            {"appletprops.title.invalidproxy", "\u30A8\u30F3\u30C8\u30EA\u304C\u7121\u52B9\u3067\u3059"},
            {"appletprops.label.invalidproxy", "\u30D7\u30ED\u30AD\u30B7\u30FB\u30DD\u30FC\u30C8\u306F\u6B63\u306E\u6574\u6570\u5024\u3067\u3042\u308B\u5FC5\u8981\u304C\u3042\u308A\u307E\u3059\u3002"},
            {"appletprops.button.ok", "OK"},
            /* end 4066432 */
            {"appletprops.prop.store", "AppletViewer\u7528\u306E\u30E6\u30FC\u30B6\u30FC\u304C\u6307\u5B9A\u3057\u305F\u30D7\u30ED\u30D1\u30C6\u30A3"},
            {"appletsecurityexception.checkcreateclassloader", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30AF\u30E9\u30B9\u30ED\u30FC\u30C0\u30FC"},
            {"appletsecurityexception.checkaccess.thread", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30B9\u30EC\u30C3\u30C9"},
            {"appletsecurityexception.checkaccess.threadgroup", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30B9\u30EC\u30C3\u30C9\u30B0\u30EB\u30FC\u30D7: {0}"},
            {"appletsecurityexception.checkexit", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u7D42\u4E86: {0}"},
            {"appletsecurityexception.checkexec", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u5B9F\u884C: {0}"},
            {"appletsecurityexception.checklink", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30EA\u30F3\u30AF: {0}"},
            {"appletsecurityexception.checkpropsaccess", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30D7\u30ED\u30D1\u30C6\u30A3"},
            {"appletsecurityexception.checkpropsaccess.key", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30D7\u30ED\u30D1\u30C6\u30A3\u30FB\u30A2\u30AF\u30BB\u30B9{0}"},
            {"appletsecurityexception.checkread.exception1", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: {0}, {1}"},
            {"appletsecurityexception.checkread.exception2", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: file.read: {0}"},
            {"appletsecurityexception.checkread", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: file.read: {0} == {1}"},
            {"appletsecurityexception.checkwrite.exception", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: {0}, {1}"},
            {"appletsecurityexception.checkwrite", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: file.write: {0} == {1}"},
            {"appletsecurityexception.checkread.fd", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: fd.read"},
            {"appletsecurityexception.checkwrite.fd", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: fd.write"},
            {"appletsecurityexception.checklisten", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: socket.listen: {0}"},
            {"appletsecurityexception.checkaccept", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: socket.accept: {0}:{1}"},
            {"appletsecurityexception.checkconnect.networknone", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: socket.connect: {0}->{1}"},
            {"appletsecurityexception.checkconnect.networkhost1", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: {1}\u306E\u8D77\u70B9\u3092\u4F7F\u7528\u3057\u3066{0}\u306B\u63A5\u7D9A\u3067\u304D\u307E\u305B\u3093\u3067\u3057\u305F\u3002"},
            {"appletsecurityexception.checkconnect.networkhost2", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30DB\u30B9\u30C8{0}\u307E\u305F\u306F{1}\u306EIP\u3092\u89E3\u6C7A\u3067\u304D\u307E\u305B\u3093\u3067\u3057\u305F\u3002 "},
            {"appletsecurityexception.checkconnect.networkhost3", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30DB\u30B9\u30C8{0}\u306EIP\u3092\u89E3\u6C7A\u3067\u304D\u307E\u305B\u3093\u3067\u3057\u305F\u3002trustProxy\u30D7\u30ED\u30D1\u30C6\u30A3\u3092\u53C2\u7167\u3057\u3066\u304F\u3060\u3055\u3044\u3002"},
            {"appletsecurityexception.checkconnect", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u63A5\u7D9A: {0}->{1}"},
            {"appletsecurityexception.checkpackageaccess", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30D1\u30C3\u30B1\u30FC\u30B8\u306B\u30A2\u30AF\u30BB\u30B9\u3067\u304D\u307E\u305B\u3093: {0}"},
            {"appletsecurityexception.checkpackagedefinition", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30D1\u30C3\u30B1\u30FC\u30B8\u3092\u5B9A\u7FA9\u3067\u304D\u307E\u305B\u3093: {0}"},
            {"appletsecurityexception.cannotsetfactory", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30D5\u30A1\u30AF\u30C8\u30EA\u3092\u8A2D\u5B9A\u3067\u304D\u307E\u305B\u3093"},
            {"appletsecurityexception.checkmemberaccess", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30E1\u30F3\u30D0\u30FC\u30FB\u30A2\u30AF\u30BB\u30B9\u306E\u78BA\u8A8D"},
            {"appletsecurityexception.checkgetprintjob", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: getPrintJob"},
            {"appletsecurityexception.checksystemclipboardaccess", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: getSystemClipboard"},
            {"appletsecurityexception.checkawteventqueueaccess", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: getEventQueue"},
            {"appletsecurityexception.checksecurityaccess", "\u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u306E\u4F8B\u5916: \u30BB\u30AD\u30E5\u30EA\u30C6\u30A3\u64CD\u4F5C: {0}"},
            {"appletsecurityexception.getsecuritycontext.unknown", "\u4E0D\u660E\u306A\u30AF\u30E9\u30B9\u30ED\u30FC\u30C0\u30FC\u30FB\u30BF\u30A4\u30D7\u3067\u3059\u3002getContext\u3092\u78BA\u8A8D\u3067\u304D\u307E\u305B\u3093"},
            {"appletsecurityexception.checkread.unknown", "\u4E0D\u660E\u306A\u30AF\u30E9\u30B9\u30ED\u30FC\u30C0\u30FC\u30FB\u30BF\u30A4\u30D7\u3067\u3059\u3002{0}\u306E\u8AAD\u53D6\u308A\u30C1\u30A7\u30C3\u30AF\u3092\u78BA\u8A8D\u3067\u304D\u307E\u305B\u3093"},
            {"appletsecurityexception.checkconnect.unknown", "\u4E0D\u660E\u306A\u30AF\u30E9\u30B9\u30ED\u30FC\u30C0\u30FC\u30FB\u30BF\u30A4\u30D7\u3067\u3059\u3002\u63A5\u7D9A\u30C1\u30A7\u30C3\u30AF\u3092\u78BA\u8A8D\u3067\u304D\u307E\u305B\u3093"},
        };

        return temp;
    }
}
