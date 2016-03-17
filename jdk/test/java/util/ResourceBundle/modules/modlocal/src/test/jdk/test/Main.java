/*
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved.
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

package jdk.test;

import java.util.Locale;
import java.util.ResourceBundle;
import java.util.ResourceBundle.Control;
import java.util.MissingResourceException;

public class Main {
    public static void main(String[] args) throws Exception {
        int errors = 0;
        for (String loctag : args) {
            Locale locale = Locale.forLanguageTag(loctag);
            if (locale.equals(Locale.ROOT)) {
                continue;
            }
            ResourceBundle rb = ResourceBundle.getBundle("jdk.test.resources.MyResources",
                                                         locale);
            String tag = locale.toLanguageTag(); // normalized
            String value = rb.getString("key");
            System.out.println("locale = " + tag + ", value = " + value);
            if (!value.startsWith(tag + ':')) {
                System.out.println("ERROR: " + value + " expected: " + tag);
                errors++;
            }
        }

        // Make sure ResourceBundle.getBundle throws an UnsupportedOperationException with
        // a ResourceBundle.Control.
        try {
            ResourceBundle rb;
            rb = ResourceBundle.getBundle("jdk.test.resources.MyResources",
                                          Locale.ENGLISH,
                                          Control.getControl(Control.FORMAT_DEFAULT));
            System.out.println("ERROR: no UnsupportedOperationException thrown with a ResourceBundle.Control");
            errors++;
        } catch (UnsupportedOperationException e) {
            // OK
        }

        if (errors > 0) {
            throw new RuntimeException(errors + " errors");
        }
    }
}
