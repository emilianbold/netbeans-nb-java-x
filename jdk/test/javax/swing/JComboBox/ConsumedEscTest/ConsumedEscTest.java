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

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

/*
  @test
  @bug 8031485
  @summary Combo box consuming escape and enter key events
  @author Petr Pchelko
  @library ../../../../lib/testlibrary/
  @build ExtendedRobot
  @run main ConsumedEscTest
*/
public class ConsumedEscTest {
    private static volatile JFrame frame;
    private static volatile boolean passed = false;

    public static void main(String... args) throws Exception {
        try {
            SwingUtilities.invokeAndWait(() -> {
                frame = new JFrame();
                JComboBox<String> combo = new JComboBox<>(new String[]{"one", "two", "three"});
                JPanel panel = new JPanel();
                panel.add(combo);
                combo.requestFocusInWindow();
                frame.setBounds(100, 150, 300, 100);
                addAction(panel);
                frame.add(panel);
                frame.setVisible(true);
            });

            ExtendedRobot robot = new ExtendedRobot();
            robot.waitForIdle();
            robot.type(KeyEvent.VK_ESCAPE);
            robot.waitForIdle();
            if (!passed) {
                throw new RuntimeException("FAILED: ESC was consumed by combo box");
            }
        } finally {
            if (frame != null) {
                frame.dispose();
            }
        }
    }

    private static void addAction(JComponent comp) {
        KeyStroke k = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
        Object actionKey = "cancel";
        comp.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(k, actionKey);
        Action cancelAction = new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent ev) {
                passed = true;
            }
        };
        comp.getActionMap().put(actionKey, cancelAction);
    }

}
