/*
 * Copyright (c) 2009, 2016, Oracle and/or its affiliates. All rights reserved.
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

/*
 * @test
 * @key headful
 * @bug 6683775 6794764
 * @summary Painting artifacts is seen when panel is made setOpaque(false) for a translucent window
 * @author Alexander Potochkin
 * @modules java.desktop/com.sun.awt
 *          java.desktop/sun.awt
 * @run main bug6683775
 */

import com.sun.awt.AWTUtilities;

import javax.swing.*;
import java.awt.*;
import java.awt.image.BufferedImage;

public class bug6683775 {
    static final int LOC = 100,
            SIZE = 200;

    public static void main(String[] args) throws Exception {
        GraphicsConfiguration gc = getGC();
       if (!AWTUtilities.isTranslucencySupported(
               AWTUtilities.Translucency.PERPIXEL_TRANSLUCENT)
                || gc == null) {
            return;
        }
        Robot robot = new Robot();
        final JFrame testFrame = new JFrame(gc);

        SwingUtilities.invokeAndWait(() -> {
            JFrame backgroundFrame = new JFrame("Background frame");
            backgroundFrame.setUndecorated(true);
            JPanel panel = new JPanel();
            panel.setBackground(Color.RED);
            backgroundFrame.add(panel);
            backgroundFrame.setBounds(LOC, LOC, SIZE, SIZE);
            backgroundFrame.setVisible(true);

            testFrame.setUndecorated(true);
            JPanel p = new JPanel();
            p.setOpaque(false);
            testFrame.add(p);
            AWTUtilities.setWindowOpaque(testFrame, false);
            testFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            testFrame.setBounds(LOC, LOC, SIZE, SIZE);
            testFrame.setVisible(true);
        });

        robot.waitForIdle();
        Thread.sleep(1500);

        //robot.getPixelColor() didn't work right for some reason
        BufferedImage capture =
                robot.createScreenCapture(new Rectangle(LOC, LOC, SIZE, SIZE));

        int redRGB = Color.RED.getRGB();
        if (redRGB != capture.getRGB(SIZE/2, SIZE/2)) {
            throw new RuntimeException("Transparent frame is not transparent!");
        }
    }

    private static GraphicsConfiguration getGC() {
        GraphicsConfiguration transparencyCapableGC =
                GraphicsEnvironment.getLocalGraphicsEnvironment()
                        .getDefaultScreenDevice().getDefaultConfiguration();
        if (!AWTUtilities.isTranslucencyCapable(transparencyCapableGC)) {
            transparencyCapableGC = null;

            GraphicsEnvironment env =
                    GraphicsEnvironment.getLocalGraphicsEnvironment();
            GraphicsDevice[] devices = env.getScreenDevices();

            for (int i = 0; i < devices.length && transparencyCapableGC == null; i++) {
                GraphicsConfiguration[] configs = devices[i].getConfigurations();
                for (int j = 0; j < configs.length && transparencyCapableGC == null; j++) {
                    if (AWTUtilities.isTranslucencyCapable(configs[j])) {
                        transparencyCapableGC = configs[j];
                    }
                }
            }
        }
        return transparencyCapableGC;
    }
}
