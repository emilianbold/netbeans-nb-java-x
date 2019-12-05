/*
 * Copyright (c) 2019, Oracle and/or its affiliates. All rights reserved.
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

import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.Map;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;
import jdk.jpackage.test.FileAssociations;
import jdk.jpackage.test.PackageType;
import jdk.jpackage.test.PackageTest;
import jdk.jpackage.test.TKit;
import jdk.jpackage.test.Annotations.Test;
import jdk.jpackage.test.*;

/**
 * Test --linux-shortcut parameter. Output of the test should be
 * shortcuthinttest_1.0-1_amd64.deb or shortcuthinttest-1.0-1.amd64.rpm package
 * bundle. The output package should provide the same functionality as the
 * default package and also create a desktop shortcut.
 *
 * Finding a shortcut of the application launcher through GUI depends on desktop
 * environment.
 *
 * deb:
 * Search online for `Ways To Open A Ubuntu Application` for instructions.
 *
 * rpm:
 *
 */

/*
 * @test
 * @summary jpackage with --linux-shortcut
 * @library ../helpers
 * @key jpackagePlatformPackage
 * @requires jpackage.test.SQETest == null
 * @build jdk.jpackage.test.*
 * @requires (os.family == "linux")
 * @modules jdk.incubator.jpackage/jdk.incubator.jpackage.internal
 * @compile ShortcutHintTest.java
 * @run main/othervm/timeout=360 -Xmx512m jdk.jpackage.test.Main
 *  --jpt-run=ShortcutHintTest
 */

/*
 * @test
 * @summary jpackage with --linux-shortcut
 * @library ../helpers
 * @key jpackagePlatformPackage
 * @build jdk.jpackage.test.*
 * @requires (os.family == "linux")
 * @requires jpackage.test.SQETest != null
 * @modules jdk.incubator.jpackage/jdk.incubator.jpackage.internal
 * @compile ShortcutHintTest.java
 * @run main/othervm/timeout=360 -Xmx512m jdk.jpackage.test.Main
 *  --jpt-run=ShortcutHintTest.testBasic
 */

public class ShortcutHintTest {

    @Test
    public static void testBasic() {
        createTest().addInitializer(cmd -> {
            cmd.addArgument("--linux-shortcut");
        }).run();
    }

    private static PackageTest createTest() {
        return new PackageTest()
                .forTypes(PackageType.LINUX)
                .configureHelloApp()
                .addBundleDesktopIntegrationVerifier(true);

    }

    /**
     * Adding `--icon` to jpackage command line should create desktop shortcut
     * even though `--linux-shortcut` is omitted.
     */
    @Test
    public static void testCustomIcon() {
        createTest().addInitializer(cmd -> {
            cmd.setFakeRuntime();
            cmd.addArguments("--icon", TKit.TEST_SRC_ROOT.resolve(
                    "apps/dukeplug.png"));
        }).run();
    }

    /**
     * Adding `--file-associations` to jpackage command line should create
     * desktop shortcut even though `--linux-shortcut` is omitted.
     */
    @Test
    public static void testFileAssociations() {
        PackageTest test = createTest().addInitializer(
                JPackageCommand::setFakeRuntime);
        new FileAssociations("ShortcutHintTest_testFileAssociations").applyTo(
                test);
        test.run();
    }

    /**
     * Additional launcher with icon should create desktop shortcut even though
     * `--linux-shortcut` is omitted.
     */
    @Test
    public static void testAdditionaltLaunchers() {
        createTest().addInitializer(cmd -> {
            cmd.setFakeRuntime();

            final String launcherName = "Foo";
            final Path propsFile = TKit.workDir().resolve(
                    launcherName + ".properties");

            cmd.addArguments("--add-launcher", String.format("%s=%s",
                    launcherName, propsFile));

            TKit.createPropertiesFile(propsFile, Map.entry("icon",
                    TKit.TEST_SRC_ROOT.resolve("apps/dukeplug.png").toString()));
        }).run();
    }

    /**
     * .desktop file from resource dir.
     */
    @Test
    public static void testDesktopFileFromResourceDir() {
        final String expectedVersionString = "Version=12345678";
        TKit.withTempDirectory("resources", tempDir -> {
            createTest().addInitializer(cmd -> {
                cmd.setFakeRuntime();

                cmd.addArgument("--linux-shortcut");
                cmd.addArguments("--resource-dir", tempDir);

                // Create custom .desktop file in resource directory
                TKit.createTextFile(tempDir.resolve(cmd.name() + ".desktop"),
                        List.of(
                                "[Desktop Entry]",
                                "Name=APPLICATION_NAME",
                                "Exec=APPLICATION_LAUNCHER",
                                "Terminal=false",
                                "Type=Application",
                                "Categories=DEPLOY_BUNDLE_CATEGORY",
                                expectedVersionString
                        ));
            })
            .addInstallVerifier(cmd -> {
                Path desktopFile = cmd.appLayout().destktopIntegrationDirectory().resolve(
                        String.format("%s-%s.desktop",
                                LinuxHelper.getPackageName(cmd), cmd.name()));
                TKit.assertFileExists(desktopFile);
                TKit.assertTextStream(expectedVersionString)
                        .label(String.format("[%s] file", desktopFile))
                        .predicate(String::equals)
                        .apply(Files.readAllLines(desktopFile).stream());
            }).run();
        });
    }
}
