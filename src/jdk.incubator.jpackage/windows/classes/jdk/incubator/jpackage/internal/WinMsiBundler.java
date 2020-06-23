/*
 * Copyright (c) 2012, 2020, Oracle and/or its affiliates. All rights reserved.
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

package jdk.incubator.jpackage.internal;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.MessageFormat;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static jdk.incubator.jpackage.internal.OverridableResource.createResource;
import static jdk.incubator.jpackage.internal.StandardBundlerParam.APP_NAME;
import static jdk.incubator.jpackage.internal.StandardBundlerParam.CONFIG_ROOT;
import static jdk.incubator.jpackage.internal.StandardBundlerParam.DESCRIPTION;
import static jdk.incubator.jpackage.internal.StandardBundlerParam.FA_CONTENT_TYPE;
import static jdk.incubator.jpackage.internal.StandardBundlerParam.FILE_ASSOCIATIONS;
import static jdk.incubator.jpackage.internal.StandardBundlerParam.LICENSE_FILE;
import static jdk.incubator.jpackage.internal.StandardBundlerParam.TEMP_ROOT;
import static jdk.incubator.jpackage.internal.StandardBundlerParam.VENDOR;
import static jdk.incubator.jpackage.internal.StandardBundlerParam.VERSION;

/**
 * WinMsiBundler
 *
 * Produces .msi installer from application image. Uses WiX Toolkit to build
 * .msi installer.
 * <p>
 * {@link #execute} method creates a number of source files with the description
 * of installer to be processed by WiX tools. Generated source files are stored
 * in "config" subdirectory next to "app" subdirectory in the root work
 * directory. The following WiX source files are generated:
 * <ul>
 * <li>main.wxs. Main source file with the installer description
 * <li>bundle.wxf. Source file with application and Java run-time directory tree
 * description.
 * </ul>
 * <p>
 * main.wxs file is a copy of main.wxs resource from
 * jdk.incubator.jpackage.internal.resources package. It is parametrized with the
 * following WiX variables:
 * <ul>
 * <li>JpAppName. Name of the application. Set to the value of --name command
 * line option
 * <li>JpAppVersion. Version of the application. Set to the value of
 * --app-version command line option
 * <li>JpAppVendor. Vendor of the application. Set to the value of --vendor
 * command line option
 * <li>JpAppDescription. Description of the application. Set to the value of
 * --description command line option
 * <li>JpProductCode. Set to product code UUID of the application. Random value
 * generated by jpackage every time {@link #execute} method is called
 * <li>JpProductUpgradeCode. Set to upgrade code UUID of the application. Random
 * value generated by jpackage every time {@link #execute} method is called if
 * --win-upgrade-uuid command line option is not specified. Otherwise this
 * variable is set to the value of --win-upgrade-uuid command line option
 * <li>JpAllowDowngrades. Set to "yes" if --win-upgrade-uuid command line option
 * was specified. Undefined otherwise
 * <li>JpLicenseRtf. Set to the value of --license-file command line option.
 * Undefined is --license-file command line option was not specified
 * <li>JpInstallDirChooser. Set to "yes" if --win-dir-chooser command line
 * option was specified. Undefined otherwise
 * <li>JpConfigDir. Absolute path to the directory with generated WiX source
 * files.
 * <li>JpIsSystemWide. Set to "yes" if --win-per-user-install command line
 * option was not specified. Undefined otherwise
 * </ul>
 */
public class WinMsiBundler  extends AbstractBundler {

    public static final BundlerParamInfo<File> MSI_IMAGE_DIR =
            new StandardBundlerParam<>(
            "win.msi.imageDir",
            File.class,
            params -> {
                File imagesRoot = IMAGES_ROOT.fetchFrom(params);
                if (!imagesRoot.exists()) imagesRoot.mkdirs();
                return new File(imagesRoot, "win-msi.image");
            },
            (s, p) -> null);

    public static final BundlerParamInfo<File> WIN_APP_IMAGE =
            new StandardBundlerParam<>(
            "win.app.image",
            File.class,
            null,
            (s, p) -> null);

    public static final StandardBundlerParam<Boolean> MSI_SYSTEM_WIDE  =
            new StandardBundlerParam<>(
                    Arguments.CLIOptions.WIN_PER_USER_INSTALLATION.getId(),
                    Boolean.class,
                    params -> true, // MSIs default to system wide
                    // valueOf(null) is false,
                    // and we actually do want null
                    (s, p) -> (s == null || "null".equalsIgnoreCase(s))? null
                            : Boolean.valueOf(s)
            );


    public static final StandardBundlerParam<String> PRODUCT_VERSION =
            new StandardBundlerParam<>(
                    "win.msi.productVersion",
                    String.class,
                    VERSION::fetchFrom,
                    (s, p) -> s
            );

    private static final BundlerParamInfo<String> UPGRADE_UUID =
            new StandardBundlerParam<>(
            Arguments.CLIOptions.WIN_UPGRADE_UUID.getId(),
            String.class,
            null,
            (s, p) -> s);

    private static final BundlerParamInfo<String> INSTALLER_FILE_NAME =
            new StandardBundlerParam<> (
            "win.installerName",
            String.class,
            params -> {
                String nm = APP_NAME.fetchFrom(params);
                if (nm == null) return null;

                String version = VERSION.fetchFrom(params);
                if (version == null) {
                    return nm;
                } else {
                    return nm + "-" + version;
                }
            },
            (s, p) -> s);

    private static final BundlerParamInfo<Boolean> INSTALLDIR_CHOOSER =
            new StandardBundlerParam<> (
            Arguments.CLIOptions.WIN_DIR_CHOOSER.getId(),
            Boolean.class,
            params -> Boolean.FALSE,
            (s, p) -> Boolean.valueOf(s)
    );

    public WinMsiBundler() {
        appImageBundler = new WinAppBundler().setDependentTask(true);
    }

    @Override
    public String getName() {
        return I18N.getString("msi.bundler.name");
    }

    @Override
    public String getID() {
        return "msi";
    }

    @Override
    public String getBundleType() {
        return "INSTALLER";
    }

    @Override
    public boolean supported(boolean platformInstaller) {
        try {
            if (wixToolset == null) {
                wixToolset = WixTool.toolset();
            }
            return true;
        } catch (ConfigException ce) {
            Log.error(ce.getMessage());
            if (ce.getAdvice() != null) {
                Log.error(ce.getAdvice());
            }
        } catch (Exception e) {
            Log.error(e.getMessage());
        }
        return false;
    }

    @Override
    public boolean isDefault() {
        return false;
    }

    private static UUID getUpgradeCode(Map<String, ? super Object> params) {
        String upgradeCode = UPGRADE_UUID.fetchFrom(params);
        if (upgradeCode != null) {
            return UUID.fromString(upgradeCode);
        }
        return createNameUUID("UpgradeCode", params, List.of(VENDOR, APP_NAME));
    }

    private static UUID getProductCode(Map<String, ? super Object> params) {
        return createNameUUID("ProductCode", params, List.of(VENDOR, APP_NAME,
                VERSION));
    }

    private static UUID createNameUUID(String prefix,
            Map<String, ? super Object> params,
            List<StandardBundlerParam<String>> components) {
        String key = Stream.concat(Stream.of(prefix), components.stream().map(
                c -> c.fetchFrom(params))).collect(Collectors.joining("/"));
        return UUID.nameUUIDFromBytes(key.getBytes(StandardCharsets.UTF_8));
    }

    @Override
    public boolean validate(Map<String, ? super Object> params)
            throws ConfigException {
        try {
            appImageBundler.validate(params);

            if (wixToolset == null) {
                wixToolset = WixTool.toolset();
            }

            try {
                getUpgradeCode(params);
            } catch (IllegalArgumentException ex) {
                throw new ConfigException(ex);
            }

            for (var toolInfo: wixToolset.values()) {
                Log.verbose(MessageFormat.format(I18N.getString(
                        "message.tool-version"), toolInfo.path.getFileName(),
                        toolInfo.version));
            }

            wixSourcesBuilder.setWixVersion(wixToolset.get(WixTool.Light).version);

            wixSourcesBuilder.logWixFeatures();

            /********* validate bundle parameters *************/

            try {
                String version = PRODUCT_VERSION.fetchFrom(params);
                MsiVersion.of(version);
            } catch (IllegalArgumentException ex) {
                throw new ConfigException(ex.getMessage(), I18N.getString(
                        "error.version-string-wrong-format.advice"), ex);
            }

            FileAssociation.verify(FileAssociation.fetchFrom(params));

            return true;
        } catch (RuntimeException re) {
            if (re.getCause() instanceof ConfigException) {
                throw (ConfigException) re.getCause();
            } else {
                throw new ConfigException(re);
            }
        }
    }

    private void prepareProto(Map<String, ? super Object> params)
                throws PackagerException, IOException {
        File appImage = StandardBundlerParam.getPredefinedAppImage(params);
        File appDir = null;

        // we either have an application image or need to build one
        if (appImage != null) {
            appDir = new File(MSI_IMAGE_DIR.fetchFrom(params),
                    APP_NAME.fetchFrom(params));
            // copy everything from appImage dir into appDir/name
            IOUtils.copyRecursive(appImage.toPath(), appDir.toPath());
        } else {
            appDir = appImageBundler.execute(params, MSI_IMAGE_DIR.fetchFrom(
                    params));
        }

        // Configure installer icon
        if (StandardBundlerParam.isRuntimeInstaller(params)) {
            // Use icon from java launcher.
            // Assume java.exe exists in Java Runtime being packed.
            // Ignore custom icon if any as we don't want to copy anything in
            // Java Runtime image.
            installerIcon = ApplicationLayout.javaRuntime()
                    .resolveAt(appDir.toPath())
                    .runtimeDirectory()
                    .resolve(Path.of("bin", "java.exe"));
        } else {
            installerIcon = ApplicationLayout.windowsAppImage()
                    .resolveAt(appDir.toPath())
                    .launchersDirectory()
                    .resolve(APP_NAME.fetchFrom(params) + ".exe");
        }

        params.put(WIN_APP_IMAGE.getID(), appDir);

        String licenseFile = LICENSE_FILE.fetchFrom(params);
        if (licenseFile != null) {
            // need to copy license file to the working directory
            // and convert to rtf if needed
            File lfile = new File(licenseFile);
            File destFile = new File(CONFIG_ROOT.fetchFrom(params),
                    lfile.getName());

            IOUtils.copyFile(lfile, destFile);
            destFile.setWritable(true);
            ensureByMutationFileIsRTF(destFile);
        }
    }

    @Override
    public File execute(Map<String, ? super Object> params,
            File outputParentDir) throws PackagerException {

        IOUtils.writableOutputDir(outputParentDir.toPath());

        Path imageDir = MSI_IMAGE_DIR.fetchFrom(params).toPath();
        try {
            Files.createDirectories(imageDir);

            prepareProto(params);

            wixSourcesBuilder
            .initFromParams(WIN_APP_IMAGE.fetchFrom(params).toPath(), params)
            .createMainFragment(CONFIG_ROOT.fetchFrom(params).toPath().resolve(
                    "bundle.wxf"));

            Map<String, String> wixVars = prepareMainProjectFile(params);

            new ScriptRunner()
            .setDirectory(imageDir)
            .setResourceCategoryId("resource.post-app-image-script")
            .setScriptNameSuffix("post-image")
            .setEnvironmentVariable("JpAppImageDir", imageDir.toAbsolutePath().toString())
            .run(params);

            return buildMSI(params, wixVars, outputParentDir);
        } catch (IOException ex) {
            Log.verbose(ex);
            throw new PackagerException(ex);
        }
    }

    private Map<String, String> prepareMainProjectFile(
            Map<String, ? super Object> params) throws IOException {
        Map<String, String> data = new HashMap<>();

        final UUID productCode = getProductCode(params);
        final UUID upgradeCode = getUpgradeCode(params);

        data.put("JpProductCode", productCode.toString());
        data.put("JpProductUpgradeCode", upgradeCode.toString());

        Log.verbose(MessageFormat.format(I18N.getString("message.product-code"),
                productCode));
        Log.verbose(MessageFormat.format(I18N.getString("message.upgrade-code"),
                upgradeCode));

        data.put("JpAllowUpgrades", "yes");
        data.put("JpAllowDowngrades", "yes");

        data.put("JpAppName", APP_NAME.fetchFrom(params));
        data.put("JpAppDescription", DESCRIPTION.fetchFrom(params));
        data.put("JpAppVendor", VENDOR.fetchFrom(params));
        data.put("JpAppVersion", PRODUCT_VERSION.fetchFrom(params));
        data.put("JpIcon", installerIcon.toString());

        final Path configDir = CONFIG_ROOT.fetchFrom(params).toPath();

        data.put("JpConfigDir", configDir.toAbsolutePath().toString());

        if (MSI_SYSTEM_WIDE.fetchFrom(params)) {
            data.put("JpIsSystemWide", "yes");
        }

        String licenseFile = LICENSE_FILE.fetchFrom(params);
        if (licenseFile != null) {
            String lname = new File(licenseFile).getName();
            File destFile = new File(CONFIG_ROOT.fetchFrom(params), lname);
            data.put("JpLicenseRtf", destFile.getAbsolutePath());
        }

        // Copy CA dll to include with installer
        if (INSTALLDIR_CHOOSER.fetchFrom(params)) {
            data.put("JpInstallDirChooser", "yes");
            String fname = "wixhelper.dll";
            try (InputStream is = OverridableResource.readDefault(fname)) {
                Files.copy(is, Paths.get(
                        CONFIG_ROOT.fetchFrom(params).getAbsolutePath(),
                        fname));
            }
        }

        // Copy l10n files.
        for (String loc : Arrays.asList("en", "ja", "zh_CN")) {
            String fname = "MsiInstallerStrings_" + loc + ".wxl";
            try (InputStream is = OverridableResource.readDefault(fname)) {
                Files.copy(is, Paths.get(
                        CONFIG_ROOT.fetchFrom(params).getAbsolutePath(),
                        fname));
            }
        }

        createResource("main.wxs", params)
                .setCategory(I18N.getString("resource.main-wix-file"))
                .saveToFile(configDir.resolve("main.wxs"));

        createResource("overrides.wxi", params)
                .setCategory(I18N.getString("resource.overrides-wix-file"))
                .saveToFile(configDir.resolve("overrides.wxi"));

        return data;
    }

    private File buildMSI(Map<String, ? super Object> params,
            Map<String, String> wixVars, File outdir)
            throws IOException {

        File msiOut = new File(
                outdir, INSTALLER_FILE_NAME.fetchFrom(params) + ".msi");

        Log.verbose(MessageFormat.format(I18N.getString(
                "message.preparing-msi-config"), msiOut.getAbsolutePath()));

        WixPipeline wixPipeline = new WixPipeline()
        .setToolset(wixToolset.entrySet().stream().collect(
                Collectors.toMap(
                        entry -> entry.getKey(),
                        entry -> entry.getValue().path)))
        .setWixObjDir(TEMP_ROOT.fetchFrom(params).toPath().resolve("wixobj"))
        .setWorkDir(WIN_APP_IMAGE.fetchFrom(params).toPath())
        .addSource(CONFIG_ROOT.fetchFrom(params).toPath().resolve("main.wxs"), wixVars)
        .addSource(CONFIG_ROOT.fetchFrom(params).toPath().resolve("bundle.wxf"), null);

        Log.verbose(MessageFormat.format(I18N.getString(
                "message.generating-msi"), msiOut.getAbsolutePath()));

        boolean enableLicenseUI = (LICENSE_FILE.fetchFrom(params) != null);
        boolean enableInstalldirUI = INSTALLDIR_CHOOSER.fetchFrom(params);

        if (!MSI_SYSTEM_WIDE.fetchFrom(params)) {
            wixPipeline.addLightOptions("-sice:ICE91");
        }
        if (enableLicenseUI || enableInstalldirUI) {
            wixPipeline.addLightOptions("-ext", "WixUIExtension");
        }

        wixPipeline.addLightOptions("-loc",
                CONFIG_ROOT.fetchFrom(params).toPath().resolve(I18N.getString(
                        "resource.wxl-file-name")).toAbsolutePath().toString());

        // Only needed if we using CA dll, so Wix can find it
        if (enableInstalldirUI) {
            wixPipeline.addLightOptions("-b", CONFIG_ROOT.fetchFrom(params).getAbsolutePath());
        }

        wixPipeline.buildMsi(msiOut.toPath().toAbsolutePath());

        return msiOut;
    }

    private static void ensureByMutationFileIsRTF(File f) {
        if (f == null || !f.isFile()) return;

        try {
            boolean existingLicenseIsRTF = false;

            try (FileInputStream fin = new FileInputStream(f)) {
                byte[] firstBits = new byte[7];

                if (fin.read(firstBits) == firstBits.length) {
                    String header = new String(firstBits);
                    existingLicenseIsRTF = "{\\rtf1\\".equals(header);
                }
            }

            if (!existingLicenseIsRTF) {
                List<String> oldLicense = Files.readAllLines(f.toPath());
                try (Writer w = Files.newBufferedWriter(
                        f.toPath(), Charset.forName("Windows-1252"))) {
                    w.write("{\\rtf1\\ansi\\ansicpg1252\\deff0\\deflang1033"
                            + "{\\fonttbl{\\f0\\fnil\\fcharset0 Arial;}}\n"
                            + "\\viewkind4\\uc1\\pard\\sa200\\sl276"
                            + "\\slmult1\\lang9\\fs20 ");
                    oldLicense.forEach(l -> {
                        try {
                            for (char c : l.toCharArray()) {
                                // 0x00 <= ch < 0x20 Escaped (\'hh)
                                // 0x20 <= ch < 0x80 Raw(non - escaped) char
                                // 0x80 <= ch <= 0xFF Escaped(\ 'hh)
                                // 0x5C, 0x7B, 0x7D (special RTF characters
                                // \,{,})Escaped(\'hh)
                                // ch > 0xff Escaped (\\ud###?)
                                if (c < 0x10) {
                                    w.write("\\'0");
                                    w.write(Integer.toHexString(c));
                                } else if (c > 0xff) {
                                    w.write("\\ud");
                                    w.write(Integer.toString(c));
                                    // \\uc1 is in the header and in effect
                                    // so we trail with a replacement char if
                                    // the font lacks that character - '?'
                                    w.write("?");
                                } else if ((c < 0x20) || (c >= 0x80) ||
                                        (c == 0x5C) || (c == 0x7B) ||
                                        (c == 0x7D)) {
                                    w.write("\\'");
                                    w.write(Integer.toHexString(c));
                                } else {
                                    w.write(c);
                                }
                            }
                            // blank lines are interpreted as paragraph breaks
                            if (l.length() < 1) {
                                w.write("\\par");
                            } else {
                                w.write(" ");
                            }
                            w.write("\r\n");
                        } catch (IOException e) {
                            Log.verbose(e);
                        }
                    });
                    w.write("}\r\n");
                }
            }
        } catch (IOException e) {
            Log.verbose(e);
        }

    }

    private Path installerIcon;
    private Map<WixTool, WixTool.ToolInfo> wixToolset;
    private AppImageBundler appImageBundler;
    private WixSourcesBuilder wixSourcesBuilder = new WixSourcesBuilder();

}
