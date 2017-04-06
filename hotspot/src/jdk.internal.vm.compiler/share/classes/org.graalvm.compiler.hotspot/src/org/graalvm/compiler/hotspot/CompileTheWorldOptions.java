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
package org.graalvm.compiler.hotspot;

import org.graalvm.compiler.options.Option;
import org.graalvm.compiler.options.OptionType;
import org.graalvm.compiler.options.OptionKey;

/**
 * Options related to {@link CompileTheWorld}.
 *
 * Note: This must be a top level class to work around for
 * <a href="https://bugs.eclipse.org/bugs/show_bug.cgi?id=477597">Eclipse bug 477597</a>.
 */
public class CompileTheWorldOptions {
    // @formatter:off
    @Option(help = "Class path denoting methods to compile", type = OptionType.Debug)
    public static final OptionKey<String> CompileTheWorldClasspath = new OptionKey<>(CompileTheWorld.SUN_BOOT_CLASS_PATH);
    @Option(help = "Verbose CompileTheWorld operation", type = OptionType.Debug)
    public static final OptionKey<Boolean> CompileTheWorldVerbose = new OptionKey<>(true);
    @Option(help = "The number of CompileTheWorld iterations to perform", type = OptionType.Debug)
    public static final OptionKey<Integer> CompileTheWorldIterations = new OptionKey<>(1);
    @Option(help = "Only compile methods matching this filter", type = OptionType.Debug)
    public static final OptionKey<String> CompileTheWorldMethodFilter = new OptionKey<>(null);
    @Option(help = "Exclude methods matching this filter from compilation", type = OptionType.Debug)
    public static final OptionKey<String> CompileTheWorldExcludeMethodFilter = new OptionKey<>(null);
    @Option(help = "First class to consider when using -XX:+CompileTheWorld", type = OptionType.Debug)
    public static final OptionKey<Integer> CompileTheWorldStartAt = new OptionKey<>(1);
    @Option(help = "Last class to consider when using -XX:+CompileTheWorld", type = OptionType.Debug)
    public static final OptionKey<Integer> CompileTheWorldStopAt = new OptionKey<>(Integer.MAX_VALUE);
    @Option(help = "Option value overrides to use during compile the world. For example, " +
                   "to disable inlining and partial escape analysis specify 'PartialEscapeAnalysis=false Inline=false'. " +
                   "The format for each option is the same as on the command line just without the '-Dgraal.' prefix.", type = OptionType.Debug)
    public static final OptionKey<String> CompileTheWorldConfig = new OptionKey<>(null);

    @Option(help = "Run CTW using as many threads as there are processors on the system", type = OptionType.Debug)
    public static final OptionKey<Boolean> CompileTheWorldMultiThreaded = new OptionKey<>(false);
    @Option(help = "Number of threads to use for multithreaded CTW.  Defaults to Runtime.getRuntime().availableProcessors()", type = OptionType.Debug)
    public static final OptionKey<Integer> CompileTheWorldThreads = new OptionKey<>(0);
    // @formatter:on
}
