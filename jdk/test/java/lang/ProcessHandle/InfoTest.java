/*
 * Copyright (c) 2014, 2015, Oracle and/or its affiliates. All rights reserved.
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

import java.io.File;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.lang.ProcessBuilder;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.UserPrincipal;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Random;
import java.util.concurrent.TimeUnit;

import jdk.testlibrary.Platform;
import jdk.testlibrary.Utils;

import org.testng.Assert;
import org.testng.annotations.Test;
import org.testng.TestNG;

/*
 * @test
 * @build jdk.testlibrary.*
 * @library /lib/testlibrary
 * @summary Functions of ProcessHandle.Info
 * @author Roger Riggs
 */

public class InfoTest {

    static String whoami;

    static {
        try {
            // Create a file and take the username from the file
            Path p = Paths.get("OwnerName.tmp");
            Files.createFile(p);
            UserPrincipal owner = Files.getOwner(p);
            whoami = owner.getName();
            Files.delete(p);
        } catch (IOException ex) {
            ex.printStackTrace();
            throw new UncheckedIOException("tmp file", ex);
        }
    }

    // Main can be used to run the tests from the command line with only testng.jar.
    @SuppressWarnings("raw_types")
    public static void main(String[] args) {
        Class<?>[] testclass = {InfoTest.class};
        TestNG testng = new TestNG();
        testng.setTestClasses(testclass);
        testng.run();
    }

    /**
     * Test that cputime used shows up in ProcessHandle.info
     */
    @Test
    public static void test1() {
        System.out.println("Note: when run in samevm mode the cputime of the " +
                "test runner is included.");
        ProcessHandle self = ProcessHandle.current();

        Duration someCPU = Duration.ofMillis(200L);
        Instant end = Instant.now().plus(someCPU);
        while (Instant.now().isBefore(end)) {
            // waste the cpu
        }
        ProcessHandle.Info info = self.info();
        System.out.printf(" info: %s%n", info);
        Optional<Duration> totalCpu = info.totalCpuDuration();
        if (totalCpu.isPresent() && (totalCpu.get().compareTo(someCPU) < 0)) {
            Assert.fail("reported cputime less than expected: " + someCPU + ", " +
                    "actual: " + info.totalCpuDuration());
        }
    }

    /**
     * Spawn a child with arguments and check they are visible via the ProcessHandle.
     */
    @Test
    public static void test2() {
        try {
            long cpuLoopTime = 100;             // 100 ms
            String[] extraArgs = {"pid", "parent", "stdin"};
            JavaChild p1 = JavaChild.spawnJavaChild((Object[])extraArgs);
            Instant afterStart = Instant.now();

            try (BufferedReader lines = p1.outputReader()) {
                Duration lastCpu = Duration.ofMillis(0L);
                for (int j = 0; j < 10; j++) {

                    p1.sendAction("cpuloop", cpuLoopTime);
                    p1.sendAction("cputime", "");

                    // Read cputime from child
                    Duration childCpuTime = null;
                    // Read lines from the child until the result from cputime is returned
                    String s;
                    while ((s = lines.readLine()) != null) {
                        String[] split = s.trim().split(" ");
                        if (split.length == 3 && split[1].equals("cputime")) {
                            long nanos = Long.valueOf(split[2]);
                            childCpuTime = Duration.ofNanos(nanos);
                            break;      // found the result we're looking for
                        }
                    }


                    ProcessHandle.Info info = p1.info();
                    System.out.printf(" info: %s%n", info);

                    if (info.user().isPresent()) {
                        String user = info.user().get();
                        Assert.assertNotNull(user, "User name");
                        Assert.assertEquals(user, whoami, "User name");
                    }

                    Optional<String> command = info.command();
                    if (command.isPresent()) {
                        String javaExe = System.getProperty("test.jdk") +
                                File.separator + "bin" + File.separator + "java";
                        String expected = Platform.isWindows() ? javaExe + ".exe" : javaExe;
                        Path expectedPath = Paths.get(expected);
                        Path actualPath = Paths.get(command.get());
                        Assert.assertTrue(Files.isSameFile(expectedPath, actualPath),
                                "Command: expected: " + javaExe + ", actual: " + command.get());
                    }

                    if (info.arguments().isPresent()) {
                        String[] args = info.arguments().get();

                        if (Platform.isLinux() || Platform.isOSX()) {
                            int offset = args.length - extraArgs.length;
                            for (int i = 0; i < extraArgs.length; i++) {
                                Assert.assertEquals(args[offset + i], extraArgs[i],
                                        "Actual argument mismatch, index: " + i);
                            }
                        } else if (Platform.isSolaris()) {
                            Assert.assertEquals(args.length, 1,
                                    "Expected argument list length: 1");
                            Assert.assertNotNull(args[0],
                                    "Expected an argument");
                        } else {
                            System.out.printf("No argument test for OS: %s%n", Platform.getOsName());
                        }

                        // Now check that the first argument is not the same as the executed command
                        if (args.length > 0) {
                            Assert.assertNotEquals(args[0], command,
                                    "First argument should not be the executable: args[0]: "
                                            + args[0] + ", command: " + command);
                        }
                    }

                    if (info.totalCpuDuration().isPresent()) {
                        Duration totalCPU = info.totalCpuDuration().get();
                        Duration epsilon = Duration.ofMillis(200L);
                        if (childCpuTime != null) {
                            System.out.printf(" info.totalCPU: %s, childCpuTime: %s, diff: %s%n",
                                    totalCPU.toNanos(), childCpuTime.toNanos(),
                                    childCpuTime.toNanos() - totalCPU.toNanos());
                            Assert.assertTrue(checkEpsilon(childCpuTime, totalCPU, epsilon),
                                    childCpuTime + " should be within " +
                                            epsilon + " of " + totalCPU);
                        }
                        Assert.assertTrue(totalCPU.toNanos() > 0L,
                                "total cpu time expected > 0ms, actual: " + totalCPU);
                        long t = Utils.adjustTimeout(10L);  // Adjusted timeout seconds
                        Assert.assertTrue(totalCPU.toNanos() < lastCpu.toNanos() + t * 1_000_000_000L,
                                "total cpu time expected < " + t
                                        + " seconds more than previous iteration, actual: "
                                        + (totalCPU.toNanos() - lastCpu.toNanos()));
                        lastCpu = totalCPU;
                    }

                    if (info.startInstant().isPresent()) {
                        Instant startTime = info.startInstant().get();
                        Assert.assertTrue(startTime.isBefore(afterStart),
                                "startTime after process spawn completed"
                                        + startTime + " + > " + afterStart);
                    }
                }
            }
            p1.waitFor(Utils.adjustTimeout(5), TimeUnit.SECONDS);
        } catch (IOException | InterruptedException ie) {
            ie.printStackTrace(System.out);
            Assert.fail("unexpected exception", ie);
        }
    }

    /**
     * Spawn a child with arguments and check they are visible via the ProcessHandle.
     */
    @Test
    public static void test3() {
        try {
            for (int sleepTime : Arrays.asList(1, 2)) {
                Process p = spawn("sleep", String.valueOf(sleepTime));
                ProcessHandle.Info info = p.info();
                System.out.printf(" info: %s%n", info);

                if (info.user().isPresent()) {
                    String user = info.user().get();
                    Assert.assertNotNull(user);
                    Assert.assertEquals(user, whoami);
                }
                if (info.command().isPresent()) {
                    String command = info.command().get();
                    String expected = Platform.isWindows() ? "sleep.exe" : "sleep";
                    Assert.assertTrue(command.endsWith(expected), "Command: expected: \'" +
                            expected + "\', actual: " + command);

                    // Verify the command exists and is executable
                    File exe = new File(command);
                    Assert.assertTrue(exe.exists(), "command must exist: " + exe);
                    Assert.assertTrue(exe.canExecute(), "command must be executable: " + exe);
                }
                if (info.arguments().isPresent()) {
                    String[] args = info.arguments().get();
                    if (args.length > 0) {
                        Assert.assertEquals(args[0], String.valueOf(sleepTime));
                    }
                }
                Assert.assertTrue(p.waitFor(15, TimeUnit.SECONDS));
            }
        } catch (IOException | InterruptedException ex) {
            ex.printStackTrace(System.out);
        } finally {
            // Destroy any children that still exist
            ProcessUtil.destroyProcessTree(ProcessHandle.current());
        }
    }

    /**
     * Cross check the cputime reported from java.management with that for the current process.
     */
    @Test
    public static void test4() {
        Duration myCputime1 = ProcessUtil.MXBeanCpuTime();

        Optional<Duration> dur1 = ProcessHandle.current().info().totalCpuDuration();

        Duration myCputime2 = ProcessUtil.MXBeanCpuTime();

        Optional<Duration> dur2 = ProcessHandle.current().info().totalCpuDuration();

        if (dur1.isPresent() && dur2.isPresent()) {
            Duration total1 = dur1.get();
            Duration total2 = dur2.get();
            System.out.printf(" total1 vs. mbean: %s, getProcessCpuTime: %s, diff: %s%n",
                    Objects.toString(total1), myCputime1, myCputime1.minus(total1));
            System.out.printf(" total2 vs. mbean: %s, getProcessCpuTime: %s, diff: %s%n",
                    Objects.toString(total2), myCputime2, myCputime2.minus(total2));

            Duration epsilon = Duration.ofMillis(200L);      // Epsilon is 200ms.
            Assert.assertTrue(checkEpsilon(myCputime1, myCputime2, epsilon),
                    myCputime1.toNanos() + " should be within " + epsilon
                            + " of " + myCputime2.toNanos());
            Assert.assertTrue(checkEpsilon(total1, total2, epsilon),
                    total1.toNanos() + " should be within " + epsilon
                            + " of " + total2.toNanos());
            Assert.assertTrue(checkEpsilon(myCputime1, total1, epsilon),
                    myCputime1.toNanos() + " should be within " + epsilon
                            + " of " + total1.toNanos());
            Assert.assertTrue(checkEpsilon(total1, myCputime2, epsilon),
                    total1.toNanos() + " should be within " + epsilon
                            + " of " + myCputime2.toNanos());
            Assert.assertTrue(checkEpsilon(myCputime2, total2, epsilon),
                    myCputime2.toNanos() + " should be within " + epsilon
                            + " of " + total2.toNanos());
        }
    }

    @Test
    public static void test5() {
        ProcessHandle self = ProcessHandle.current();
        Random r = new Random();
        for (int i = 0; i < 30; i++) {
            Instant end = Instant.now().plusMillis(500L);
            while (end.isBefore(Instant.now())) {
                // burn the cpu time checking the time
                long x = r.nextLong();
            }
            if (self.info().totalCpuDuration().isPresent()) {
                Duration totalCpu = self.info().totalCpuDuration().get();
                long infoTotalCputime = totalCpu.toNanos();
                long beanCputime = ProcessUtil.MXBeanCpuTime().toNanos();
                System.out.printf(" infoTotal: %12d, beanCpu: %12d, diff: %12d%n",
                        infoTotalCputime, beanCputime, beanCputime - infoTotalCputime);
            } else {
                break;  // nothing to compare; continue
            }
        }
    }
    /**
     * Check two Durations, the second should be greater than the first or
     * within the supplied Epsilon.
     * @param d1 a Duration - presumed to be shorter
     * @param d2 a 2nd Duration - presumed to be greater (or within Epsilon)
     * @param epsilon Epsilon the amount of overlap allowed
     * @return true if d2 is greater than d1 or within epsilon, false otherwise
     */
    static boolean checkEpsilon(Duration d1, Duration d2, Duration epsilon) {
        if (d1.toNanos() <= d2.toNanos()) {
            return true;
        }
        Duration diff = d1.minus(d2).abs();
        return diff.compareTo(epsilon) <= 0;
    }

    /**
     * Spawn a native process with the provided arguments.
     * @param command the executable of native process
     * @param args to start a new process
     * @return the Process that was started
     * @throws IOException thrown by ProcessBuilder.start
     */
    static Process spawn(String command, String... args) throws IOException {
        ProcessBuilder pb = new ProcessBuilder();
        pb.inheritIO();
        List<String> list = new ArrayList<>();
        list.add(command);
        for (String arg : args)
            list.add(arg);
        pb.command(list);
        return pb.start();
    }
}
