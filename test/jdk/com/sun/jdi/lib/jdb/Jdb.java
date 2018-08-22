/*
 * Copyright (c) 2018, Oracle and/or its affiliates. All rights reserved.
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

package lib.jdb;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import jdk.test.lib.JDKToolFinder;
import jdk.test.lib.process.ProcessTools;
import jdk.test.lib.process.StreamPumper;

public class Jdb {

    public static class LaunchOptions {
        public final String debuggeeClass;
        public final List<String> debuggeeOptions = new LinkedList<>();

        public LaunchOptions(String debuggeeClass) {
            this.debuggeeClass = debuggeeClass;
        }
        public LaunchOptions addDebuggeeOption(String option) {
            debuggeeOptions.add(option);
            return this;
        }
        public LaunchOptions addDebuggeeOptions(String[] options) {
            debuggeeOptions.addAll(Arrays.asList(options));
            return this;
        }
    }

    public static Jdb launchLocal(LaunchOptions options) {
        return new Jdb(options);
    }

    public static Jdb launchLocal(String debuggeeClass) {
        return new Jdb(new LaunchOptions(debuggeeClass));
    }

    public Jdb(LaunchOptions options) {
        /* run debuggee as:
            java -agentlib:jdwp=transport=dt_socket,address=0,server=n,suspend=y <debuggeeClass>
        it reports something like : Listening for transport dt_socket at address: 60810
        after that connect jdb by:
            jdb -connect com.sun.jdi.SocketAttach:port=60810
        */
        // launch debuggee
        List<String> debuggeeArgs = new LinkedList<>();
        // specify address=0 to automatically select free port
        debuggeeArgs.add("-agentlib:jdwp=transport=dt_socket,address=0,server=y,suspend=y");
        debuggeeArgs.addAll(options.debuggeeOptions);
        debuggeeArgs.add(options.debuggeeClass);
        ProcessBuilder pbDebuggee = ProcessTools.createJavaProcessBuilder(true, debuggeeArgs.toArray(new String[0]));

        // debuggeeListen[0] - transport, debuggeeListen[1] - address
        String[] debuggeeListen = new String[2];
        Pattern listenRegexp = Pattern.compile("Listening for transport \\b(.+)\\b at address: \\b(\\d+)\\b");
        try {
            debuggee = ProcessTools.startProcess("debuggee", pbDebuggee,
                    s -> debuggeeOutput.add(s),  // output consumer
                    s -> {  // warm-up predicate
                        Matcher m = listenRegexp.matcher(s);
                        if (!m.matches()) {
                            return false;
                        }
                        debuggeeListen[0] = m.group(1);
                        debuggeeListen[1] = m.group(2);
                        return true;
                    },
                    30, TimeUnit.SECONDS);
        } catch (IOException | InterruptedException | TimeoutException ex) {
            throw new RuntimeException("failed to launch debuggee", ex);
        }

        // launch jdb
        try {
            ProcessBuilder pb = new ProcessBuilder(JDKToolFinder.getTestJDKTool("jdb"));
            pb.command().add("-connect");
            pb.command().add("com.sun.jdi.SocketAttach:port=" + debuggeeListen[1]);
            System.out.println("Launching jdb:" + pb.command().stream().collect(Collectors.joining(" ")));
            try {
                jdb = pb.start();
            } catch (IOException ex) {
                throw new RuntimeException("failed to launch pdb", ex);
            }
            StreamPumper stdout = new StreamPumper(jdb.getInputStream());
            StreamPumper stderr = new StreamPumper(jdb.getErrorStream());

            stdout.addPump(new StreamPumper.StreamPump(outputHandler));
            stderr.addPump(new StreamPumper.StreamPump(outputHandler));

            stdout.process();
            stderr.process();

            inputWriter = new PrintWriter(jdb.getOutputStream(), true);
        } catch (Throwable ex) {
            // terminate debuggee if something went wrong
            debuggee.destroy();
            throw ex;
        }
    }

    private final Process jdb;
    private final Process debuggee;
    private final OutputHandler outputHandler = new OutputHandler();
    private final PrintWriter inputWriter;
    // contains all jdb output (to be used by getJdbOutput())
    private final List<String> jdbOutput = new LinkedList<>();
    private final List<String> debuggeeOutput = new LinkedList<>();

    private static final String lineSeparator = System.getProperty("line.separator");
    // wait time before check jdb output (in ms)
    private static long sleepTime = 1000;
    // max time to wait for  jdb output (in ms)
    private static long timeout = 60000;

    // jdb prompt when debuggee is not started nor suspended after breakpoint
    public static final String SIMPLE_PROMPT = "> ";
    // pattern for message of a breakpoint hit
    public static final String BREAKPOINT_HIT = "Breakpoint hit:";
    // pattern for message of an application exit
    public static final String APPLICATION_EXIT = "The application exited";
    // pattern for message of an application disconnect
    public static final String APPLICATION_DISCONNECTED = "The application has been disconnected";


    // waits until the process shutdown or crash
    public boolean waitFor(long timeout, TimeUnit unit) {
        try {
            return jdb.waitFor(timeout, unit);
        } catch (InterruptedException e) {
            return false;
        }
    }

    public void shutdown() {
        // shutdown jdb
        if (jdb.isAlive()) {
            try {
                quit();
                // wait some time after the command for the process termination
                waitFor(10, TimeUnit.SECONDS);
            } finally {
                if (jdb.isAlive()) {
                    jdb.destroy();
                }
            }
        }
        // shutdown debuggee
        if (debuggee.isAlive()) {
            try {
                debuggee.waitFor(10, TimeUnit.SECONDS);
            } catch (InterruptedException e) {
                // ignore
            } finally {
                if (debuggee.isAlive()) {
                    debuggee.destroy();
                }
            }
        }
    }


    // waits until string {@pattern} appears in the jdb output, within the last {@code lines} lines.
    /* Comment from original /test/jdk/com/sun/jdi/ShellScaffold.sh
        # Now we have to wait for the next jdb prompt.  We wait for a pattern
        # to appear in the last line of jdb output.  Normally, the prompt is
        #
        # 1) ^main[89] @
        #
        # where ^ means start of line, and @ means end of file with no end of line
        # and 89 is the current command counter. But we have complications e.g.,
        # the following jdb output can appear:
        #
        # 2) a[89] = 10
        #
        # The above form is an array assignment and not a prompt.
        #
        # 3) ^main[89] main[89] ...
        #
        # This occurs if the next cmd is one that causes no jdb output, e.g.,
        # 'trace methods'.
        #
        # 4) ^main[89] [main[89]] .... > @
        #
        # jdb prints a > as a prompt after something like a cont.
        # Thus, even though the above is the last 'line' in the file, it
        # isn't the next prompt we are waiting for after the cont completes.
        # HOWEVER, sometimes we see this for a cont command:
        #
        #   ^main[89] $
        #      <lines output for hitting a bkpt>
        #
        # 5) ^main[89] > @
        #
        # i.e., the > prompt comes out AFTER the prompt we we need to wait for.
    */
    // compile regexp once
    private final String promptPattern = "[a-zA-Z0-9_-][a-zA-Z0-9_-]*\\[[1-9][0-9]*\\] [ >]*$";
    private final Pattern promptRegexp = Pattern.compile(promptPattern);
    public List<String> waitForPrompt(int lines, boolean allowExit) {
        long startTime = System.currentTimeMillis();
        while (System.currentTimeMillis() - startTime < timeout) {
            try {
                Thread.sleep(sleepTime);
            } catch (InterruptedException e) {
                // ignore
            }
            synchronized (outputHandler) {
                if (!outputHandler.updated()) {
                    try {
                        outputHandler.wait(sleepTime);
                    } catch (InterruptedException e) {
                        // ignore
                    }
                } else {
                    // if something appeared in the jdb output, reset the timeout
                    startTime = System.currentTimeMillis();
                }
            }
            List<String> reply = outputHandler.get();
            for (String line: reply.subList(Math.max(0, reply.size() - lines), reply.size())) {
                if (promptRegexp.matcher(line).find()) {
                    logJdb(reply);
                    return outputHandler.reset();
                }
            }
            if (!jdb.isAlive()) {
                // ensure we get the whole output
                reply = outputHandler.reset();
                logJdb(reply);
                if (!allowExit) {
                    throw new RuntimeException("waitForPrompt timed out after " + (timeout/1000)
                            + " seconds, looking for '" + promptPattern + "', in " + lines + " lines");
                }
                return reply;
            }
        }
        // timeout
        logJdb(outputHandler.get());
        throw new RuntimeException("waitForPrompt timed out after " + (timeout/1000)
                + " seconds, looking for '" + promptPattern + "', in " + lines + " lines");
    }

    public List<String> command(JdbCommand cmd) {
        if (!jdb.isAlive()) {
            if (cmd.allowExit) {
                // return remaining output
                return outputHandler.reset();
            }
            throw new RuntimeException("Attempt to send command '" + cmd.cmd + "' to terminated jdb");
        }

        System.out.println("> " + cmd.cmd);

        inputWriter.println(cmd.cmd);

        if (inputWriter.checkError()) {
            throw new RuntimeException("Unexpected IO error while writing command '" + cmd.cmd + "' to jdb stdin stream");
        }

        return waitForPrompt(1, cmd.allowExit);
    }

    public List<String> command(String cmd) {
        return command(new JdbCommand(cmd));
    }

    // sends "cont" command up to maxTimes until debuggee exit
    public void contToExit(int maxTimes) {
        boolean exited = false;
        JdbCommand cont = JdbCommand.cont().allowExit();
        for (int i = 0; i < maxTimes && jdb.isAlive(); i++) {
            String reply = command(cont).stream().collect(Collectors.joining(lineSeparator));
            if (reply.contains(APPLICATION_EXIT)) {
                exited = true;
                break;
            }
        }
        if (!exited && jdb.isAlive()) {
            throw new RuntimeException("Debuggee did not exit after " + maxTimes + " <cont> commands");
        }
    }

    // quits jdb by using "quit" command
    public void quit() {
        command(JdbCommand.quit());
    }

    private void log(String s) {
        System.out.println(s);
    }

    private void logJdb(List<String> reply) {
        jdbOutput.addAll(reply);
        reply.forEach(s -> System.out.println("[jdb] " + s));
    }

    // returns the whole jdb output as a string
    public String getJdbOutput() {
        return jdbOutput.stream().collect(Collectors.joining(lineSeparator));
    }

    // returns the whole debuggee output as a string
    public String getDebuggeeOutput() {
        return debuggeeOutput.stream().collect(Collectors.joining(lineSeparator));
    }

    // handler for out/err of the pdb process
    private class OutputHandler extends OutputStream {
        // there are 2 buffers:
        // outStream - data from the process stdout/stderr after last get() call
        // cachedData - data collected at get(), cleared by reset()

        private final ByteArrayOutputStream outStream = new ByteArrayOutputStream();
        // if the last line in the reply had EOL, the list's last element is empty
        private final List<String> cachedData = new ArrayList<>();

        @Override
        public synchronized void write(int b) throws IOException {
            outStream.write((byte)(b & 0xFF));
            notifyAll();
        }
        @Override
        public synchronized void write(byte b[], int off, int len) throws IOException {
            outStream.write(b, off, len);
            notifyAll();
        }

        // gets output after the last {@ reset}.
        // returned data becomes invalid after {@reset}.
        public synchronized List<String> get() {
            if (updated()) {
                // we don't want to discard empty lines
                String[] newLines = outStream.toString().split("\\R", -1);
                if (!cachedData.isEmpty()) {
                    // concat the last line if previous data had no EOL
                    newLines[0] = cachedData.remove(cachedData.size()-1) + newLines[0];
                }
                cachedData.addAll(Arrays.asList(newLines));
                outStream.reset();
            }
            return Collections.unmodifiableList(cachedData);
        }

        // clears last replay (does not touch replyStream)
        // returns list as the last get()
        public synchronized List<String> reset() {
            List<String> result = new ArrayList<>(cachedData);
            cachedData.clear();
            return result;
        }

        // tests if there are some new data after the last lastReply() call
        public synchronized boolean updated() {
            return outStream.size() > 0;
        }
    }
}

