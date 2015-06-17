/*
 * Copyright (c) 2014, 2015, Oracle and/or its affiliates. All rights reserved.
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
package java.lang;

import java.security.PrivilegedAction;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

import sun.misc.InnocuousThread;

import static java.security.AccessController.doPrivileged;

/**
 * ProcessHandleImpl is the implementation of ProcessHandle.
 *
 * @see Process
 * @since 1.9
 */
final class ProcessHandleImpl implements ProcessHandle {

    /**
     * The thread pool of "process reaper" daemon threads.
     */
    private static final Executor processReaperExecutor =
            doPrivileged((PrivilegedAction<Executor>) () -> {

                ThreadGroup tg = Thread.currentThread().getThreadGroup();
                while (tg.getParent() != null) tg = tg.getParent();
                ThreadGroup systemThreadGroup = tg;

                ThreadFactory threadFactory = grimReaper -> {
                    // Our thread stack requirement is quite modest.
                    Thread t = new Thread(systemThreadGroup, grimReaper,
                            "process reaper", 32768);
                    t.setDaemon(true);
                    // A small attempt (probably futile) to avoid priority inversion
                    t.setPriority(Thread.MAX_PRIORITY);
                    return t;
                };

                return Executors.newCachedThreadPool(threadFactory);
            });

    private static class ExitCompletion extends CompletableFuture<Integer> {
        final boolean isReaping;

        ExitCompletion(boolean isReaping) {
            this.isReaping = isReaping;
        }
    }

    private static final ConcurrentMap<Long, ExitCompletion>
        completions = new ConcurrentHashMap<>();

    /**
     * Returns a CompletableFuture that completes with process exit status when
     * the process completes.
     *
     * @param shouldReap true if the exit value should be reaped
     */
    static CompletableFuture<Integer> completion(long pid, boolean shouldReap) {
        // check canonicalizing cache 1st
        ExitCompletion completion = completions.get(pid);
        // re-try until we get a completion that shouldReap => isReaping
        while (completion == null || (shouldReap && !completion.isReaping)) {
            ExitCompletion newCompletion = new ExitCompletion(shouldReap);
            if (completion == null) {
                completion = completions.putIfAbsent(pid, newCompletion);
            } else {
                completion = completions.replace(pid, completion, newCompletion)
                    ? null : completions.get(pid);
            }
            if (completion == null) {
                // newCompletion has just been installed successfully
                completion = newCompletion;
                // spawn a thread to wait for and deliver the exit value
                processReaperExecutor.execute(() -> {
                    int exitValue = waitForProcessExit0(pid, shouldReap);
                    newCompletion.complete(exitValue);
                    // remove from cache afterwards
                    completions.remove(pid, newCompletion);
                });
            }
        }
        return completion;
    }

    @Override
    public CompletableFuture<ProcessHandle> onExit() {
        if (this.equals(current)) {
            throw new IllegalStateException("onExit for current process not allowed");
        }

        return ProcessHandleImpl.completion(getPid(), false)
                .handleAsync((exitStatus, unusedThrowable) -> this);
    }

    /**
     * Wait for the process to exit, return the value.
     * Conditionally reap the value if requested
     * @param pid the processId
     * @param reapvalue if true, the value is retrieved,
     *                   else return the value and leave the process waitable
     *
     * @return the value or -1 if an error occurs
     */
    private static native int waitForProcessExit0(long pid, boolean reapvalue);

    /**
     * Cache the ProcessHandle of this process.
     */
    private static final ProcessHandleImpl current =
            new ProcessHandleImpl(getCurrentPid0());

    /**
     * The pid of this ProcessHandle.
     */
    private final long pid;

    /**
     * Private constructor.  Instances are created by the {@code get(long)} factory.
     * @param pid the pid for this instance
     */
    private ProcessHandleImpl(long pid) {
        this.pid = pid;
    }

    /**
     * Returns a ProcessHandle for an existing native process.
     *
     * @param pid the native process identifier
     * @return The ProcessHandle for the pid if the process is alive;
     *      or {@code null} if the process ID does not exist in the native system.
     * @throws SecurityException if RuntimePermission("manageProcess") is not granted
     */
    static Optional<ProcessHandle> get(long pid) {
        SecurityManager sm = System.getSecurityManager();
        if (sm != null) {
            sm.checkPermission(new RuntimePermission("manageProcess"));
        }
        return Optional.ofNullable(isAlive0(pid) ? new ProcessHandleImpl(pid) : null);
    }

    /**
     * Returns a ProcessHandle corresponding known to exist pid.
     * Called from ProcessImpl, it does not perform a security check or check if the process is alive.
     * @param pid of the known to exist process
     * @return a ProcessHandle corresponding to an existing Process instance
     */
    static ProcessHandle getUnchecked(long pid) {
        return new ProcessHandleImpl(pid);
    }

    /**
     * Returns the native process ID.
     * A {@code long} is used to be able to fit the system specific binary values
     * for the process.
     *
     * @return the native process ID
     */
    @Override
    public long getPid() {
        return pid;
    }

    /**
     * Returns the ProcessHandle for the current native process.
     *
     * @return The ProcessHandle for the OS process.
     * @throws SecurityException if RuntimePermission("manageProcess") is not granted
     */
    public static ProcessHandleImpl current() {
        SecurityManager sm = System.getSecurityManager();
        if (sm != null) {
            sm.checkPermission(new RuntimePermission("manageProcess"));
        }
        return current;
    }

    /**
     * Return the pid of the current process.
     *
     * @return the pid of the  current process
     */
    private static native long getCurrentPid0();

    /**
     * Returns a ProcessHandle for the parent process.
     *
     * @return a ProcessHandle of the parent process; {@code null} is returned
     *         if the child process does not have a parent
     * @throws SecurityException           if permission is not granted by the
     *                                     security policy
     */
    static Optional<ProcessHandle> parent(long pid) {
        SecurityManager sm = System.getSecurityManager();
        if (sm != null) {
            sm.checkPermission(new RuntimePermission("manageProcess"));
        }
        long ppid = parent0(pid);
        if (ppid <= 0) {
            return Optional.empty();
        }
        return get(ppid);
    }

    /**
     * Returns the parent of the native pid argument.
     *
     * @return the parent of the native pid; if any, otherwise -1
     */
    private static native long parent0(long pid);

    /**
     * Returns the number of pids filled in to the array.
     * @param pid if {@code pid} equals zero, then all known processes are returned;
     *      otherwise only direct child process pids are returned
     * @param pids an allocated long array to receive the pids
     * @param ppids an allocated long array to receive the parent pids; may be null
     * @return if greater than or equals to zero is the number of pids in the array;
     *      if greater than the length of the arrays, the arrays are too small
     */
    private static native int getProcessPids0(long pid, long[] pids, long[] ppids);

    /**
     * Destroy the process for this ProcessHandle.
     * @param pid the processs ID to destroy
     * @param force {@code true} if the process should be terminated forcibly;
     *     else {@code false} for a normal termination
     */
    static void destroyProcess(long pid, boolean force) {
        destroy0(pid, force);
    }

    private static native boolean destroy0(long pid, boolean forcibly);

    @Override
    public boolean destroy() {
        if (this.equals(current)) {
            throw new IllegalStateException("destroy of current process not allowed");
        }
        return destroy0(getPid(), false);
    }

    @Override
    public boolean destroyForcibly() {
        if (this.equals(current)) {
            throw new IllegalStateException("destroy of current process not allowed");
        }
        return destroy0(getPid(), true);
    }


    @Override
    public boolean supportsNormalTermination() {
        return ProcessImpl.SUPPORTS_NORMAL_TERMINATION;
    }

    /**
     * Tests whether the process represented by this {@code ProcessHandle} is alive.
     *
     * @return {@code true} if the process represented by this
     * {@code ProcessHandle} object has not yet terminated.
     * @since 1.9
     */
    @Override
    public boolean isAlive() {
        return isAlive0(pid);
    }

    /**
     * Returns true or false depending on whether the pid is alive.
     * This must not reap the exitValue like the isAlive method above.
     *
     * @param pid the pid to check
     * @return true or false
     */
    private static native boolean isAlive0(long pid);

    @Override
    public Optional<ProcessHandle> parent() {
        return parent(pid);
    }

    @Override
    public Stream<ProcessHandle> children() {
        return children(pid);
    }

    /**
     * Returns a Stream of the children of a process or all processes.
     *
     * @param pid the pid of the process for which to find the children;
     *            0 for all processes
     * @return a stream of ProcessHandles
     */
    static Stream<ProcessHandle> children(long pid) {
        SecurityManager sm = System.getSecurityManager();
        if (sm != null) {
            sm.checkPermission(new RuntimePermission("manageProcess"));
        }
        int size = 100;
        long[] childpids = null;
        while (childpids == null || size > childpids.length) {
            childpids = new long[size];
            size = getProcessPids0(pid, childpids, null);
        }
        return Arrays.stream(childpids, 0, size).mapToObj((id) -> new ProcessHandleImpl(id));
    }

    @Override
    public Stream<ProcessHandle> allChildren() {
        SecurityManager sm = System.getSecurityManager();
        if (sm != null) {
            sm.checkPermission(new RuntimePermission("manageProcess"));
        }
        int size = 100;
        long[] pids = null;
        long[] ppids = null;
        while (pids == null || size > pids.length) {
            pids = new long[size];
            ppids = new long[size];
            size = getProcessPids0(0, pids, ppids);
        }

        int next = 0;       // index of next process to check
        int count = -1;     // count of subprocesses scanned
        long ppid = pid;    // start looking for this parent
        do {
            // Scan from next to size looking for ppid
            // if found, exchange it to index next
            for (int i = next; i < size; i++) {
                if (ppids[i] == ppid) {
                    swap(pids, i, next);
                    swap(ppids, i, next);
                    next++;
                }
            }
            ppid = pids[++count];   // pick up the next pid to scan for
        } while (count < next);

        return Arrays.stream(pids, 0, count).mapToObj((id) -> new ProcessHandleImpl(id));
    }

    // Swap two elements in an array
    private static void swap(long[] array, int x, int y) {
        long v = array[x];
        array[x] = array[y];
        array[y] = v;
    }

    @Override
    public ProcessHandle.Info info() {
        return ProcessHandleImpl.Info.info(pid);
    }

    @Override
    public int compareTo(ProcessHandle other) {
        return Long.compare(pid, ((ProcessHandleImpl) other).pid);
    }

    @Override
    public String toString() {
        return Long.toString(pid);
    }

    @Override
    public int hashCode() {
        return Long.hashCode(pid);
    }

    @Override
    public boolean equals(Object obj) {
        return (obj instanceof ProcessHandleImpl) &&
            (pid == ((ProcessHandleImpl) obj).pid);
    }

    /**
     * Implementation of ProcessHandle.Info.
     * Information snapshot about a process.
     * The attributes of a process vary by operating system and not available
     * in all implementations.  Additionally, information about other processes
     * is limited by the operating system privileges of the process making the request.
     * If a value is not available, either a {@code null} or {@code -1} is stored.
     * The accessor methods return {@code null} if the value is not available.
     */
    static class Info implements ProcessHandle.Info {
        static {
            initIDs();
        }

        /**
         * Initialization of JNI fieldIDs.
         */
        private static native void initIDs();

        /**
         * Fill in this Info instance with information about the native process.
         * If values are not available the native code does not modify the field.
         * @param pid  of the native process
         */
        private native void info0(long pid);

        String command;
        String[] arguments;
        long startTime;
        long totalTime;
        String user;

        Info() {
            command = null;
            arguments = null;
            startTime = -1L;
            totalTime = -1L;
            user = null;
        }

        /**
         * Returns the Info object with the fields from the process.
         * Whatever fields are provided by native are returned.
         *
         * @param pid the native process identifier
         * @return ProcessHandle.Info non-null; individual fields may be null
         *          or -1 if not available.
         */
        public static ProcessHandle.Info info(long pid) {
            Info info = new Info();
            info.info0(pid);
            return info;
        }

        @Override
        public Optional<String> command() {
            return Optional.ofNullable(command);
        }

        @Override
        public Optional<String[]> arguments() {
            return Optional.ofNullable(arguments);
        }

        @Override
        public Optional<Instant> startInstant() {
            return (startTime > 0)
                    ? Optional.of(Instant.ofEpochMilli(startTime))
                    : Optional.empty();
        }

        @Override
        public Optional<Duration> totalCpuDuration() {
            return (totalTime != -1)
                    ? Optional.of(Duration.ofNanos(totalTime))
                    : Optional.empty();
        }

        @Override
        public Optional<String> user() {
            return Optional.ofNullable(user);
        }

        @Override
        public String toString() {
            StringBuilder sb = new StringBuilder(60);
            sb.append('[');
            if (user != null) {
                sb.append("user: ");
                sb.append(user());
            }
            if (command != null) {
                if (sb.length() != 0) sb.append(", ");
                sb.append("cmd: ");
                sb.append(command);
            }
            if (arguments != null && arguments.length > 0) {
                if (sb.length() != 0) sb.append(", ");
                sb.append("args: ");
                sb.append(Arrays.toString(arguments));
            }
            if (startTime != -1) {
                if (sb.length() != 0) sb.append(", ");
                sb.append("startTime: ");
                sb.append(startInstant());
            }
            if (totalTime != -1) {
                if (sb.length() != 0) sb.append(", ");
                sb.append("totalTime: ");
                sb.append(totalCpuDuration().toString());
            }
            sb.append(']');
            return sb.toString();
        }
    }
}
