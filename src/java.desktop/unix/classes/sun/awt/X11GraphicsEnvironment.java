/*
 * Copyright (c) 1997, 2019, Oracle and/or its affiliates. All rights reserved.
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

package sun.awt;

import java.awt.AWTError;
import java.awt.GraphicsDevice;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Enumeration;

import sun.java2d.SunGraphicsEnvironment;
import sun.java2d.SurfaceManagerFactory;
import sun.java2d.UnixSurfaceManagerFactory;
import sun.java2d.xr.XRSurfaceData;
import sun.util.logging.PlatformLogger;

/**
 * This is an implementation of a GraphicsEnvironment object for the
 * default local GraphicsEnvironment used by the Java Runtime Environment
 * for X11 environments.
 *
 * @see GraphicsDevice
 * @see java.awt.GraphicsConfiguration
 */
public final class X11GraphicsEnvironment extends SunGraphicsEnvironment {

    private static final PlatformLogger log = PlatformLogger.getLogger("sun.awt.X11GraphicsEnvironment");
    private static final PlatformLogger screenLog = PlatformLogger.getLogger("sun.awt.screen.X11GraphicsEnvironment");

    private static Boolean xinerState;

    static {
        java.security.AccessController.doPrivileged(
                          new java.security.PrivilegedAction<Object>() {
            public Object run() {
                System.loadLibrary("awt");

                /*
                 * Note: The MToolkit object depends on the static initializer
                 * of X11GraphicsEnvironment to initialize the connection to
                 * the X11 server.
                 */
                if (!isHeadless()) {
                    // first check the OGL system property
                    boolean glxRequested = false;
                    String prop = System.getProperty("sun.java2d.opengl");
                    if (prop != null) {
                        if (prop.equals("true") || prop.equals("t")) {
                            glxRequested = true;
                        } else if (prop.equals("True") || prop.equals("T")) {
                            glxRequested = true;
                            glxVerbose = true;
                        }
                    }

                    // Now check for XRender system property
                    boolean xRenderRequested = true;
                    boolean xRenderIgnoreLinuxVersion = false;
                    String xProp = System.getProperty("sun.java2d.xrender");
                        if (xProp != null) {
                        if (xProp.equals("false") || xProp.equals("f")) {
                            xRenderRequested = false;
                        } else if (xProp.equals("True") || xProp.equals("T")) {
                            xRenderRequested = true;
                            xRenderVerbose = true;
                        }

                        if(xProp.equalsIgnoreCase("t") || xProp.equalsIgnoreCase("true")) {
                            xRenderIgnoreLinuxVersion = true;
                        }
                    }

                    // initialize the X11 display connection
                    initDisplay(glxRequested);

                    // only attempt to initialize GLX if it was requested
                    if (glxRequested) {
                        glxAvailable = initGLX();
                        if (glxVerbose && !glxAvailable) {
                            System.out.println(
                                "Could not enable OpenGL " +
                                "pipeline (GLX 1.3 not available)");
                        }
                    }

                    // only attempt to initialize Xrender if it was requested
                    if (xRenderRequested) {
                        xRenderAvailable = initXRender(xRenderVerbose, xRenderIgnoreLinuxVersion);
                        if (xRenderVerbose && !xRenderAvailable) {
                            System.out.println(
                                         "Could not enable XRender pipeline");
                        }
                    }

                    if (xRenderAvailable) {
                        XRSurfaceData.initXRSurfaceData();
                    }
                }

                return null;
            }
         });

        // Install the correct surface manager factory.
        SurfaceManagerFactory.setInstance(new UnixSurfaceManagerFactory());

    }


    private static boolean glxAvailable;
    private static boolean glxVerbose;

    private static native boolean initGLX();

    public static boolean isGLXAvailable() {
        return glxAvailable;
    }

    public static boolean isGLXVerbose() {
        return glxVerbose;
    }

    private static boolean xRenderVerbose;
    private static boolean xRenderAvailable;

    private static native boolean initXRender(boolean verbose, boolean ignoreLinuxVersion);
    public static boolean isXRenderAvailable() {
        return xRenderAvailable;
    }

    public static boolean isXRenderVerbose() {
        return xRenderVerbose;
    }

    /**
     * Checks if Shared Memory extension can be used.
     * Returns:
     *   -1 if server doesn't support MITShm
     *    1 if server supports it and it can be used
     *    0 otherwise
     */
    private static native int checkShmExt();

    private static  native String getDisplayString();
    private Boolean isDisplayLocal;

    /**
     * This should only be called from the static initializer, so no need for
     * the synchronized keyword.
     */
    private static native void initDisplay(boolean glxRequested);

    public X11GraphicsEnvironment() {
    }

    protected native int getNumScreens();

    protected GraphicsDevice makeScreenDevice(int screennum) {
        return new X11GraphicsDevice(screennum);
    }

    private native int getDefaultScreenNum();
    /**
     * Returns the default screen graphics device.
     */
    public GraphicsDevice getDefaultScreenDevice() {
        GraphicsDevice[] screens = getScreenDevices();
        if (screens.length == 0) {
            throw new AWTError("no screen devices");
        }
        int index = getDefaultScreenNum();
        return screens[0 < index && index < screens.length ? index : 0];
    }

    public boolean isDisplayLocal() {
        if (isDisplayLocal == null) {
            SunToolkit.awtLock();
            try {
                if (isDisplayLocal == null) {
                    isDisplayLocal = Boolean.valueOf(_isDisplayLocal());
                }
            } finally {
                SunToolkit.awtUnlock();
            }
        }
        return isDisplayLocal.booleanValue();
    }

    private static boolean _isDisplayLocal() {
        if (isHeadless()) {
            return true;
        }

        String isRemote = java.security.AccessController.doPrivileged(
            new sun.security.action.GetPropertyAction("sun.java2d.remote"));
        if (isRemote != null) {
            return isRemote.equals("false");
        }

        int shm = checkShmExt();
        if (shm != -1) {
            return (shm == 1);
        }

        // If XServer doesn't support ShMem extension,
        // try the other way

        String display = getDisplayString();
        int ind = display.indexOf(':');
        final String hostName = display.substring(0, ind);
        if (ind <= 0) {
            // ':0' case
            return true;
        }

        Boolean result = java.security.AccessController.doPrivileged(
            new java.security.PrivilegedAction<Boolean>() {
            public Boolean run() {
                InetAddress[] remAddr = null;
                Enumeration<InetAddress> locals = null;
                Enumeration<NetworkInterface> interfaces = null;
                try {
                    interfaces = NetworkInterface.getNetworkInterfaces();
                    remAddr = InetAddress.getAllByName(hostName);
                    if (remAddr == null) {
                        return Boolean.FALSE;
                    }
                } catch (UnknownHostException e) {
                    System.err.println("Unknown host: " + hostName);
                    return Boolean.FALSE;
                } catch (SocketException e1) {
                    System.err.println(e1.getMessage());
                    return Boolean.FALSE;
                }

                for (; interfaces.hasMoreElements();) {
                    locals = interfaces.nextElement().getInetAddresses();
                    for (; locals.hasMoreElements();) {
                        final InetAddress localAddr = locals.nextElement();
                        for (int i = 0; i < remAddr.length; i++) {
                            if (localAddr.equals(remAddr[i])) {
                                return Boolean.TRUE;
                            }
                        }
                    }
                }
                return Boolean.FALSE;
            }});
        return result.booleanValue();
    }



    /**
     * Returns face name for default font, or null if
     * no face names are used for CompositeFontDescriptors
     * for this platform.
     */
    public String getDefaultFontFaceName() {

        return null;
    }

    private static native boolean pRunningXinerama();

    public boolean runningXinerama() {
        if (xinerState == null) {
            // pRunningXinerama() simply returns a global boolean variable,
            // so there is no need to synchronize here
            xinerState = Boolean.valueOf(pRunningXinerama());
            if (screenLog.isLoggable(PlatformLogger.Level.FINER)) {
                screenLog.finer("Running Xinerama: " + xinerState);
            }
        }
        return xinerState.booleanValue();
    }

    /**
     * From the DisplayChangedListener interface; devices do not need
     * to react to this event.
     */
    @Override
    public void paletteChanged() {
    }
}
