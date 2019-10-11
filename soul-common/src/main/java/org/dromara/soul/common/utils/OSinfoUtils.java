/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.common.utils;

/**
 * The type O sinfo utils.
 *
 * @author: xiaoyu
 */
public final class OSinfoUtils {

    private static final String OS = System.getProperty("os.name").toLowerCase();

    private static OSinfoUtils instance = new OSinfoUtils();


    private OSinfoUtils() {
    }

    /**
     * Is linux boolean.
     *
     * @return the boolean
     */
    public static boolean isLinux() {
        return OS.contains("linux");
    }

    /**
     * Is mac os boolean.
     *
     * @return the boolean
     */
    public static boolean isMacOS() {
        return OS.contains("mac") && OS.indexOf("os") > 0 && !OS.contains("x");
    }

    /**
     * Is mac osx boolean.
     *
     * @return the boolean
     */
    public static boolean isMacOSX() {
        return OS.contains("mac") && OS.indexOf("os") > 0 && OS.indexOf("x") > 0;
    }

    /**
     * Is windows boolean.
     *
     * @return the boolean
     */
    public static boolean isWindows() {
        return OS.contains("windows");
    }

    /**
     * Is os 2 boolean.
     *
     * @return the boolean
     */
    public static boolean isOS2() {
        return OS.contains("os/2");
    }

    /**
     * Is solaris boolean.
     *
     * @return the boolean
     */
    public static boolean isSolaris() {
        return OS.contains("solaris");
    }

    /**
     * Is sun os boolean.
     *
     * @return the boolean
     */
    public static boolean isSunOS() {
        return OS.contains("sunos");
    }

    /**
     * Is mp ei x boolean.
     *
     * @return the boolean
     */
    public static boolean isMPEiX() {
        return OS.contains("mpe/ix");
    }

    /**
     * Is hpux boolean.
     *
     * @return the boolean
     */
    public static boolean isHPUX() {
        return OS.contains("hp-ux");
    }

    /**
     * Is aix boolean.
     *
     * @return the boolean
     */
    public static boolean isAix() {
        return OS.contains("aix");
    }

    /**
     * Is os 390 boolean.
     *
     * @return the boolean
     */
    public static boolean isOS390() {
        return OS.contains("os/390");
    }

    /**
     * Is free bsd boolean.
     *
     * @return the boolean
     */
    public static boolean isFreeBSD() {
        return OS.contains("freebsd");
    }

    /**
     * Is irix boolean.
     *
     * @return the boolean
     */
    public static boolean isIrix() {
        return OS.contains("irix");
    }

    /**
     * Is digital unix boolean.
     *
     * @return the boolean
     */
    public static boolean isDigitalUnix() {
        return OS.contains("digital") && OS.indexOf("unix") > 0;
    }

    /**
     * Is net ware boolean.
     *
     * @return the boolean
     */
    public static boolean isNetWare() {
        return OS.contains("netware");
    }

    /**
     * Is osf 1 boolean.
     *
     * @return the boolean
     */
    public static boolean isOSF1() {
        return OS.contains("osf1");
    }

    /**
     * Is open vms boolean.
     *
     * @return the boolean
     */
    public static boolean isOpenVMS() {
        return OS.contains("openvms");
    }
}  