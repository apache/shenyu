/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.common.utils;

import org.apache.shenyu.common.enums.PlatformEnum;

import java.util.Locale;

/**
 * The type OS utils.
 */
public final class OSUtils {
    
    private static final String OPERATING_SYSTEM_NAME = System.getProperty("os.name").toLowerCase(Locale.ENGLISH);
    
    private static final String OPERATING_SYSTEM_ARCH = System.getProperty("os.arch").toLowerCase(Locale.ENGLISH);
    
    private static final String UNKNOWN = "unknown";
    
    private static final PlatformEnum PLATFORM;
    
    private static final String ARCH;
    
    static {
        if (OPERATING_SYSTEM_NAME.startsWith("linux")) {
            PLATFORM = PlatformEnum.LINUX;
        } else if (OPERATING_SYSTEM_NAME.startsWith("mac") || OPERATING_SYSTEM_NAME.startsWith("darwin")) {
            PLATFORM = PlatformEnum.MACOSX;
        } else if (OPERATING_SYSTEM_NAME.startsWith("windows")) {
            PLATFORM = PlatformEnum.WINDOWS;
        } else {
            PLATFORM = PlatformEnum.UNKNOWN;
        }
        
        ARCH = normalizeArch();
    }
    
    private OSUtils() {
    }
    
    private static String normalizeArch() {
        String value = OPERATING_SYSTEM_ARCH.toLowerCase(Locale.US).replaceAll("[^a-z0-9]+", "");
        if (value.matches("^(x8664|amd64|ia32e|em64t|x64)$")) {
            return "x86_64";
        }
        if (value.matches("^(x8632|x86|i[3-6]86|ia32|x32)$")) {
            return "x86_32";
        }
        if (value.matches("^(ia64w?|itanium64)$")) {
            return "itanium_64";
        }
        if ("ia64n".equals(value)) {
            return "itanium_32";
        }
        if (value.matches("^(sparc|sparc32)$")) {
            return "sparc_32";
        }
        if (value.matches("^(sparcv9|sparc64)$")) {
            return "sparc_64";
        }
        if (value.matches("^(arm|arm32)$")) {
            return "arm_32";
        }
        if ("aarch64".equals(value)) {
            return "aarch_64";
        }
        if (value.matches("^(mips|mips32)$")) {
            return "mips_32";
        }
        if (value.matches("^(mipsel|mips32el)$")) {
            return "mipsel_32";
        }
        if ("mips64".equals(value)) {
            return "mips_64";
        }
        if ("mips64el".equals(value)) {
            return "mipsel_64";
        }
        if (value.matches("^(ppc|ppc32)$")) {
            return "ppc_32";
        }
        if (value.matches("^(ppcle|ppc32le)$")) {
            return "ppcle_32";
        }
        if ("ppc64".equals(value)) {
            return "ppc_64";
        }
        if ("ppc64le".equals(value)) {
            return "ppcle_64";
        }
        if ("s390".equals(value)) {
            return "s390_32";
        }
        if ("s390x".equals(value)) {
            return "s390_64";
        }
        
        return UNKNOWN;
    }
    
    /**
     * Check whether it is a windows system.
     *
     * @return true if it is a windows system
     */
    public static boolean isWindows() {
        return PLATFORM == PlatformEnum.WINDOWS;
    }
    
    /**
     * Check whether it is a linux system.
     *
     * @return true if it is a linux system
     */
    public static boolean isLinux() {
        return PLATFORM == PlatformEnum.LINUX;
    }
    
    /**
     * Check whether it is a macos.
     *
     * @return true if it is a macos
     */
    public static boolean isMac() {
        return PLATFORM == PlatformEnum.MACOSX;
    }
    
    /**
     * Get the CPU architecture.
     *
     * @return the CPU architecture
     */
    public static String arch() {
        return ARCH;
    }
    
    /**
     * Detect whether it is x86 architecture.
     *
     * @return true if it is x86 architecture
     */
    public static boolean isX86() {
        return "x86_32".equals(ARCH);
    }
    
    /**
     * Detect whether it is x86_64 architecture.
     *
     * @return true if it is x86_64 architecture
     */
    public static boolean isX64() {
        return "x86_64".equals(ARCH);
    }
    
    /**
     * Detect whether it is arm32 architecture.
     *
     * @return true if it is arm32 architecture
     */
    public static boolean isArm32() {
        return "arm_32".equals(ARCH);
    }
    
    /**
     * Detect whether it is arm64 architecture.
     *
     * @return true if it is arm64 architecture
     */
    public static boolean isArm64() {
        return "aarch_64".equals(ARCH);
    }
}
