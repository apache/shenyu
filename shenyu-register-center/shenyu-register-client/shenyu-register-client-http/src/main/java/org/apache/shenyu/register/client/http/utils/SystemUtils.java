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

package org.apache.shenyu.register.client.http.utils;

import java.lang.management.ManagementFactory;
import java.util.Optional;

/**
 * SystemUtils.
 */
public final class SystemUtils {

    private static final String SYSTEM_PROP_OS_NAME = "os.name";

    private static final String WIN_OS_NAME_PREFIX = "Windows";

    private SystemUtils() {
    }

    /**
     * Check whether the port is listening to by other process.
     *
     * @return String current PID
     */
    public static String getCurrentPID() {
        return ManagementFactory.getRuntimeMXBean().getName().split("@")[0];
    }

    /**
     * Check if the operating system is windows.
     *
     * @return boolean is windows os
     */
    public static boolean isWindows() {
        return getOsName().map(n -> n.startsWith(WIN_OS_NAME_PREFIX)).orElse(false);
    }

    /**
     * get the operating system name.
     *
     * @return Optional operating system name
     */
    public static Optional<String> getOsName() {
        try {
            return Optional.of(System.getProperty(SYSTEM_PROP_OS_NAME));
        } catch (SecurityException e) {
            return Optional.empty();
        }
    }
}
