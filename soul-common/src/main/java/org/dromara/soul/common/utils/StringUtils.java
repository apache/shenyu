/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.dromara.soul.common.utils;

/**
 * StringUtils .
 * 1. String相关的判断操作.
 *
 * @author sixh
 */
public class StringUtils {
    private static final String FOLDER_SEPARATOR = "/";

    private static final String WINDOWS_FOLDER_SEPARATOR = "\\";

    private static final String TOP_PATH = "..";

    private static final String CURRENT_PATH = ".";

    private static final char EXTENSION_SEPARATOR = '.';

    /**
     * 判断字符串是否为空.
     *
     * @param arg arg.
     * @return bool .
     */
    public static boolean isBlank(final String arg) {
        return arg == null || arg.length() == 0;
    }

    /**
     * 判断字符串是不为空.
     *
     * @param arg arg.
     * @return bool .
     */
    public static boolean isNotBlank(final String arg) {
        return !isBlank(arg);
    }

    /**
     * Has length boolean.
     *
     * @param str the str
     * @return the boolean
     */
    public static boolean hasLength(CharSequence str) {
        return (str != null && str.length() > 0);
    }
}
