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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.constant.Constants;
import org.checkerframework.checker.nullness.qual.NonNull;

/**
 * The type Path utils.
 */
public final class PathUtils {
    
    /**
     * Decorator path string.
     *
     * @param contextPath the context path
     * @return the string
     */
    public static String decoratorPath(final String contextPath) {
        return StringUtils.contains(contextPath, AdminConstants.URI_SUFFIX) ? contextPath : contextPath + AdminConstants.URI_SUFFIX;
    }
    
    /**
     * Decorator context path string.
     *
     * @param contextPath the context path
     * @return the string
     */
    public static String decoratorContextPath(final String contextPath) {
        return StringUtils.contains(contextPath, AdminConstants.URI_SUFFIX) ? StringUtils.substringBefore(contextPath, AdminConstants.URI_SUFFIX) : contextPath;
    }

    /**
     * Decorator path string with slash.
     *
     * @param contextPath the context path
     * @return the string
     */
    public static String decoratorPathWithSlash(final String contextPath) {
        return StringUtils.endsWith(contextPath, AdminConstants.URI_SLASH_SUFFIX) ? contextPath : contextPath + AdminConstants.URI_SLASH_SUFFIX;
    }

    /**
     * Joint paths.
     * @param path paths
     * @return path
     */
    public static String pathJoin(@NonNull final String... path) {

        StringBuilder result = new StringBuilder(Constants.PATH_SEPARATOR);

        for (String p : path) {
            if (!result.toString().endsWith(Constants.PATH_SEPARATOR)) {
                result.append(Constants.PATH_SEPARATOR);
            }
            result.append(p.startsWith(Constants.PATH_SEPARATOR) ? p.replaceFirst(Constants.PATH_SEPARATOR, "") : p);
        }
        return result.toString();
    }
}
