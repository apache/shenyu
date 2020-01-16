/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.common.utils;

import org.springframework.util.AntPathMatcher;

/**
 * The type Path match utils.
 *
 * @author xiaoyu
 */
public class PathMatchUtils {

    private static final AntPathMatcher MATCHER = new AntPathMatcher();

    /**
     * Match boolean.
     *
     * @param matchUrls the ignore urls
     * @param path      the path
     * @return the boolean
     */
    public static boolean match(final String matchUrls, final String path) {
        String[] urlList = matchUrls.split(",");
        for (String pattern : urlList) {
            boolean match = reg(pattern, path);
            if (match) {
                return true;
            }
        }
        return false;
    }

    private static boolean reg(final String pattern, final String path) {
        return MATCHER.match(pattern, path);
    }

}
