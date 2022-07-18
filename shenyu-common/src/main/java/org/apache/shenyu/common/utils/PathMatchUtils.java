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

import org.springframework.http.server.PathContainer;
import org.springframework.web.util.pattern.PathPattern;
import org.springframework.web.util.pattern.PathPatternParser;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The type Path match utils.
 */
public class PathMatchUtils {

    /**
     * replace url {id} to real param.
     *
     * @param path        the total path
     * @param regex       the regex content
     * @param replacement the replacement content
     * @return the string
     */
    public static String replaceAll(final String path, final String regex, final String replacement) {
        return path.replaceAll(Pattern.quote(regex), Matcher.quoteReplacement(replacement));
    }

    /**
     * Match boolean.
     *
     * @param matchUrls the path patter
     * @param path      the real path
     * @return the boolean
     */
    public static boolean match(final String matchUrls, final String path) {
        PathPattern pattern = PathPatternParser.defaultInstance.parse(matchUrls);
        return pattern.matches(PathContainer.parsePath(path));
    }
}
