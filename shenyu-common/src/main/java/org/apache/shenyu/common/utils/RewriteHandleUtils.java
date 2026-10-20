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

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

/**
 * The utils that tell whether a rewrite expression can be applied.
 */
public final class RewriteHandleUtils {

    private static final String INVALID_REGEX = "invalid regular expression: ";

    private static final String INVALID_REPLACEMENT = "invalid replacement template: ";

    private RewriteHandleUtils() {
    }

    /**
     * Explain why a rewrite expression cannot be applied.
     *
     * <p>The plugin expands the replacement with {@link String#replaceAll(String, String)}, so the
     * replacement is a template in which a dollar sign references a regex group. The configured
     * expression is validated on its own first: appending the empty alternative that makes the
     * dry-run match would turn a trailing backslash into an escaped pipe and hide an invalid
     * expression. The dry-run that follows only validates the replacement template. An incomplete
     * expression and the placeholder mode, where both values contain a left brace, are always
     * applicable because the plugin skips them or substitutes them literally.</p>
     *
     * @param regex the configured regular expression
     * @param replace the configured replacement template
     * @return {@code null} when the plugin can apply the expression, otherwise the reason why it cannot
     */
    public static String applicabilityProblem(final String regex, final String replace) {
        if (isSkipped(regex, replace)) {
            return null;
        }
        try {
            Pattern.compile(regex);
        } catch (PatternSyntaxException ex) {
            return INVALID_REGEX + ex.getDescription();
        }
        try {
            // the trailing empty alternative makes the empty input match, so the replacement is parsed too
            Pattern.compile(regex + "|").matcher("").replaceAll(replace);
            return null;
        } catch (IllegalArgumentException | IndexOutOfBoundsException ex) {
            return INVALID_REPLACEMENT + ex.getMessage();
        }
    }

    private static boolean isSkipped(final String regex, final String replace) {
        return StringUtils.isAnyBlank(regex, replace) || replace.contains("{") && regex.contains("{");
    }
}
