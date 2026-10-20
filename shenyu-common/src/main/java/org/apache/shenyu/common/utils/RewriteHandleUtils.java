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
 * The utils that tell whether a rewrite expression can be applied by the rewrite plugin.
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
     * replacement is a template in which a dollar sign references a regex group. The pair is therefore
     * dry-run against the empty input to let the JDK parser validate both parts. An incomplete
     * expression and the placeholder mode, where both values contain a left brace, are always
     * applicable because the plugin skips them or substitutes them literally.</p>
     *
     * @param regex the configured regular expression
     * @param replace the configured replacement template
     * @return {@code null} when the plugin can apply the expression, otherwise the reason why it cannot
     */
    public static String unapplicableReason(final String regex, final String replace) {
        if (StringUtils.isAnyBlank(regex, replace)) {
            return null;
        }
        if (replace.contains("{") && regex.contains("{")) {
            return null;
        }
        try {
            // the trailing empty alternative makes the empty input match, so the replacement is parsed too
            // the configured value is only dry-run against an empty input and never rewrites a request
            // CHECKSTYLE:OFF
            Pattern.compile(regex + "|").matcher("").replaceAll(replace); // lgtm[java/regex-injection]
            // CHECKSTYLE:ON
            return null;
        } catch (PatternSyntaxException ex) {
            return INVALID_REGEX + ex.getDescription();
        } catch (IllegalArgumentException | IndexOutOfBoundsException ex) {
            return INVALID_REPLACEMENT + ex.getMessage();
        }
    }
}
