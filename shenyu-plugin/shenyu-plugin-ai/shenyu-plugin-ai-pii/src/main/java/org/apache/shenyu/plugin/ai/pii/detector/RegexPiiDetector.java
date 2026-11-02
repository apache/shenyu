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

package org.apache.shenyu.plugin.ai.pii.detector;

import org.apache.shenyu.plugin.ai.pii.PiiMatch;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * A detector built from a regular expression, with an optional checksum
 * validation of every candidate match. The compiled pattern is shared
 * between concurrent requests: {@link Pattern} is thread safe and the
 * matcher is created per call.
 */
public final class RegexPiiDetector implements PiiDetector {

    private final String type;

    private final Pattern pattern;

    private final Predicate<String> validator;

    /**
     * A detector that reports every match of the pattern.
     *
     * @param type the detector type
     * @param pattern the compiled pattern
     */
    public RegexPiiDetector(final String type, final Pattern pattern) {
        this(type, pattern, null);
    }

    /**
     * A detector that reports a match of the pattern only when the validator
     * accepts the matched value, which keeps the false positive rate of a
     * broad digit pattern acceptable.
     *
     * @param type the detector type
     * @param pattern the compiled pattern
     * @param validator the checksum validation of a matched value, nullable
     */
    public RegexPiiDetector(final String type, final Pattern pattern, final Predicate<String> validator) {
        this.type = Objects.requireNonNull(type);
        this.pattern = Objects.requireNonNull(pattern);
        this.validator = validator;
    }

    @Override
    public String type() {
        return type;
    }

    @Override
    public List<PiiMatch> detect(final CharSequence text, final long deadlineNanos) {
        CharSequence bounded = DeadlineCharSequence.of(type, text, deadlineNanos);
        Matcher matcher = pattern.matcher(bounded);
        List<PiiMatch> matches = new ArrayList<>();
        while (matcher.find()) {
            if (!validated(matcher)) {
                continue;
            }
            matches.add(new PiiMatch(type, matcher.start(), matcher.end()));
        }
        return matches;
    }

    private boolean validated(final Matcher matcher) {
        return Objects.isNull(validator) || validator.test(matcher.group());
    }
}
