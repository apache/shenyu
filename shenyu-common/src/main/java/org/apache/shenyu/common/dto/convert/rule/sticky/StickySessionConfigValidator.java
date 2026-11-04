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

package org.apache.shenyu.common.dto.convert.rule.sticky;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Management-plane validation for {@link StickySessionConfig}.
 *
 * <p>The gateway keeps a tolerant runtime posture: a malformed sticky configuration
 * degrades to plain load balancing through the runtime activation checks, so an already
 * persisted bad rule keeps working after an upgrade. This validator exists for the write
 * path instead: the admin rejects new rules whose sticky section is syntactically valid
 * JSON but semantically unusable (an unknown failure policy, a non-positive lifetime,
 * blank secrets), so operators get an error at save time rather than a silently
 * disabled feature at runtime.</p>
 *
 * <p>A disabled configuration is not validated: the runtime activation checks ignore it
 * entirely, so its values cannot affect a request and must not block the write of an
 * unrelated rule change.</p>
 */
public final class StickySessionConfigValidator {

    /**
     * Failure policies accepted for {@code sticky.onUnavailable}. Must stay in sync with
     * the runtime comparison in {@code StickySessionAffinity} (POLICY_STRICT), which is
     * case-sensitive.
     */
    private static final Set<String> FAILURE_POLICIES = Set.of("REBIND", "STRICT");

    private StickySessionConfigValidator() {
    }

    /**
     * Validate the sticky section of a divide rule handle.
     *
     * <p>Only an enabled configuration is validated: a disabled one is inert at runtime
     * (the gateway activation checks ignore it), so it must not stop an operator from
     * editing an unrelated field of an already stored rule.
     *
     * @param config the sticky configuration, may be null (nothing to validate)
     * @throws IllegalArgumentException when a value cannot work at runtime
     */
    public static void validate(final StickySessionConfig config) {
        if (Objects.isNull(config) || !config.isEnabled()) {
            return;
        }
        validatePolicy(config.getOnUnavailable());
        validateLifetime(config.getMaxAgeSeconds());
        validateCookieName(config.getCookieName());
        validateSecrets(config.getSecrets());
        validateKey(config.getKey());
    }

    private static void validatePolicy(final String onUnavailable) {
        if (StringUtils.isBlank(onUnavailable)) {
            return;
        }
        // the runtime compares exactly ("STRICT"), so any other casing would silently
        // behave as REBIND; reject it here instead
        if (!FAILURE_POLICIES.contains(onUnavailable)) {
            throw new IllegalArgumentException("sticky.onUnavailable must be REBIND or STRICT, got: " + onUnavailable);
        }
    }

    private static void validateLifetime(final int maxAgeSeconds) {
        if (maxAgeSeconds <= 0) {
            throw new IllegalArgumentException("sticky.maxAgeSeconds must be a positive number, got: " + maxAgeSeconds);
        }
    }

    private static void validateCookieName(final String cookieName) {
        if (Objects.nonNull(cookieName) && StringUtils.isBlank(cookieName)) {
            throw new IllegalArgumentException("sticky.cookieName must not be blank; omit it to use the default");
        }
    }

    private static void validateSecrets(final List<String> secrets) {
        if (CollectionUtils.isEmpty(secrets)) {
            throw new IllegalArgumentException("sticky.secrets must contain at least one key when sticky is enabled");
        }
        boolean blank = secrets.stream().anyMatch(StringUtils::isBlank);
        if (blank) {
            throw new IllegalArgumentException("sticky.secrets must not contain blank keys");
        }
    }

    private static void validateKey(final DeterministicKeyConfig key) {
        if (Objects.isNull(key)) {
            return;
        }
        if (StringUtils.isBlank(key.getName())) {
            throw new IllegalArgumentException("sticky.key.name must not be blank when sticky.key is configured");
        }
    }
}
