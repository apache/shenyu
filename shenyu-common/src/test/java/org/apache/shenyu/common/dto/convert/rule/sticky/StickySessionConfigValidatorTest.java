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

import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Tests for the management-plane validation of sticky session configuration: the admin
 * rejects semantically unusable values at save time while the gateway runtime keeps its
 * tolerant degradation posture.
 */
public final class StickySessionConfigValidatorTest {

    @Test
    public void nullConfigIsValid() {
        assertDoesNotThrow(() -> StickySessionConfigValidator.validate(null));
    }

    @Test
    public void disabledConfigIsNotValidated() {
        StickySessionConfig config = new StickySessionConfig();
        config.setEnabled(false);
        // a disabled configuration is inert at runtime, so none of these values may
        // block the write of an unrelated rule change
        config.setSecrets(Collections.emptyList());
        config.setMaxAgeSeconds(0);
        config.setOnUnavailable("strict");
        config.setCookieName(" ");
        config.setKey(new DeterministicKeyConfig());
        assertDoesNotThrow(() -> StickySessionConfigValidator.validate(config));
    }

    @Test
    public void validEnabledConfigPasses() {
        StickySessionConfig config = new StickySessionConfig();
        config.setEnabled(true);
        config.setSecrets(Arrays.asList("current-secret", "old-secret"));
        config.setOnUnavailable("STRICT");
        assertDoesNotThrow(() -> StickySessionConfigValidator.validate(config));
    }

    @Test
    public void unknownFailurePolicyIsRejected() {
        StickySessionConfig config = new StickySessionConfig();
        config.setEnabled(true);
        config.setSecrets(Collections.singletonList("secret"));
        config.setOnUnavailable("REBIND_ALL");
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> StickySessionConfigValidator.validate(config));
        assertEquals("sticky.onUnavailable must be REBIND or STRICT, got: REBIND_ALL", ex.getMessage());
    }

    @Test
    public void lowerCasePolicyIsRejectedBecauseRuntimeComparesExactly() {
        StickySessionConfig config = new StickySessionConfig();
        config.setEnabled(true);
        config.setSecrets(Collections.singletonList("secret"));
        config.setOnUnavailable("strict");
        assertThrows(IllegalArgumentException.class, () -> StickySessionConfigValidator.validate(config));
    }

    @Test
    public void nonPositiveMaxAgeIsRejected() {
        StickySessionConfig config = new StickySessionConfig();
        config.setEnabled(true);
        config.setSecrets(Collections.singletonList("secret"));
        config.setMaxAgeSeconds(0);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> StickySessionConfigValidator.validate(config));
        assertEquals("sticky.maxAgeSeconds must be a positive number, got: 0", ex.getMessage());
    }

    @Test
    public void blankCookieNameIsRejected() {
        StickySessionConfig config = enabledConfig();
        config.setCookieName(" ");
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> StickySessionConfigValidator.validate(config));
        assertEquals("sticky.cookieName must not be blank; omit it to use the default", ex.getMessage());
    }

    @Test
    public void explicitNullCookieNameIsRejected() {
        StickySessionConfig config = enabledConfig();
        config.setCookieName(null);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> StickySessionConfigValidator.validate(config));
        assertEquals("sticky.cookieName must not be blank; omit it to use the default", ex.getMessage());
    }

    @Test
    public void enabledWithoutSecretsIsRejected() {
        StickySessionConfig config = new StickySessionConfig();
        config.setEnabled(true);
        config.setSecrets(Collections.emptyList());
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> StickySessionConfigValidator.validate(config));
        assertEquals("sticky.secrets must contain at least one key when sticky is enabled", ex.getMessage());
    }

    @Test
    public void enabledWithBlankSecretIsRejected() {
        StickySessionConfig config = new StickySessionConfig();
        config.setEnabled(true);
        config.setSecrets(Arrays.asList("current-secret", "  "));
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> StickySessionConfigValidator.validate(config));
        assertEquals("sticky.secrets must not contain blank keys", ex.getMessage());
    }

    @Test
    public void deterministicKeyWithoutNameIsRejected() {
        StickySessionConfig config = new StickySessionConfig();
        config.setEnabled(true);
        config.setSecrets(Collections.singletonList("secret"));
        config.setKey(new DeterministicKeyConfig());
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> StickySessionConfigValidator.validate(config));
        assertEquals("sticky.key.name must not be blank when sticky.key is configured", ex.getMessage());
    }

    @Test
    public void deterministicKeyWithNamePasses() {
        StickySessionConfig config = enabledConfig();
        DeterministicKeyConfig key = new DeterministicKeyConfig();
        key.setName("X-User-Id");
        config.setKey(key);
        assertDoesNotThrow(() -> StickySessionConfigValidator.validate(config));
    }

    @Test
    public void deterministicKeyWithHeaderTypePasses() {
        StickySessionConfig config = enabledConfig();
        DeterministicKeyConfig key = new DeterministicKeyConfig();
        key.setName("X-User-Id");
        key.setType(DeterministicKeyConfig.KeyType.HEADER);
        config.setKey(key);
        assertDoesNotThrow(() -> StickySessionConfigValidator.validate(config));
    }

    @Test
    public void deterministicKeyWithCookieTypePasses() {
        StickySessionConfig config = enabledConfig();
        DeterministicKeyConfig key = new DeterministicKeyConfig();
        key.setName("session");
        key.setType(DeterministicKeyConfig.KeyType.COOKIE);
        config.setKey(key);
        assertDoesNotThrow(() -> StickySessionConfigValidator.validate(config));
    }

    @Test
    public void deterministicKeyWithoutTypeIsRejected() {
        StickySessionConfig config = enabledConfig();
        DeterministicKeyConfig key = new DeterministicKeyConfig();
        key.setName("X-User-Id");
        key.setType(null);
        config.setKey(key);
        IllegalArgumentException ex = assertThrows(IllegalArgumentException.class,
                () -> StickySessionConfigValidator.validate(config));
        assertEquals("sticky.key.type must be HEADER or COOKIE", ex.getMessage());
    }

    @Test
    public void enabledValidationAgreesWithRuntimeActivation() {
        assertValidatorAgreesWithRuntime(enabledConfig());

        StickySessionConfig zeroLifetime = enabledConfig();
        zeroLifetime.setMaxAgeSeconds(0);
        assertValidatorAgreesWithRuntime(zeroLifetime);

        StickySessionConfig blankCookie = enabledConfig();
        blankCookie.setCookieName(" ");
        assertValidatorAgreesWithRuntime(blankCookie);

        StickySessionConfig nullCookie = enabledConfig();
        nullCookie.setCookieName(null);
        assertValidatorAgreesWithRuntime(nullCookie);

        StickySessionConfig missingSecrets = enabledConfig();
        missingSecrets.setSecrets(null);
        assertValidatorAgreesWithRuntime(missingSecrets);

        StickySessionConfig emptySecrets = enabledConfig();
        emptySecrets.setSecrets(Collections.emptyList());
        assertValidatorAgreesWithRuntime(emptySecrets);

        StickySessionConfig blankSecret = enabledConfig();
        blankSecret.setSecrets(Arrays.asList("secret", " "));
        assertValidatorAgreesWithRuntime(blankSecret);

        StickySessionConfig disabled = enabledConfig();
        disabled.setEnabled(false);
        assertDoesNotThrow(() -> StickySessionConfigValidator.validate(disabled));
        assertFalse(disabled.isUsable());
    }

    private StickySessionConfig enabledConfig() {
        StickySessionConfig config = new StickySessionConfig();
        config.setEnabled(true);
        config.setSecrets(Collections.singletonList("secret"));
        return config;
    }

    private void assertValidatorAgreesWithRuntime(final StickySessionConfig config) {
        boolean accepted;
        try {
            StickySessionConfigValidator.validate(config);
            accepted = true;
        } catch (IllegalArgumentException ex) {
            accepted = false;
        }
        assertEquals(config.isUsable(), accepted);
    }
}
