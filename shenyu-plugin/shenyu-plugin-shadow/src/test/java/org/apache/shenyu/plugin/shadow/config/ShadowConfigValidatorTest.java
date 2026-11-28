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

package org.apache.shenyu.plugin.shadow.config;

import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * test case for {@link ShadowConfigValidator}.
 */
public final class ShadowConfigValidatorTest {

    @Test
    public void testAcceptsValidHandle() {
        assertTrue(ShadowConfigValidator.validate(validHandle()).isEmpty());
    }

    @Test
    public void testIgnoresDisabledHandle() {
        ShadowHandle handle = new ShadowHandle();
        handle.setEnabled(false);
        handle.setTarget("not-a-uri");
        assertTrue(ShadowConfigValidator.validate(handle).isEmpty());
    }

    @Test
    public void testRejectsRelativeTarget() {
        assertProblem("shadow target must be an absolute URI");
    }

    @Test
    public void testRejectsUnsupportedScheme() {
        ShadowHandle handle = validHandle();
        handle.setTarget("ftp://shadow:21");
        assertProblem(handle, "must use http or https");
    }

    @Test
    public void testRejectsUserInfo() {
        ShadowHandle handle = validHandle();
        handle.setTarget("http://user:pass@shadow:8080");
        assertProblem(handle, "must not carry user info");
    }

    @Test
    public void testRejectsFragment() {
        ShadowHandle handle = validHandle();
        handle.setTarget("http://shadow:8080#fragment");
        assertProblem(handle, "must not carry a fragment");
    }

    @Test
    public void testRejectsTargetQuery() {
        ShadowHandle handle = validHandle();
        handle.setTarget("http://shadow:8080/base?token=x");
        assertProblem(handle, "must not carry a query");
    }

    @Test
    public void testRejectsMalformedUri() {
        ShadowHandle handle = validHandle();
        handle.setTarget("http://sh adow:8080");
        assertProblem(handle, "not a valid URI");
    }

    @Test
    public void testRejectsMissingTarget() {
        ShadowHandle handle = validHandle();
        handle.setTarget(" ");
        assertProblem(handle, "target is required");
    }

    @Test
    public void testRejectsEmptyMethods() {
        ShadowHandle handle = validHandle();
        handle.setMethods(Collections.emptyList());
        assertProblem(handle, "methods must not be empty");
    }

    @Test
    public void testRejectsRateOutOfRange() {
        ShadowHandle handle = validHandle();
        handle.setSampleRate(1.5D);
        assertProblem(handle, "sampleRate must be within");
    }

    @Test
    public void testRejectsNonPositiveConcurrency() {
        ShadowHandle handle = validHandle();
        handle.setMaxConcurrent(0);
        assertProblem(handle, "maxConcurrent");
    }

    @Test
    public void testRejectsNegativeQueue() {
        ShadowHandle handle = validHandle();
        handle.setQueueCapacity(-1);
        assertProblem(handle, "queueCapacity");
    }

    @Test
    public void testRejectsNegativeResponseBound() {
        ShadowHandle handle = validHandle();
        handle.setMaxResponseBodyBytes(-1);
        assertProblem(handle, "maxResponseBodyBytes");
    }

    @Test
    public void testRejectsMissingCompareHeaders() {
        ShadowHandle handle = validHandle();
        handle.setCompareHeaders(null);
        assertProblem(handle, "compareHeaders");
    }

    @Test
    public void testRejectsMissingIgnorePaths() {
        ShadowHandle handle = validHandle();
        handle.setIgnoreJsonPaths(null);
        assertProblem(handle, "ignoreJsonPaths");
    }

    @Test
    public void testRejectsHeaderLineBreak() {
        ShadowHandle handle = validHandle();
        handle.setExtraHeaders(Map.of("X-Token", "value\r\nInjected: true"));
        assertProblem(handle, "line break");
    }

    private void assertProblem(final String messageFragment) {
        ShadowHandle handle = validHandle();
        handle.setTarget("relative/path");
        assertProblem(handle, messageFragment);
    }

    private void assertProblem(final ShadowHandle handle, final String messageFragment) {
        assertTrue(ShadowConfigValidator.validate(handle)
                .orElseThrow(() -> new AssertionError("expected a validation problem"))
                .contains(messageFragment));
    }

    private ShadowHandle validHandle() {
        ShadowHandle handle = new ShadowHandle();
        handle.setEnabled(true);
        handle.setTarget("http://shadow:8080");
        return handle;
    }

    @Test
    public void testMessagesDoNotEchoTheConfiguredTarget() {
        ShadowHandle handle = validHandle();
        handle.setTarget("http://user:secret@shadow:8080");
        String problem = ShadowConfigValidator.validate(handle).orElseThrow(AssertionError::new);
        assertFalse(problem.contains("secret"));
        assertFalse(problem.contains("shadow:8080"));
    }
}
