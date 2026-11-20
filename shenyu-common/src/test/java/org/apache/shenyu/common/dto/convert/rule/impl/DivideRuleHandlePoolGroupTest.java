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

package org.apache.shenyu.common.dto.convert.rule.impl;

import org.apache.shenyu.common.dto.convert.rule.canary.CanaryConfigValidator;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for the optional poolGroup field of {@link DivideRuleHandle}: it binds from the
 * handle JSON and stays null for every existing rule so the default pool is untouched.
 */
public final class DivideRuleHandlePoolGroupTest {

    @Test
    public void poolGroupBindsFromHandleJson() {
        DivideRuleHandle handle = CanaryConfigValidator.parseHandle("{\"retry\":1,\"poolGroup\":\"bulk\"}");
        assertEquals("bulk", handle.getPoolGroup());
        assertEquals(1, handle.getRetry());
    }

    @Test
    public void missingPoolGroupDefaultsToNull() {
        DivideRuleHandle handle = CanaryConfigValidator.parseHandle("{\"retry\":3}");
        assertNull(handle.getPoolGroup());
    }

    @Test
    public void poolGroupParticipatesInEqualsHashCodeAndToString() {
        DivideRuleHandle bulk = new DivideRuleHandle();
        bulk.setPoolGroup("bulk");
        DivideRuleHandle interactive = new DivideRuleHandle();
        interactive.setPoolGroup("interactive");

        assertNotEquals(bulk, interactive);
        assertNotEquals(bulk.hashCode(), interactive.hashCode());
        assertTrue(bulk.toString().contains("poolGroup='bulk'"));
    }
}
