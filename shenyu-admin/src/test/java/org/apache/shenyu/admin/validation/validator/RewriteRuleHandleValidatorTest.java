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

package org.apache.shenyu.admin.validation.validator;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Tests for rewrite rule handle validation at the Admin write boundary.
 */
public final class RewriteRuleHandleValidatorTest {

    @Test
    public void testApplicableHandles() {
        assertDoesNotThrow(() -> RewriteRuleHandleValidator.validate(null));
        assertDoesNotThrow(() -> RewriteRuleHandleValidator.validate(""));
        assertDoesNotThrow(() -> RewriteRuleHandleValidator.validate("{}"));
        assertDoesNotThrow(() -> RewriteRuleHandleValidator.validate("{\"regex\":\"(test)\",\"replace\":\"/new/$1\"}"));
        assertDoesNotThrow(() -> RewriteRuleHandleValidator.validate("{\"regex\":\"/shenyu/test\",\"replace\":\"/new\"}"));
        assertDoesNotThrow(() -> RewriteRuleHandleValidator.validate("{\"regex\":\"/shenyu/{id}\",\"replace\":\"/target/{id}\"}"));
        assertDoesNotThrow(() -> RewriteRuleHandleValidator.validate("{\"regex\":\"test\",\"replace\":\"/new\",\"percentage\":30}"));
        // an incomplete expression means the plugin skips the rewrite
        assertDoesNotThrow(() -> RewriteRuleHandleValidator.validate("{\"regex\":\"test\"}"));
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "not-json",
        "[]",
        "\"test\"",
        "null",
        "{\"regex\":\"test\",\"replace\":\"/new/$1\"}",
        "{\"regex\":\"test\",\"replace\":\"/v1/$$\"}",
        "{\"regex\":\"test\",\"replace\":\"/new/$\"}",
        "{\"regex\":\"test\",\"replace\":\"/new/${name\"}",
        "{\"regex\":\"(test\",\"replace\":\"/new\"}",
        "{\"regex\":\"[a-\",\"replace\":\"/new\"}",
        "{\"regex\":\"test\\\\\",\"replace\":\"/new\"}"
    })
    public void testUnapplicableHandles(final String handle) {
        assertThrows(ShenyuAdminException.class, () -> RewriteRuleHandleValidator.validate(handle));
    }

    @Test
    public void testMessageNamesTheProblem() {
        final ShenyuAdminException error = assertThrows(ShenyuAdminException.class,
            () -> RewriteRuleHandleValidator.validate("{\"regex\":\"test\",\"replace\":\"/new/$1\"}"));
        assertTrue(error.getMessage().contains("invalid replacement template"));
    }
}
