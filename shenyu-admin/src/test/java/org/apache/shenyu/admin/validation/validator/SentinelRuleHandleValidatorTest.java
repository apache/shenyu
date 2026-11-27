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

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class SentinelRuleHandleValidatorTest {

    private static final String HEADER_RULE = """
            {"flowRuleCount":10,"paramFlowEnable":1,"paramFlowSource":"HEADER","paramFlowKey":"X-Quota-Group"}
            """;

    @Test
    void acceptsLegacyAndOptInHandles() {
        assertDoesNotThrow(() -> SentinelRuleHandleValidator.validate("{\"flowRuleCount\":10,\"flowRuleGrade\":0}"));
        assertDoesNotThrow(() -> SentinelRuleHandleValidator.validate(HEADER_RULE));
        assertDoesNotThrow(() -> SentinelRuleHandleValidator.validate(HEADER_RULE.replace("\"HEADER\"", "\"EXCHANGE_ATTRIBUTE\"")));
    }

    @Test
    void rejectsInvalidParameterSettings() {
        assertInvalid(HEADER_RULE.replace("\"paramFlowEnable\":1", "\"paramFlowEnable\":1,\"flowRuleEnable\":0"),
                "paramFlowEnable=1 requires flowRuleEnable=1");
        assertInvalid(HEADER_RULE.replace("\"HEADER\"", "\"INVALID\""), "paramFlowSource");
        assertInvalid(HEADER_RULE.replace("X-Quota-Group", " "), "paramFlowKey");
        assertInvalid(HEADER_RULE.replace("Count\":10", "Count\":0"), "flowRuleCount");
        assertInvalid(HEADER_RULE.replace("\"flowRuleCount\":10", "\"flowRuleCount\":10,\"flowRuleGrade\":0"), "QPS grade");
        assertInvalid(HEADER_RULE.replace("\"flowRuleCount\":10", "\"flowRuleCount\":10,\"paramFlowMissingKeyPolicy\":\"INVALID\""), "paramFlowMissingKeyPolicy");
    }

    private void assertInvalid(final String handle, final String field) {
        ShenyuAdminException exception = assertThrows(ShenyuAdminException.class, () -> SentinelRuleHandleValidator.validate(handle));
        assertTrue(exception.getMessage().contains(field));
    }
}
