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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.common.dto.RuleData;
import org.junit.jupiter.api.Test;

/**
 * CacheKeyUtilsTest test.
 */
public final class CacheKeyUtilsTest {

    private static final String MOCK_SELECTOR_ID = "mockSelectorId";

    private static final String MOCK_NAME = "mockName";

    private static final String MOCK_CACHE_KEY_UTILS_GET_KEY_RESULT = "mockSelectorId_mockName";

    @Test
    public void getKey() {
        RuleData ruleData = RuleData.builder().selectorId(MOCK_SELECTOR_ID).id(MOCK_NAME).build();
        assert MOCK_CACHE_KEY_UTILS_GET_KEY_RESULT.equals(CacheKeyUtils.INST.getKey(ruleData));
    }
}
