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

import org.apache.shenyu.common.dto.convert.rule.RuleHandle;
import org.junit.Test;
import org.springframework.http.HttpStatus;

import java.util.Collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

/**
 * Test case for ModifyResponseRuleHandle.
 */
public class ModifyResponseRuleHandleTest {

    private static final String PATH = "/path/ModifyResponseRuleHandle";

    @Test
    public void testCreateDefault() {
        ModifyResponseRuleHandle modifyResponseRuleHandle = new ModifyResponseRuleHandle();
        RuleHandle aDefault = modifyResponseRuleHandle.createDefault(PATH);
        assertNotNull(aDefault);
        assertEquals(aDefault, modifyResponseRuleHandle);
        assertEquals(HttpStatus.OK.value(), modifyResponseRuleHandle.getStatusCode());
    }

    /**
     * just improve code coverage.
     */
    @Test
    public void testGetterSetter() {
        ModifyResponseRuleHandle modifyResponseRuleHandle = new ModifyResponseRuleHandle();
        modifyResponseRuleHandle.setAddHeaders(Collections.EMPTY_MAP);
        assertEquals(modifyResponseRuleHandle.getAddHeaders(), Collections.EMPTY_MAP);
        modifyResponseRuleHandle.setSetHeaders(Collections.EMPTY_MAP);
        assertEquals(modifyResponseRuleHandle.getSetHeaders(), Collections.EMPTY_MAP);
        modifyResponseRuleHandle.setReplaceHeaderKeys(Collections.EMPTY_MAP);
        assertEquals(modifyResponseRuleHandle.getReplaceHeaderKeys(), Collections.EMPTY_MAP);
        modifyResponseRuleHandle.setRemoveHeaderKeys(Collections.EMPTY_SET);
        assertEquals(modifyResponseRuleHandle.getRemoveHeaderKeys(), Collections.EMPTY_SET);
        modifyResponseRuleHandle.setStatusCode(HttpStatus.OK.value());
        assertEquals(modifyResponseRuleHandle.getStatusCode(), HttpStatus.OK.value());
        modifyResponseRuleHandle.setAddBodyKeys(Collections.EMPTY_LIST);
        assertEquals(modifyResponseRuleHandle.getAddBodyKeys(), Collections.EMPTY_LIST);
        modifyResponseRuleHandle.setReplaceBodyKeys(Collections.EMPTY_LIST);
        assertEquals(modifyResponseRuleHandle.getReplaceBodyKeys(), Collections.EMPTY_LIST);
        modifyResponseRuleHandle.setRemoveBodyKeys(Collections.EMPTY_SET);
        assertEquals(modifyResponseRuleHandle.getRemoveBodyKeys(), Collections.EMPTY_SET);
        assertNotNull(modifyResponseRuleHandle.toString());
    }

}
