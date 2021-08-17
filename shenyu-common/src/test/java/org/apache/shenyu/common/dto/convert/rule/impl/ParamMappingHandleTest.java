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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.dto.convert.rule.RuleHandle;
import org.junit.Test;

import java.util.Collections;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * Test case for ParamMappingHandle.
 */
public class ParamMappingHandleTest {

    private static final String PATH = "/path/ModifyResponseRuleHandle";

    @Test
    public void testCreateDefault() {
        ParamMappingHandle paramMappingHandle = new ParamMappingHandle();
        RuleHandle aDefault = paramMappingHandle.createDefault(PATH);
        assertNotNull(aDefault);
        assertEquals(aDefault, paramMappingHandle);
    }

    /**
     * just for improve code coverage.
     */
    @Test
    public void testGetterSetter() {
        ParamMappingHandle paramMappingHandle = new ParamMappingHandle();
        paramMappingHandle.setAddParameterKeys(Collections.EMPTY_LIST);
        assertEquals(paramMappingHandle.getAddParameterKeys(), Collections.EMPTY_LIST);
        paramMappingHandle.setReplaceParameterKeys(Collections.EMPTY_LIST);
        assertEquals(paramMappingHandle.getReplaceParameterKeys(), Collections.EMPTY_LIST);
        paramMappingHandle.setRemoveParameterKeys(Collections.EMPTY_SET);
        assertEquals(paramMappingHandle.getRemoveParameterKeys(), Collections.EMPTY_SET);
        ParamMappingHandle.ParamMapInfo paramMapInfo = new ParamMappingHandle.ParamMapInfo();
        paramMapInfo.setPath("path");
        assertTrue(StringUtils.equals(paramMapInfo.getPath(), "path"));
        paramMapInfo.setKey("key");
        assertTrue(StringUtils.equals(paramMapInfo.getKey(), "key"));
        paramMapInfo.setValue("value");
        assertTrue(StringUtils.equals(paramMapInfo.getValue(), "value"));
    }

}
