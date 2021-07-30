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

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertEquals;

/**
 * Test case for ContextMappingHandle.
 */
public class ContextMappingHandleTest {

    private static final String PATH = "/path/ContextMapping";

    @Test
    public void testCreateDefault() {
        ContextMappingHandle contextMappingHandle = new ContextMappingHandle();
        RuleHandle aDefault = contextMappingHandle.createDefault(PATH);
        assertNotNull(aDefault);
        assertEquals(aDefault, contextMappingHandle);
        assertEquals(contextMappingHandle.getContextPath(), PATH);
    }


    /**
     * just improve code coverage.
     */
    @Test
    public void testGetterSetter() {
        ContextMappingHandle contextMappingHandle = new ContextMappingHandle();
        contextMappingHandle.setContextPath("contextPath");
        assertTrue(StringUtils.equals(contextMappingHandle.getContextPath(), "contextPath"));
        contextMappingHandle.setAddPrefix("addPrefix");
        assertTrue(StringUtils.equals(contextMappingHandle.getAddPrefix(), "addPrefix"));
    }

}
