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

import com.google.common.collect.ImmutableSet;
import org.junit.jupiter.api.Test;

import java.util.Arrays;
import java.util.HashSet;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.core.Is.is;

/**
 * Test case for ParamMappingRuleHandle.
 */
public class ParamMappingRuleHandleTest {

    @Test
    public void testGetterSetterOfParamMapInfo() {
        ParamMappingRuleHandle.ParamMapInfo paramMapInfo = new ParamMappingRuleHandle.ParamMapInfo();
        
        paramMapInfo.setKey("key");
        paramMapInfo.setPath("path");
        paramMapInfo.setValue("value");
        
        assertThat(paramMapInfo.getKey(), is("key"));
        assertThat(paramMapInfo.getPath(), is("path"));
        assertThat(paramMapInfo.getValue(), is("value"));
    }
    
    @Test
    public void testGetterSetter() {
        ParamMappingRuleHandle handle = new ParamMappingRuleHandle();
    
        handle.setRemoveParameterKeys(new HashSet<>());
        handle.setReplaceParameterKeys(Arrays.asList(new ParamMappingRuleHandle.ParamMapInfo()));
        handle.setAddParameterKeys(Arrays.asList(new ParamMappingRuleHandle.ParamMapInfo()));
    
        assertThat(handle.getRemoveParameterKeys(), is(notNullValue()));
        assertThat(handle.getReplaceParameterKeys(), is(notNullValue()));
        assertThat(handle.getAddParameterKeys(), is(notNullValue()));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        ParamMappingRuleHandle handle1 = new ParamMappingRuleHandle();
        ParamMappingRuleHandle handle2 = new ParamMappingRuleHandle();
    
        assertThat(ImmutableSet.of(handle1, handle2), hasSize(1));
    }

}
