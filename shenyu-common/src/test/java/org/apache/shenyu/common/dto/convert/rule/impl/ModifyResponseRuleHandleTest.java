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
import java.util.HashMap;
import java.util.HashSet;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.core.Is.is;

/**
 * Test case for ModifyResponseRuleHandle.
 */
public class ModifyResponseRuleHandleTest {
    
    @Test
    public void testGetterSetter() {
        ModifyResponseRuleHandle handle = new ModifyResponseRuleHandle();
        
        handle.setAddHeaders(new HashMap<>());
        handle.setSetHeaders(new HashMap<>());
        handle.setReplaceHeaderKeys(new HashMap<>());
        handle.setRemoveHeaderKeys(new HashSet<>());
        handle.setStatusCode(1);
        handle.setAddBodyKeys(Arrays.asList(new ParamMappingRuleHandle.ParamMapInfo()));
        handle.setReplaceBodyKeys(Arrays.asList(new ParamMappingRuleHandle.ParamMapInfo()));
        handle.setRemoveBodyKeys(new HashSet<>());
    
        assertThat(handle.getAddHeaders(), is(notNullValue()));
        assertThat(handle.getSetHeaders(), is(notNullValue()));
        assertThat(handle.getReplaceHeaderKeys(), is(notNullValue()));
        assertThat(handle.getRemoveHeaderKeys(), is(notNullValue()));
        assertThat(handle.getRemoveBodyKeys(), is(notNullValue()));
        assertThat(handle.getStatusCode(), is(1));
        assertThat(handle.getAddBodyKeys(), hasSize(1));
        assertThat(handle.getReplaceBodyKeys(), hasSize(1));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        ModifyResponseRuleHandle handle1 = new ModifyResponseRuleHandle();
        ModifyResponseRuleHandle handle2 = new ModifyResponseRuleHandle();
        
        assertThat(ImmutableSet.of(handle1, handle2), hasSize(1));
    }
    
}
