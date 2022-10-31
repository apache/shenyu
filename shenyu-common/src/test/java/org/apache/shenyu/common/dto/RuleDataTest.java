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

package org.apache.shenyu.common.dto;

import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;
import static org.hamcrest.core.IsNull.nullValue;

/**
 * Test case for RuleData.
 */
public class RuleDataTest {
    
    @Test
    public void testBuilderAndGetterSetter() {
        RuleData ruleData = RuleData.builder().id("id").name("name").pluginName("pluginName")
                .selectorId("selectorId").matchMode(1).sort(0).enabled(true).loged(true)
                .handle("handle").conditionDataList(new ArrayList<>()).build();
        
        assertThat(ruleData.getId(), is("id"));
        assertThat(ruleData.getName(), is("name"));
        assertThat(ruleData.getPluginName(), is("pluginName"));
        assertThat(ruleData.getSelectorId(), is("selectorId"));
        assertThat(ruleData.getMatchMode(), is(1));
        assertThat(ruleData.getSort(), is(0));
        assertThat(ruleData.getEnabled(), is(true));
        assertThat(ruleData.getLoged(), is(true));
        assertThat(ruleData.getHandle(), is("handle"));
        assertThat(ruleData.getConditionDataList(), hasSize(0));
        
        ruleData.setId("newId");
        ruleData.setName("newName");
        ruleData.setPluginName("newPlugin");
        ruleData.setSelectorId("newSelector");
        ruleData.setMatchMode(0);
        ruleData.setSort(99);
        ruleData.setEnabled(false);
        ruleData.setLoged(false);
        ruleData.setHandle("newHandle");
        ruleData.setConditionDataList(null);
    
        assertThat(ruleData.getId(), is("newId"));
        assertThat(ruleData.getName(), is("newName"));
        assertThat(ruleData.getPluginName(), is("newPlugin"));
        assertThat(ruleData.getSelectorId(), is("newSelector"));
        assertThat(ruleData.getMatchMode(), is(0));
        assertThat(ruleData.getSort(), is(99));
        assertThat(ruleData.getEnabled(), is(false));
        assertThat(ruleData.getLoged(), is(false));
        assertThat(ruleData.getHandle(), is("newHandle"));
        assertThat(ruleData.getConditionDataList(), is(nullValue()));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        RuleData ruleData1 = RuleData.builder().id("id").name("name").pluginName("pluginName")
                .selectorId("selectorId").matchMode(1).sort(0).enabled(true).loged(true)
                .handle("handle").conditionDataList(new ArrayList<>()).build();
        RuleData ruleData2 = RuleData.builder().id("id").name("name").pluginName("pluginName")
                .selectorId("selectorId").matchMode(1).sort(0).enabled(true).loged(true)
                .handle("handle").conditionDataList(new ArrayList<>()).build();
        
        Set<RuleData> set = new HashSet<>();
        set.add(ruleData1);
        set.add(ruleData2);
        
        assertThat(set, hasSize(1));
    }
    
}
