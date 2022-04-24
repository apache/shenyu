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
 * Test case for SelectorData.
 */
public class SelectorDataTest {
    
    @Test
    public void testBuilderAndGetterSetter() {
        SelectorData selectorData = SelectorData.builder().id("id").pluginId("pluginId").pluginName("pluginName")
                .name("name").matchMode(0).type(0).sort(0).enabled(true)
                .logged(true).continued(true).handle("handle").conditionList(new ArrayList<>(1)).build();
    
        assertThat(selectorData.getId(), is("id"));
        assertThat(selectorData.getName(), is("name"));
        assertThat(selectorData.getPluginId(), is("pluginId"));
        assertThat(selectorData.getPluginName(), is("pluginName"));
        assertThat(selectorData.getName(), is("name"));
        assertThat(selectorData.getMatchMode(), is(0));
        assertThat(selectorData.getType(), is(0));
        assertThat(selectorData.getSort(), is(0));
        assertThat(selectorData.getEnabled(), is(true));
        assertThat(selectorData.getLogged(), is(true));
        assertThat(selectorData.getContinued(), is(true));
        assertThat(selectorData.getHandle(), is("handle"));
        assertThat(selectorData.getConditionList(), hasSize(0));
    
        selectorData.setId("newId");
        selectorData.setName("newName");
        selectorData.setPluginId("newPluginId");
        selectorData.setPluginName("newPlugin");
        selectorData.setName("newName");
        selectorData.setMatchMode(1);
        selectorData.setType(1);
        selectorData.setSort(99);
        selectorData.setEnabled(false);
        selectorData.setLogged(false);
        selectorData.setContinued(false);
        selectorData.setHandle("newHandle");
        selectorData.setConditionList(null);
    
        assertThat(selectorData.getId(), is("newId"));
        assertThat(selectorData.getName(), is("newName"));
        assertThat(selectorData.getPluginId(), is("newPluginId"));
        assertThat(selectorData.getPluginName(), is("newPlugin"));
        assertThat(selectorData.getName(), is("newName"));
        assertThat(selectorData.getMatchMode(), is(1));
        assertThat(selectorData.getType(), is(1));
        assertThat(selectorData.getSort(), is(99));
        assertThat(selectorData.getEnabled(), is(false));
        assertThat(selectorData.getLogged(), is(false));
        assertThat(selectorData.getContinued(), is(false));
        assertThat(selectorData.getHandle(), is("newHandle"));
        assertThat(selectorData.getConditionList(), is(nullValue()));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        SelectorData selectorData1 = SelectorData.builder().id("id").pluginId("pluginId").pluginName("pluginName")
                .name("name").matchMode(0).type(0).sort(0).enabled(true)
                .logged(true).continued(true).handle("handle").conditionList(new ArrayList<>(1)).build();
        SelectorData selectorData2 = SelectorData.builder().id("id").pluginId("pluginId").pluginName("pluginName")
                .name("name").matchMode(0).type(0).sort(0).enabled(true)
                .logged(true).continued(true).handle("handle").conditionList(new ArrayList<>(1)).build();
        
        Set<SelectorData> set = new HashSet<>();
        set.add(selectorData1);
        set.add(selectorData2);
        
        assertThat(set, hasSize(1));
    }
    
}
