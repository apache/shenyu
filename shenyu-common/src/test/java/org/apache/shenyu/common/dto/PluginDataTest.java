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

import java.util.HashSet;
import java.util.Set;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;

/**
 * Test case for PluginData.
 */
public class PluginDataTest {
    
    @Test
    public void testBuilderAndGetterSetter() {
        PluginData pluginData = PluginData.builder().id("id").name("name").config("config")
                .role("role").enabled(true).sort(0).build();
        
        assertThat(pluginData.getId(), is("id"));
        assertThat(pluginData.getName(), is("name"));
        assertThat(pluginData.getConfig(), is("config"));
        assertThat(pluginData.getRole(), is("role"));
        assertThat(pluginData.getEnabled(), is(true));
        assertThat(pluginData.getSort(), is(0));
        
        pluginData.setId("newId");
        pluginData.setName("newName");
        pluginData.setConfig("newConfig");
        pluginData.setRole("newRole");
        pluginData.setEnabled(false);
        pluginData.setSort(-1);
        
        assertThat(pluginData.getId(), is("newId"));
        assertThat(pluginData.getName(), is("newName"));
        assertThat(pluginData.getConfig(), is("newConfig"));
        assertThat(pluginData.getRole(), is("newRole"));
        assertThat(pluginData.getEnabled(), is(false));
        assertThat(pluginData.getSort(), is(-1));
    }
    
    @Test
    public void testEqualsAndHashCode() {
        PluginData pluginData1 = PluginData.builder().id("id").name("name").config("config")
                .role("role").enabled(true).sort(0).build();
        PluginData pluginData2 = PluginData.builder().id("id").name("name").config("config")
                .role("role").enabled(true).sort(0).build();
        
        Set<PluginData> set = new HashSet<>();
        set.add(pluginData1);
        set.add(pluginData2);
        
        assertThat(set, hasSize(1));
    }
    
}
