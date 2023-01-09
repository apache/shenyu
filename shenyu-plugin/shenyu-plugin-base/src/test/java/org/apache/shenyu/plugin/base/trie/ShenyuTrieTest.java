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

package org.apache.shenyu.plugin.base.trie;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.TrieMatchModeEvent;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;

import java.util.Collections;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

class ShenyuTrieTest {
    
    @Test
    public void clear() {
        this.mockAntPathShenyuTrie();
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).clear();
        Assertions.assertTrue(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).isEmpty());
    }
    
    @Test
    public void isEmpty() {
        this.mockAntPathShenyuTrie();
        Assertions.assertTrue(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).isEmpty());
    }
    
    @Test
    public void putNode() {
        this.mockAntPathShenyuTrie();
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue("/a/b/c/**");
        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/a/b/c/**", ruleData, null);
        Assertions.assertNotNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class));
    }
    
    @Test
    public void match() {
        this.mockAntPathShenyuTrie();
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue("/a/b/c/**");
        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/a/b/c/**", ruleData, null);
        Assertions.assertNotNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).match("/a/b/c/d/e/f", "1"));
    
        RuleData ruleData2 = RuleData.builder()
                .id("2")
                .pluginName("test2")
                .selectorId("2")
                .name("test-plugin-rule")
                .enabled(true)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/a/*/b/c", ruleData2, null);
        Assertions.assertNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).match("/a/m/b/c", "1"));
        Assertions.assertNotNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).match("/a/m/b/c", "2"));
        
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/path1/{name}/{age}", ruleData, null);
        Assertions.assertNotNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).match("/path1/111/222", "1"));
        Assertions.assertNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).match("/path1/111/222/333", "1"));
        
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("path1/name/age", ruleData, null);
        Assertions.assertNotNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).match("path1/name/age", "1"));
        Assertions.assertEquals(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).match("path1/name/age", "1").getFullPath(), "path1/name/age");
    }
    
    @Test
    public void remove() {
        this.mockAntPathShenyuTrie();
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue("/a/b/c/**");
        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("2")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(1)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        RuleData ruleData2 = RuleData.builder()
                .id("2")
                .pluginName("test")
                .selectorId("2")
                .name("test-plugin-rule2")
                .enabled(true)
                .sort(2)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/a/b/c/**", ruleData, null);
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/a/b/c/**", ruleData2, null);
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).remove("/a/b/c/**", "2", "2");
        Assertions.assertNotNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).getNode("/a/b/c/**"));
    
        RuleData ruleData3 = RuleData.builder()
                .id("3")
                .pluginName("test")
                .selectorId("3")
                .name("test-plugin-rule3")
                .enabled(true)
                .sort(2)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/path1/path2", ruleData3, null);
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).remove("/path1/path2", "3", "3");
        Assertions.assertNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).getNode("/path1/path2"));
    }
    
    @Test
    public void getNode() {
        this.mockAntPathShenyuTrie();
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue("/a/b/c/**");
        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("2")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(1)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        RuleData ruleData2 = RuleData.builder()
                .id("2")
                .pluginName("test2")
                .selectorId("2")
                .name("test-plugin-rule2")
                .enabled(true)
                .sort(2)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/a/b/c/**", ruleData, null);
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/a/b/c/**", ruleData2, null);
        Assertions.assertNotNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).getNode("/a/b/c/**"));
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/path1/{age}/{name}", ruleData2, null);
        Assertions.assertNotNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).getNode("/path1/{age}/{name}"));
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/aaa/bbb/ccc", ruleData2, null);
        Assertions.assertNotNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).getNode("/aaa/bbb/ccc"));
        SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).putNode("/aa/*/cc", ruleData2, null);
        Assertions.assertNotNull(SpringBeanUtils.getInstance().getBean(ShenyuTrie.class).getNode("/aa/*/cc"));
    }
    
    private void mockAntPathShenyuTrie() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuTrie.class)).thenReturn(new ShenyuTrie(100L, 100L, 100L, TrieMatchModeEvent.ANT_PATH_MATCH.getMatchMode()));
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }
}