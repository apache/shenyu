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
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.enums.TrieMatchModeEnum;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import org.springframework.context.ConfigurableApplicationContext;

import java.util.Collections;

public class ShenyuSelectorTrieTest {
    
    private ShenyuTrie shenyuAntPathTrie;
    
    private ShenyuTrie shenyuPathPatternTrie;
    
    @BeforeEach
    public void mockAntPathShenyuTrie() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        when(context.getBean(ShenyuTrie.class)).thenReturn(new ShenyuTrie(100L, TrieMatchModeEnum.ANT_PATH_MATCH.getMatchMode()));
        SpringBeanUtils.getInstance().setApplicationContext(context);
        shenyuAntPathTrie = SpringBeanUtils.getInstance().getBean(ShenyuTrie.class);
        
        when(context.getBean(ShenyuTrie.class)).thenReturn(new ShenyuTrie(100L, TrieMatchModeEnum.PATH_PATTERN.getMatchMode()));
        SpringBeanUtils.getInstance().setApplicationContext(context);
        shenyuPathPatternTrie = SpringBeanUtils.getInstance().getBean(ShenyuTrie.class);
    }
    
    @Test
    public void clear() {
        shenyuAntPathTrie.clear();
        shenyuPathPatternTrie.clear();
        Assertions.assertTrue(shenyuAntPathTrie.isEmpty());
        Assertions.assertTrue(shenyuPathPatternTrie.isEmpty());
    }
    
    @Test
    public void putNode() {
        final String uri = "/a/b/c/**/*/{name}/cc";
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue(uri);
        SelectorData selectorData = SelectorData.builder()
                .id("1")
                .pluginName("test")
                .name("test-plugin-selector")
                .enabled(true)
                .conditionList(Collections.singletonList(conditionData))
                .build();
        
        Assertions.assertNull(shenyuAntPathTrie.getNode(uri, "1"));
        shenyuAntPathTrie.putNode(uri, selectorData, TrieCacheTypeEnum.SELECTOR);
        Assertions.assertNotNull(shenyuAntPathTrie.getNode(uri, "test"));
    }
    
    @Test
    public void remove() {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue("/a/b/c/**");
        SelectorData selectorData = SelectorData.builder()
                .id("1")
                .pluginName("test")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(1)
                .conditionList(Collections.singletonList(conditionData))
                .build();
        SelectorData selectorData2 = SelectorData.builder()
                .id("2")
                .pluginName("test2")
                .name("test-plugin-rule2")
                .enabled(true)
                .sort(2)
                .conditionList(Collections.singletonList(conditionData))
                .build();
        shenyuAntPathTrie.putNode("/a/b/c/**", selectorData, TrieCacheTypeEnum.SELECTOR);
        shenyuAntPathTrie.putNode("/a/b/c/**", selectorData2, TrieCacheTypeEnum.SELECTOR);
        shenyuAntPathTrie.remove("/a/b/c/**", selectorData, TrieCacheTypeEnum.SELECTOR);
        Assertions.assertNotNull(shenyuAntPathTrie.getNode("/a/b/c/**", "test2"));
        shenyuAntPathTrie.remove("/a/b/c/**", selectorData2, TrieCacheTypeEnum.SELECTOR);
        Assertions.assertNull(shenyuAntPathTrie.getNode("/a/b/c/**", "test1"));
    }
    
}
