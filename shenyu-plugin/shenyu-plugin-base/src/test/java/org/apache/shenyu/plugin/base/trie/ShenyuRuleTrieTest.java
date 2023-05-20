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
import org.apache.shenyu.common.enums.TrieCacheTypeEnum;
import org.apache.shenyu.common.enums.TrieMatchModeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.ListUtil;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class ShenyuRuleTrieTest {
    
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
    public void testPathPattern() {
        final String matchAllEndUri = "/a/b/c/**";
        final String matchAllNotEndUri = "/a/b/**/c";

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue(matchAllEndUri);

        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();

        Assertions.assertNull(shenyuPathPatternTrie.getNode(matchAllEndUri, "1"));
        shenyuPathPatternTrie.putNode(matchAllEndUri, ruleData, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuPathPatternTrie.getNode(matchAllEndUri, "1"));

        Assertions.assertNull(shenyuAntPathTrie.getNode(matchAllEndUri, "1"));
        shenyuAntPathTrie.putNode(matchAllEndUri, ruleData, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.getNode(matchAllEndUri, "1"));

        ConditionData conditionData1 = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue(matchAllNotEndUri);

        RuleData ruleData1 = RuleData.builder()
                .id("2")
                .pluginName("test")
                .selectorId("2")
                .name("test-plugin-rule")
                .enabled(true)
                .conditionDataList(Collections.singletonList(conditionData1))
                .build();

        Assertions.assertThrows(ShenyuException.class, () -> shenyuPathPatternTrie.putNode(matchAllNotEndUri, ruleData1, TrieCacheTypeEnum.RULE));
        Assertions.assertDoesNotThrow(() -> shenyuAntPathTrie.putNode(matchAllNotEndUri, ruleData1, TrieCacheTypeEnum.RULE));

    }
    
    @Test
    public void putNode() {
        final String uri = "/a/b/c/**/*/{name}/cc";
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue(uri);
        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();

        Assertions.assertNull(shenyuAntPathTrie.getNode(uri, "1"));
        shenyuAntPathTrie.putNode(uri, ruleData, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.getNode(uri, "1"));
    }

    @Test
    public void putNodeSameSelectorId() {
        final String normalUri = "/a/b/c";

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue(normalUri);

        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(2)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();

        RuleData ruleData1 = RuleData.builder()
                .id("2")
                .pluginName("test2")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(1)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();

        shenyuAntPathTrie.putNode(normalUri, ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode(normalUri, ruleData1, TrieCacheTypeEnum.RULE);
        List<RuleData> ruleDataList = ListUtil.castList(shenyuAntPathTrie.getNode(normalUri, "1").getPathCache().get("1"), RuleData.class::cast);
        Assertions.assertEquals(ruleDataList.get(0).getId(), "2");
        Assertions.assertEquals(ruleDataList.get(1).getId(), "1");
    }

    @Test
    public void putNodeDifferentSelectorId() {
        final String normalUri = "/a/b/c";

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue(normalUri);

        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(2)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();

        RuleData ruleData1 = RuleData.builder()
                .id("2")
                .pluginName("test2")
                .selectorId("2")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(1)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();

        shenyuAntPathTrie.putNode(normalUri, ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode(normalUri, ruleData1, TrieCacheTypeEnum.RULE);
        Assertions.assertEquals(shenyuAntPathTrie.getNode(normalUri, "1").getPathCache().size(), 1);
    }

    @Test
    public void pathPatternMatch() {
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
        shenyuPathPatternTrie.putNode("/aa/bb/cc/{name}/{age}/tt", ruleData, TrieCacheTypeEnum.RULE);
        shenyuPathPatternTrie.putNode("/aa/bb/cc/*/*/ii", ruleData, TrieCacheTypeEnum.RULE);
        shenyuPathPatternTrie.putNode("/aa/bb/cc/**", ruleData, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuPathPatternTrie.match("/aa/bb/cc/hh/dd/ee/hh", "1"));
        Assertions.assertNotNull(shenyuPathPatternTrie.match("/aa/bb/cc/hh/dd/ee/tt", "1"));
        Assertions.assertNotNull(shenyuPathPatternTrie.match("/aa/bb/cc/xx/yy/ii", "1"));
        Assertions.assertNull(shenyuPathPatternTrie.match("/aa/bb/mm/yyy/hhhl", "1"));
    }
    
    @Test
    public void match() {
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
        
        shenyuAntPathTrie.putNode("/aa/**/*.html", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/a/b/c/**", ruleData, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.match("/a/b/c/d/e/f", "1"));

        shenyuAntPathTrie.putNode("/a/b/**/c", ruleData, TrieCacheTypeEnum.RULE);
    
        RuleData ruleData2 = RuleData.builder()
                .id("2")
                .pluginName("test2")
                .selectorId("2")
                .name("test-plugin-rule")
                .enabled(true)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        shenyuAntPathTrie.putNode("/a/*/b/c", ruleData2, TrieCacheTypeEnum.RULE);
        Assertions.assertNull(shenyuAntPathTrie.match("/a/m/b/c", "1"));
        Assertions.assertNotNull(shenyuAntPathTrie.match("/a/m/b/c", "2"));
        
        shenyuAntPathTrie.putNode("/path1/{name}/{age}", ruleData, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.match("/path1/111/222", "1").getFullPath(), "/path1/{name}/{age}");
        Assertions.assertNull(shenyuAntPathTrie.match("/path1/111/222/333", "1"));
        
        shenyuAntPathTrie.putNode("path1/name/age", ruleData, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.match("path1/name/age", "1"));
        Assertions.assertEquals(shenyuAntPathTrie.match("path1/name/age", "1").getFullPath(), "path1/name/age");
        
        shenyuAntPathTrie.putNode("/aa/bb/cc/{name}/{age}/tt", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/aa/bb/cc/*/*/ii", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/aa/bb/cc/**/hh", ruleData, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.match("/aa/bb/cc/dd/ee/tt", "1"));
        Assertions.assertNotNull(shenyuAntPathTrie.match("/aa/bb/cc/dd/ee/hh", "1"));
        Assertions.assertNotNull(shenyuAntPathTrie.match("/aa/bb/cc/dd/ee/ii", "1"));
        Assertions.assertNotNull(shenyuAntPathTrie.match("/aa/bb/cc/dd/rr/mm/ee/hh", "1"));
        Assertions.assertNull(shenyuAntPathTrie.match("/aa/bb/cc/dd/rr/mm/ee/yy", "1"));
    }

    @Test
    public void matchSameSelectorId() {
        final String normalUri = "/a/b/c";

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue(normalUri);

        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(2)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();

        RuleData ruleData1 = RuleData.builder()
                .id("2")
                .pluginName("test2")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(1)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();

        shenyuAntPathTrie.putNode(normalUri, ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode(normalUri, ruleData1, TrieCacheTypeEnum.RULE);
        Assertions.assertEquals(shenyuAntPathTrie.match("/a/b/c", "1").getPathCache().get("1").size(), 2);
    }

    @Test
    public void matchSpec() {
        final String uriPath = "/a/b/c/**";
        final String uriPath1 = "/a/*/c/**";
        final String uriPath2 = "/a/*/*/{d}";
        final String uriPath3 = "/a/*/{c}/{d}";
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue(uriPath);
        
        ConditionData conditionData1 = new ConditionData();
        conditionData1.setParamType(ParamTypeEnum.URI.getName());
        conditionData1.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData1.setParamName("/");
        conditionData1.setParamValue(uriPath1);

        ConditionData conditionData2 = new ConditionData();
        conditionData2.setParamType(ParamTypeEnum.URI.getName());
        conditionData2.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData2.setParamName("/");
        conditionData2.setParamValue(uriPath2);

        ConditionData conditionData3 = new ConditionData();
        conditionData3.setParamType(ParamTypeEnum.URI.getName());
        conditionData3.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData3.setParamName("/");
        conditionData3.setParamValue(uriPath3);

        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .conditionDataList(Arrays.asList(conditionData, conditionData1, conditionData2, conditionData3))
                .build();
        shenyuAntPathTrie.putNode(Arrays.asList(uriPath, uriPath1, uriPath2, uriPath3), ruleData, TrieCacheTypeEnum.RULE);
        
        Assertions.assertEquals(shenyuAntPathTrie.match("/a/b/c/d/e/f", "1").getFullPath(), uriPath);
        Assertions.assertEquals(shenyuAntPathTrie.match("/a/g/c/e/ef/hi", "1").getFullPath(), uriPath1);
        Assertions.assertEquals(shenyuAntPathTrie.match("/a/g/hi/def", "1").getFullPath(), uriPath2);
        Assertions.assertEquals(shenyuAntPathTrie.match("/a/gh/ij/klm", "1").getFullPath(), uriPath2);
        Assertions.assertNotEquals(shenyuAntPathTrie.match("/a/egh/fij/klm", "1").getFullPath(), uriPath3);
    }
    
    @Test
    public void remove() {
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
        shenyuAntPathTrie.putNode("/a/b/c/**", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/a/b/c/**", ruleData2, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.remove("/a/b/c/**", ruleData2, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.getNode("/a/b/c/**", "2"));
        shenyuAntPathTrie.remove("/a/b/c/**", ruleData, TrieCacheTypeEnum.RULE);
        Assertions.assertNull(shenyuAntPathTrie.getNode("/a/b/c/**", "1"));
    
        RuleData ruleData3 = RuleData.builder()
                .id("3")
                .pluginName("test")
                .selectorId("3")
                .name("test-plugin-rule3")
                .enabled(true)
                .sort(2)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        shenyuAntPathTrie.putNode("/path1/path2", ruleData3, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.remove("/path1/path2", ruleData3, TrieCacheTypeEnum.RULE);
        Assertions.assertNull(shenyuAntPathTrie.getNode("/path1/path2", "3"));
    }
    
    @Test
    public void getNode() {
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
        shenyuAntPathTrie.putNode("/a/b/c/**", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/a/b/c/**", ruleData2, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.getNode("/a/b/c/**", "2"));
        shenyuAntPathTrie.putNode("/path1/{age}/{name}", ruleData2, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.getNode("/path1/{age}/{name}", "2"));
        shenyuAntPathTrie.putNode("/aaa/bbb/ccc", ruleData2, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.getNode("/aaa/bbb/ccc", "2"));
        shenyuAntPathTrie.putNode("/aa/*/cc", ruleData2, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.getNode("/aa/*/cc", "2"));
        shenyuAntPathTrie.putNode("/a/x/{name}/{age}/b", ruleData2, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/a/x/{name}/{sex}/c", ruleData2, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.getNode("/a/x/{name}/{age}/b", "2"));
    }

    @Test
    public void testWildcardMatch() {
        final String uriPath = "/a/*.html";
        final String uriPath1 = "/a/b/*Safe*/b";
        final String uriPath2 = "/a/c/{name}/*.jpg";
        final String uriPath3 = "/**/*.json";

        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(ParamTypeEnum.URI.getName());
        conditionData.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData.setParamName("/");
        conditionData.setParamValue(uriPath);

        ConditionData conditionData1 = new ConditionData();
        conditionData1.setParamType(ParamTypeEnum.URI.getName());
        conditionData1.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData1.setParamName("/");
        conditionData1.setParamValue(uriPath1);

        ConditionData conditionData2 = new ConditionData();
        conditionData2.setParamType(ParamTypeEnum.URI.getName());
        conditionData2.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData2.setParamName("/");
        conditionData2.setParamValue(uriPath2);

        ConditionData conditionData3 = new ConditionData();
        conditionData3.setParamType(ParamTypeEnum.URI.getName());
        conditionData3.setOperator(OperatorEnum.MATCH.getAlias());
        conditionData3.setParamName("/");
        conditionData3.setParamValue(uriPath3);

        RuleData ruleData = RuleData.builder()
                .id("1")
                .pluginName("test")
                .selectorId("1")
                .name("test-plugin-rule")
                .enabled(true)
                .conditionDataList(Arrays.asList(conditionData, conditionData1, conditionData2, conditionData3))
                .build();
        shenyuAntPathTrie.putNode(Arrays.asList(uriPath, uriPath1, uriPath2, uriPath3), ruleData, TrieCacheTypeEnum.RULE);

        Assertions.assertEquals(shenyuAntPathTrie.match("/a/index.html", "1").getFullPath(), uriPath);
        Assertions.assertEquals(shenyuAntPathTrie.match("/a/b/Safe/b", "1").getFullPath(), uriPath1);
        Assertions.assertEquals(shenyuAntPathTrie.match("/a/b/shenyuSafe/b", "1").getFullPath(), uriPath1);
        Assertions.assertEquals(shenyuAntPathTrie.match("/a/b/shenyuSafeckj/b", "1").getFullPath(), uriPath1);
        Assertions.assertEquals(shenyuAntPathTrie.match("/a/c/Safe/bbb.jpg", "1").getFullPath(), uriPath2);
        Assertions.assertEquals(shenyuAntPathTrie.match("/aa/c/d/c/exx/data.json", "1").getFullPath(), uriPath3);
        Assertions.assertNull(shenyuAntPathTrie.match("/a/c/egh/klm", "1"));
    }
    
    @Test
    public void conflictMatchTest() {
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
                .pluginName("test3")
                .selectorId("3")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(1)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        
        RuleData ruleData3 = RuleData.builder()
                .id("2")
                .pluginName("test3")
                .selectorId("43")
                .name("test-plugin-rule")
                .enabled(true)
                .sort(1)
                .conditionDataList(Collections.singletonList(conditionData))
                .build();
        
        shenyuAntPathTrie.putNode("/http/**", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/http/client/hello", ruleData2, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/http/client/hell", ruleData2, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/http/{name}/hell", ruleData3, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/http/*/helle", ruleData3, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/http/**/hello", ruleData3, TrieCacheTypeEnum.RULE);
        Assertions.assertNotNull(shenyuAntPathTrie.match("/http/client/hell", "43"));
    }
    
    @Test
    public void multiConflictMatchTest() {
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
        
        shenyuAntPathTrie.putNode("/aa/bb/*/a", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/aa/bb/*/b", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/aa/bb/*/c", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/aa/bb/*/*/i", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/aa/bb/**/m/c", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/aa/bb/**/n/b", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/aa/bb/{path}/x/a", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/aa/bb/{path}/y/b", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/http/a/{a}/{b}/m", ruleData, TrieCacheTypeEnum.RULE);
        shenyuAntPathTrie.putNode("/http/a/{c}/{d}/n", ruleData, TrieCacheTypeEnum.RULE);
        Assertions.assertEquals("/aa/bb/**/n/b", shenyuAntPathTrie.match("/aa/bb/dd/uu/n/b", "2").getFullPath());
    }
    
}
