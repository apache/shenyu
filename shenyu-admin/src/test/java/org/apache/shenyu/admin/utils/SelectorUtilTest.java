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

package org.apache.shenyu.admin.utils;

import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.SelectorTypeEnum;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

/**
 * Test case for {@link SelectorUtil}.
 */
public class SelectorUtilTest {

    @Test
    public void testBuildDivideUpstreamWithDividePluginAndValidHandle() {
        // Test case: plugin is divide and handle is not blank - should parse JSON
        SelectorDO selectorDO = new SelectorDO();
        String jsonHandle = "[{\"upstreamHost\":\"localhost\",\"protocol\":\"http\",\"upstreamUrl\":\"http://localhost:8080\",\"weight\":50,\"warmup\":100}]";
        selectorDO.setHandle(jsonHandle);

        List<DivideUpstream> result = SelectorUtil.buildDivideUpstream(selectorDO, PluginEnum.DIVIDE.getName());

        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        Assert.assertEquals("localhost", result.get(0).getUpstreamHost());
        Assert.assertEquals("http", result.get(0).getProtocol());
        Assert.assertEquals("http://localhost:8080", result.get(0).getUpstreamUrl());
        Assert.assertEquals(50, result.get(0).getWeight());
        Assert.assertEquals(100, result.get(0).getWarmup());
    }

    @Test
    public void testBuildDivideUpstreamWithDividePluginAndEmptyHandle() {
        // Test case: plugin is divide but handle is empty - should return empty list
        SelectorDO selectorDO = new SelectorDO();
        selectorDO.setHandle("");

        List<DivideUpstream> result = SelectorUtil.buildDivideUpstream(selectorDO, PluginEnum.DIVIDE.getName());

        Assert.assertNotNull(result);
        Assert.assertTrue(result.isEmpty());
    }

    @Test
    public void testBuildDivideUpstreamWithDividePluginAndNullHandle() {
        // Test case: plugin is divide but handle is null - should return empty list
        SelectorDO selectorDO = new SelectorDO();
        selectorDO.setHandle(null);

        List<DivideUpstream> result = SelectorUtil.buildDivideUpstream(selectorDO, PluginEnum.DIVIDE.getName());

        Assert.assertNotNull(result);
        Assert.assertTrue(result.isEmpty());
    }

    @Test
    public void testBuildDivideUpstreamWithNonDividePlugin() {
        // Test case: plugin is not divide - should return empty list
        SelectorDO selectorDO = new SelectorDO();
        selectorDO.setHandle("[{\"upstreamHost\":\"localhost\"}]");

        List<DivideUpstream> result = SelectorUtil.buildDivideUpstream(selectorDO, "other-plugin");

        Assert.assertNotNull(result);
        Assert.assertTrue(result.isEmpty());
    }

    @Test(expected = NullPointerException.class)
    public void testBuildDivideUpstreamWithNullSelectorDO() {
        // Test case: selectorDO is null - should throw NullPointerException
        SelectorUtil.buildDivideUpstream(null, PluginEnum.DIVIDE.getName());
    }

    @Test
    public void testBuildDivideUpstreamWithNullPluginName() {
        // Test case: pluginName is null - should return empty list
        SelectorDO selectorDO = new SelectorDO();
        selectorDO.setHandle("[{\"upstreamHost\":\"localhost\"}]");

        List<DivideUpstream> result = SelectorUtil.buildDivideUpstream(selectorDO, null);

        Assert.assertNotNull(result);
        Assert.assertTrue(result.isEmpty());
    }

    @Test
    public void testBuildDivideUpstreamWithMultipleUpstreams() {
        // Test case: multiple upstreams in JSON
        SelectorDO selectorDO = new SelectorDO();
        String jsonHandle = "[{\"upstreamHost\":\"host1\",\"protocol\":\"http\",\"upstreamUrl\":\"http://host1:8080\",\"weight\":30,\"warmup\":50},"
                          + "{\"upstreamHost\":\"host2\",\"protocol\":\"http\",\"upstreamUrl\":\"http://host2:8080\",\"weight\":70,\"warmup\":75}]";
        selectorDO.setHandle(jsonHandle);

        List<DivideUpstream> result = SelectorUtil.buildDivideUpstream(selectorDO, PluginEnum.DIVIDE.getName());

        Assert.assertNotNull(result);
        Assert.assertEquals(2, result.size());
        Assert.assertEquals("host1", result.get(0).getUpstreamHost());
        Assert.assertEquals("host2", result.get(1).getUpstreamHost());
    }

    @Test
    public void testBuildSelectorDTO() {
        // Test normal case
        String contextPath = "/test";
        String pluginId = "test-plugin-id";

        SelectorDTO result = SelectorUtil.buildSelectorDTO(contextPath, pluginId);

        Assert.assertNotNull(result);
        Assert.assertEquals(contextPath, result.getName());
        Assert.assertEquals(pluginId, result.getPluginId());
        Assert.assertEquals(Long.valueOf(SelectorTypeEnum.CUSTOM_FLOW.getCode()), Long.valueOf(result.getType()));
        Assert.assertEquals(Long.valueOf(MatchModeEnum.AND.getCode()), Long.valueOf(result.getMatchMode()));
        Assert.assertTrue(result.getEnabled());
        Assert.assertTrue(result.getLoged());
        Assert.assertTrue(result.getContinued());
        Assert.assertFalse(result.getMatchRestful());
        Assert.assertEquals(Long.valueOf(1), Long.valueOf(result.getSort()));

        // Check selector conditions
        Assert.assertNotNull(result.getSelectorConditions());
        Assert.assertEquals(1, result.getSelectorConditions().size());
        SelectorConditionDTO condition = result.getSelectorConditions().get(0);
        Assert.assertEquals(ParamTypeEnum.URI.getName(), condition.getParamType());
        Assert.assertEquals("/", condition.getParamName());
        Assert.assertEquals(OperatorEnum.STARTS_WITH.getAlias(), condition.getOperator());
        Assert.assertEquals(contextPath + Constants.PATH_SEPARATOR, condition.getParamValue());
    }

    @Test
    public void testBuildSelectorDTOWithNullInputs() {
        // Test with null inputs
        SelectorDTO result = SelectorUtil.buildSelectorDTO(null, null);

        Assert.assertNotNull(result);
        Assert.assertNull(result.getName());
        Assert.assertNull(result.getPluginId());
        Assert.assertNotNull(result.getSelectorConditions());
    }

    @Test
    public void testBuildSelectorDTOWithEmptyInputs() {
        // Test with empty inputs
        SelectorDTO result = SelectorUtil.buildSelectorDTO("", "");

        Assert.assertNotNull(result);
        Assert.assertEquals("", result.getName());
        Assert.assertEquals("", result.getPluginId());
        Assert.assertNotNull(result.getSelectorConditions());
        Assert.assertEquals(1, result.getSelectorConditions().size());
        Assert.assertEquals(Constants.PATH_SEPARATOR, result.getSelectorConditions().get(0).getParamValue());
    }

    @Test
    public void testBuildDefaultSelectorDTO() {
        // Test normal case
        String name = "test-selector";

        SelectorDTO result = SelectorUtil.buildDefaultSelectorDTO(name);

        Assert.assertNotNull(result);
        Assert.assertEquals(name, result.getName());
        Assert.assertEquals(Long.valueOf(SelectorTypeEnum.CUSTOM_FLOW.getCode()), Long.valueOf(result.getType()));
        Assert.assertEquals(Long.valueOf(MatchModeEnum.AND.getCode()), Long.valueOf(result.getMatchMode()));
        Assert.assertTrue(result.getEnabled());
        Assert.assertTrue(result.getLoged());
        Assert.assertTrue(result.getContinued());
        Assert.assertFalse(result.getMatchRestful());
        Assert.assertEquals(Long.valueOf(1), Long.valueOf(result.getSort()));
    }

    @Test
    public void testBuildDefaultSelectorDTOWithNullName() {
        // Test with null name
        SelectorDTO result = SelectorUtil.buildDefaultSelectorDTO(null);

        Assert.assertNotNull(result);
        Assert.assertNull(result.getName());
        Assert.assertEquals(Long.valueOf(SelectorTypeEnum.CUSTOM_FLOW.getCode()), Long.valueOf(result.getType()));
        Assert.assertEquals(Long.valueOf(MatchModeEnum.AND.getCode()), Long.valueOf(result.getMatchMode()));
        Assert.assertTrue(result.getEnabled());
        Assert.assertTrue(result.getLoged());
        Assert.assertTrue(result.getContinued());
        Assert.assertFalse(result.getMatchRestful());
        Assert.assertEquals(Long.valueOf(1), Long.valueOf(result.getSort()));
    }

    @Test
    public void testBuildDefaultSelectorDTOWithEmptyName() {
        // Test with empty name
        SelectorDTO result = SelectorUtil.buildDefaultSelectorDTO("");

        Assert.assertNotNull(result);
        Assert.assertEquals("", result.getName());
        Assert.assertEquals(Long.valueOf(SelectorTypeEnum.CUSTOM_FLOW.getCode()), Long.valueOf(result.getType()));
        Assert.assertEquals(Long.valueOf(MatchModeEnum.AND.getCode()), Long.valueOf(result.getMatchMode()));
        Assert.assertTrue(result.getEnabled());
        Assert.assertTrue(result.getLoged());
        Assert.assertTrue(result.getContinued());
        Assert.assertFalse(result.getMatchRestful());
        Assert.assertEquals(Long.valueOf(1), Long.valueOf(result.getSort()));
    }

    @Test
    public void testBuildDefaultSelectorConditionDTO() {
        // Test normal case
        String contextPath = "/test";

        List<SelectorConditionDTO> result = SelectorUtil.buildDefaultSelectorConditionDTO(contextPath);

        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        SelectorConditionDTO condition = result.get(0);
        Assert.assertEquals(ParamTypeEnum.URI.getName(), condition.getParamType());
        Assert.assertEquals("/", condition.getParamName());
        Assert.assertEquals(OperatorEnum.STARTS_WITH.getAlias(), condition.getOperator());
        Assert.assertEquals(contextPath + Constants.PATH_SEPARATOR, condition.getParamValue());
    }

    @Test
    public void testBuildDefaultSelectorConditionDTOWithNullContextPath() {
        // Test with null context path - will concatenate with PATH_SEPARATOR
        List<SelectorConditionDTO> result = SelectorUtil.buildDefaultSelectorConditionDTO(null);

        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        SelectorConditionDTO condition = result.get(0);
        Assert.assertEquals(ParamTypeEnum.URI.getName(), condition.getParamType());
        Assert.assertEquals("/", condition.getParamName());
        Assert.assertEquals(OperatorEnum.STARTS_WITH.getAlias(), condition.getOperator());
        // null + PATH_SEPARATOR results in "null/"
        Assert.assertEquals("null" + Constants.PATH_SEPARATOR, condition.getParamValue());
    }

    @Test
    public void testBuildDefaultSelectorConditionDTOWithEmptyContextPath() {
        // Test with empty context path
        List<SelectorConditionDTO> result = SelectorUtil.buildDefaultSelectorConditionDTO("");

        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        SelectorConditionDTO condition = result.get(0);
        Assert.assertEquals(ParamTypeEnum.URI.getName(), condition.getParamType());
        Assert.assertEquals("/", condition.getParamName());
        Assert.assertEquals(OperatorEnum.STARTS_WITH.getAlias(), condition.getOperator());
        Assert.assertEquals(Constants.PATH_SEPARATOR, condition.getParamValue());
    }

    @Test
    public void testBuildDivideUpstreamWithInvalidJson() {
        // Test case: plugin is divide but handle contains invalid JSON - should throw exception
        SelectorDO selectorDO = new SelectorDO();
        selectorDO.setHandle("invalid json");

        Assert.assertThrows(RuntimeException.class,
                () -> SelectorUtil.buildDivideUpstream(selectorDO, PluginEnum.DIVIDE.getName()));
    }
}




































