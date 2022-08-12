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

package org.apache.shenyu.web.controller;

import com.google.common.collect.Lists;
import com.google.common.reflect.TypeToken;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.rule.impl.DivideRuleHandle;
import org.apache.shenyu.common.enums.LoadBalanceEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.CommonPluginDataSubscriber;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.web.controller.LocalPluginController.SelectorRuleData;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.mock;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


/**
 * The type Plugin controller Test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class LocalPluginControllerTest {

    private PluginDataSubscriber subscriber;

    private MockMvc mockMvc;

    private BaseDataCache baseDataCache;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @BeforeEach
    public void setup() {
        ArrayList<PluginDataHandler> pluginDataHandlerList = Lists.newArrayList();
        subscriber = new CommonPluginDataSubscriber(pluginDataHandlerList, eventPublisher);
        mockMvc = MockMvcBuilders.standaloneSetup(new LocalPluginController(subscriber))
                .build();
        baseDataCache = BaseDataCache.getInstance();
    }

    @Test
    public void testCleanAll() throws Exception {
        final String testCleanPluginName = "testCleanPluginName";
        subscribePluginForTest(testCleanPluginName);
        subscribeSelectorForTest(testCleanPluginName);
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/shenyu/cleanAll")
                .contentType(MediaType.APPLICATION_JSON))
                .andReturn()
                .getResponse();
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
        assertThat(baseDataCache.obtainSelectorData(testCleanPluginName)).isNull();
        assertThat(baseDataCache.obtainPluginData(testCleanPluginName)).isNull();
    }

    private void subscribeSelectorForTest(final String name) {
        subscriber.onSelectorSubscribe(SelectorData.builder()
                .id(name)
                .name(name)
                .pluginName(name)
                .build());
    }

    @Test
    public void testSaveOrUpdate() throws Exception {
        final String testPluginName = "testSavePluginName";
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.post("/shenyu/plugin/saveOrUpdate")
                .content(GsonUtils.getGson().toJson(createTestCleanPlugin(testPluginName)))
                .contentType(MediaType.APPLICATION_JSON))
                .andReturn()
                .getResponse();
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
        assertThat(baseDataCache.obtainPluginData(testPluginName)).isNotNull();
        assertThat(baseDataCache.obtainPluginData(testPluginName).getName()).isEqualTo(testPluginName);
    }

    @Test
    public void testCleanPlugin() throws Exception {
        final String testCleanPluginName = "testCleanPluginName";
        subscribePluginForTest(testCleanPluginName);
        subscribeSelectorForTest(testCleanPluginName);
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/shenyu/cleanPlugin")
                .param("name", testCleanPluginName))
                .andReturn()
                .getResponse();
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
        assertThat(baseDataCache.obtainPluginData(testCleanPluginName)).isNull();
        assertThat(baseDataCache.obtainSelectorData(testCleanPluginName)).isNull();
    }

    @Test
    public void testDelete() throws Exception {
        final String testPluginName = "testDeletePluginName";
        final PluginData pluginData = new PluginData();
        pluginData.setName(testPluginName);
        subscriber.onSubscribe(pluginData);
        assertThat(baseDataCache.obtainPluginData(testPluginName)).isNotNull();
        final MockHttpServletResponse response = this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/delete")
                        .param("name", testPluginName))
                .andExpect(status().isOk())
                .andReturn()
                .getResponse();
        assertThat(response.getStatus()).isEqualTo(HttpStatus.OK.value());
        assertThat(baseDataCache.obtainPluginData(testPluginName)).isNull();
    }

    @Test
    public void testDeleteAll() throws Exception {
        final String[] testPluginName = {"testDeleteAllPluginName", "testDeleteAllPluginName2"};
        Arrays.stream(testPluginName).map(s ->
                new PluginData("id", s, null, null, null))
                .forEach(subscriber::onSubscribe);
        Arrays.stream(testPluginName)
                .forEach(s -> assertThat(baseDataCache.obtainPluginData(s)).isNotNull());
        this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/deleteAll"))
                .andExpect(status().isOk())
                .andReturn();
        Arrays.stream(testPluginName)
                .forEach(s -> assertThat(baseDataCache.obtainPluginData(s)).isNull());
    }

    @Test
    public void testFindListRule() throws Exception {
        final String testSelectorId = "testFindListRuleSelectorId";
        final String testId = "testId";
        subscribeRuleForTest(testSelectorId, testId);
        final Object result = this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/rule/findList")
                        .param("selectorId", testSelectorId)
                        .param("id", testId))
                .andExpect(status().isOk())
                .andReturn()
                .getAsyncResult();
        assertThat(result).isInstanceOf(String.class);
        @SuppressWarnings("UnstableApiUsage")
        final List<RuleData> list = GsonUtils.getGson().fromJson((String) result, new TypeToken<List<RuleData>>() {
        }.getType());
        final List<String> ruleSelectorIds = list.stream().map(RuleData::getSelectorId).collect(Collectors.toList());
        final List<String> ruleIds = list.stream().map(RuleData::getId).collect(Collectors.toList());
        assertThat(ruleSelectorIds).contains(testSelectorId);
        assertThat(ruleIds).contains(testId);

        final Object result2 = this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/rule/findList")
                        .param("selectorId", testSelectorId))
                .andExpect(status().isOk())
                .andReturn()
                .getAsyncResult();
        assertThat(result).isInstanceOf(String.class);
        @SuppressWarnings("UnstableApiUsage")
        final List<RuleData> list2 = GsonUtils.getGson().fromJson((String) result2, new TypeToken<List<RuleData>>() {
        }.getType());
        final List<String> ruleSelectorIds2 = list2.stream().map(RuleData::getSelectorId).collect(Collectors.toList());
        assertThat(ruleSelectorIds2).contains(testSelectorId);

        final Object resultError = this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/rule/findList")
                        .param("selectorId", "testSelectorIdError")
                        .param("id", testId))
                .andExpect(status().isOk())
                .andReturn()
                .getAsyncResult();
        assertThat(resultError).isEqualTo("Error: can not find rule data by selector id :testSelectorIdError");
    }

    @Test
    public void testDeleteSelector() throws Exception {
        final String selectorPluginName = "testSaveSelector";
        final String testSelectorId = "id";
        final SelectorData selectorData = new SelectorData();
        selectorData.setId(testSelectorId);
        selectorData.setPluginName(selectorPluginName);
        subscriber.onSelectorSubscribe(selectorData);
        this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/selector/delete")
                        .param("pluginName", selectorPluginName)
                        .param("id", testSelectorId))
                .andExpect(status().isOk())
                .andReturn();
        assertThat(baseDataCache.obtainSelectorData(selectorPluginName)).isEmpty();
    }

    @Test
    public void testFindListSelector() throws Exception {
        final String selectorPluginName = "testFindListSelector";
        final String testFindListSelectorId = "testFindListSelectorId";
        final SelectorData selectorData = new SelectorData();
        selectorData.setPluginName(selectorPluginName);
        selectorData.setId(testFindListSelectorId);
        subscriber.onSelectorSubscribe(selectorData);
        final Object result = this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/selector/findList")
                        .param("pluginName", selectorPluginName))
                .andExpect(status().isOk())
                .andReturn()
                .getAsyncResult();
        assertThat(result).isInstanceOf(String.class);
        @SuppressWarnings("UnstableApiUsage")
        final List<SelectorData> list = GsonUtils.getGson().fromJson((String) result, new TypeToken<List<SelectorData>>() {
        }.getType());
        final List<String> idList = list.stream().map(SelectorData::getPluginName).collect(Collectors.toList());
        assertThat(idList).contains(selectorPluginName);

        final Object resultError1 = this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/selector/findList")
                        .param("pluginName", "testFindListSelectorError"))
                .andExpect(status().isOk())
                .andReturn()
                .getAsyncResult();
        assertThat(resultError1).isEqualTo("Error: can not find selector data by pluginName :testFindListSelectorError");

        final Object result2 = this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/selector/findList")
                        .param("id", "testFindListSelectorId")
                        .param("pluginName", selectorPluginName))
                .andExpect(status().isOk())
                .andReturn()
                .getAsyncResult();
        final List<SelectorData> list2 = GsonUtils.getGson().fromJson((String) result2, new TypeToken<List<SelectorData>>() {
        }.getType());
        final List<String> selectorDataIds = list2.stream().map(SelectorData::getId).collect(Collectors.toList());
        assertThat(selectorDataIds).contains(testFindListSelectorId);
    }

    @Test
    public void testFindByName() throws Exception {
        final String pluginName = "testFindByNamePlugin";
        subscribePluginForTest(pluginName);
        final Object result = this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/findByName")
                        .param("name", pluginName))
                .andExpect(status().isOk())
                .andReturn()
                .getAsyncResult();
        assertThat(result).isInstanceOf(String.class);
        final PluginData pluginData = GsonUtils.getGson().fromJson((String) result, PluginData.class);
        assertThat(pluginData.getName()).isEqualTo(pluginName);

        final Object resultErr = this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/findByName")
                        .param("name", "testFindByNamePluginError"))
                .andExpect(status().isOk())
                .andReturn()
                .getAsyncResult();
        assertThat(resultErr).isEqualTo("can not find this plugin : testFindByNamePluginError");
    }

    @Test
    public void saveSelector() throws Exception {
        final String selectorPluginName = "testSaveSelector";
        final SelectorData selectorData = new SelectorData();
        selectorData.setPluginName(selectorPluginName);
        final String json = GsonUtils.getGson().toJson(selectorData);
        this.mockMvc
                .perform(MockMvcRequestBuilders.post("/shenyu/plugin/selector/saveOrUpdate")
                        .content(json)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        assertThat(baseDataCache.obtainSelectorData(selectorPluginName)).isNotNull();

        final String selectorPluginNameError = "testSaveSelectorError";
        final SelectorData selectorDataError = new SelectorData();
        selectorData.setPluginName(selectorPluginNameError);
        final String jsonError = GsonUtils.getGson().toJson(selectorDataError);
        final Object resultErr = this.mockMvc
                .perform(MockMvcRequestBuilders.post("/shenyu/plugin/selector/saveOrUpdate")
                        .content(jsonError)
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn()
                .getAsyncResult();
        assertThat(resultErr).isEqualTo("Error: please add pluginName!");
    }

    @Test
    public void testSelectorAndRule() throws Exception {
        final String testPluginName = "testPluginName";
        final SelectorRuleData selectorRuleData = new SelectorRuleData();
        selectorRuleData.setPluginName(testPluginName);
        this.mockMvc
                .perform(MockMvcRequestBuilders.post("/shenyu/plugin/selectorAndRule")
                        .content(GsonUtils.getGson().toJson(selectorRuleData))
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        assertThat(baseDataCache.obtainSelectorData(testPluginName)).isNotNull();
    }

    @Test
    public void testSaveRule() throws Exception {
        final String testSelectorId = "testSaveRuleId";
        final String testRuleId = "ruleId";
        final RuleData ruleData = createRuleData(testSelectorId, testRuleId);
        this.mockMvc
                .perform(MockMvcRequestBuilders.post("/shenyu/plugin/rule/saveOrUpdate")
                .content(GsonUtils.getGson().toJson(ruleData))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();

        final List<RuleData> selectorId = baseDataCache.obtainRuleData(testSelectorId);
        assertThat(selectorId.get(0).getSelectorId()).isEqualTo(testSelectorId);

        Object result = this.mockMvc
                .perform(MockMvcRequestBuilders.post("/shenyu/plugin/rule/saveOrUpdate")
                        .content("{}")
                        .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn()
                .getAsyncResult();
        assertThat(result).isEqualTo("Error: please add selectorId!");
    }

    @Test
    public void testDeleteRule() throws Exception {
        final String testSelectorId = "testSaveRuleId";
        final String testRuleId = "ruleId";
        final String pluginName = "pluginName";
        final RuleData ruleData = createRuleData(testSelectorId, testRuleId);
        subscriber.onRuleSubscribe(ruleData);
        this.mockMvc
                .perform(MockMvcRequestBuilders.get("/shenyu/plugin/rule/delete")
                .param("selectorId", ruleData.getSelectorId())
                .param("id", ruleData.getId())
                .param("pluginName", pluginName)
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();

        final List<RuleData> selectorId = baseDataCache.obtainRuleData(testSelectorId);
        Assertions.assertTrue(selectorId.isEmpty());
    }

    @Test
    public void testSelectorAndRules() throws Exception {
        final LocalPluginController.SelectorRulesData selectorRulesData = new LocalPluginController.SelectorRulesData();
        selectorRulesData.setPluginName("pluginName");
        selectorRulesData.setSelectorName("selectorName");
        selectorRulesData.setSelectorHandler("{}");
        selectorRulesData.setMatchMode(0);
        LocalPluginController.RuleLocalData ruleLocalData = new LocalPluginController.RuleLocalData();
        ruleLocalData.setRuleName("ruleName");
        ruleLocalData.setRuleHandler("{}");
        ruleLocalData.setMatchMode(0);
        ruleLocalData.setConditionDataList(Collections.emptyList());
        selectorRulesData.setRuleDataList(Collections.singletonList(ruleLocalData));
        selectorRulesData.setConditionDataList(Collections.emptyList());
        this.mockMvc
                .perform(MockMvcRequestBuilders.post("/shenyu/plugin/selectorAndRules")
                .content(GsonUtils.getGson().toJson(selectorRulesData))
                .contentType(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
        Assertions.assertNotNull(baseDataCache.obtainSelectorData(selectorRulesData.getPluginName()));
        Assertions.assertEquals(selectorRulesData.getSelectorName(), "selectorName");
    }

    private void subscribeRuleForTest(final String testSelectorId, final String testId) {
        final RuleData ruleData = createRuleData(testSelectorId, testId);
        subscriber.onRuleSubscribe(ruleData);
    }

    private RuleData createRuleData(final String testSelectorId, final String testId) {
        return RuleData.builder()
                .selectorId(testSelectorId)
                .pluginName("testPluginName")
                .id(testId)
                .build();
    }

    private void subscribePluginForTest(final String pluginName) {
        PluginData pluginData = createTestCleanPlugin(pluginName);
        subscriber.onSubscribe(pluginData);
    }

    private PluginData createTestCleanPlugin(final String pluginName) {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(true);
        pluginData.setId("2");
        pluginData.setName(pluginName);
        pluginData.setRole("1");
        return pluginData;
    }

    @Test
    public void testSelectorRuleData() {
        List<DivideUpstream> collect = Stream.of(1).map(weight -> DivideUpstream.builder()
                .upstreamUrl("127.0.0.1:8089")
                .build())
                .collect(Collectors.toList());
        SelectorRuleData selectorRuleData = new SelectorRuleData();
        selectorRuleData.setPluginName(PluginEnum.DIVIDE.getName());
        selectorRuleData.setSelectorHandler(JsonUtils.toJson(collect));
        selectorRuleData.setSelectorName("selectorName");
        List<ConditionData> dataList = Stream.of(1).map(weight -> {
            ConditionData data = new ConditionData();
            data.setParamType(ParamTypeEnum.URI.getName());
            data.setOperator(OperatorEnum.MATCH.getAlias());
            data.setParamValue("/**");
            return data;
        }).collect(Collectors.toList());
        selectorRuleData.setConditionDataList(dataList);
        DivideRuleHandle divideRuleHandle = new DivideRuleHandle();
        divideRuleHandle.setLoadBalance(LoadBalanceEnum.RANDOM.getName());
        selectorRuleData.setRuleHandler(JsonUtils.toJson(divideRuleHandle));
        Assertions.assertEquals(selectorRuleData.getSelectorName(), "selectorName");
    }

    @Test
    public void testBuildDefaultSelectorData() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Method buildDefaultSelectorData = LocalPluginController.class.getDeclaredMethod("buildDefaultSelectorData", SelectorData.class);
        buildDefaultSelectorData.setAccessible(true);
        LocalPluginController localPluginController = mock(LocalPluginController.class);
        SelectorData selectorData = SelectorData.builder().id("id").name("name").sort(1)
                .enabled(true)
                .logged(true)
                .build();
        buildDefaultSelectorData.invoke(localPluginController, selectorData);
    }

    @Test
    public void testBuildDefaultRuleData() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Method buildDefaultRuleData = LocalPluginController.class.getDeclaredMethod("buildDefaultRuleData", RuleData.class);
        buildDefaultRuleData.setAccessible(true);
        LocalPluginController localPluginController = mock(LocalPluginController.class);

        buildDefaultRuleData.invoke(localPluginController, RuleData.builder().sort(1).enabled(true).loged(true).build());
    }
}
