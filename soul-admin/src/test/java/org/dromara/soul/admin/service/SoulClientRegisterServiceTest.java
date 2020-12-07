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

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.dto.SpringCloudRegisterDTO;
import org.dromara.soul.admin.dto.SpringMvcRegisterDTO;
import org.dromara.soul.admin.entity.MetaDataDO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.mapper.MetaDataMapper;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.service.impl.SoulClientRegisterServiceImpl;
import org.dromara.soul.admin.service.impl.UpstreamCheckService;
import org.dromara.soul.admin.utils.SoulResultMessage;
import org.dromara.soul.common.dto.SelectorData;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ApplicationEventPublisher;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

/**
 * Test cases for SoulClientRegisterService.
 *
 * @author sakiila
 */
@RunWith(MockitoJUnitRunner.class)
public final class SoulClientRegisterServiceTest {

    @Mock
    private SoulClientRegisterServiceImpl soulClientRegisterService;

    @Mock
    private MetaDataMapper metaDataMapper;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private SelectorService selectorService;

    @Mock
    private RuleService ruleService;

    @Mock
    private RuleMapper ruleMapper;

    @Mock
    private UpstreamCheckService upstreamCheckService;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Before
    public void setUp() {
        soulClientRegisterService = new SoulClientRegisterServiceImpl(metaDataMapper, eventPublisher, selectorService,
                ruleService, ruleMapper, upstreamCheckService, selectorMapper, pluginMapper);
    }

    @Test
    public void testRegisterSpringMvc() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        SelectorDO selectorDO = buildSelectorDO();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        SelectorData selectorData = buildSelectorData();
        given(selectorService.buildByName(any())).willReturn(selectorData);
        given(selectorMapper.updateSelective(any())).willReturn(1);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        SpringMvcRegisterDTO dto = buildSpringMvcRegisterDTO();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringMvc(dto));
    }

    @Test
    public void testRegisterSpringCloud() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        SelectorDO selectorDO = buildSelectorDO();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        SpringCloudRegisterDTO dto = buildCloudRegisterDTO();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringCloud(dto));
    }

    @Test
    public void testRegisterDubbo() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.update(any())).willReturn(1);
        SelectorDO selectorDO = buildSelectorDO();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        MetaDataDTO dto = buildMetaDataD();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerDubbo(dto));
    }

    @Test
    public void testRegisterSofa() {
        MetaDataDTO dto = buildMetaDataD();
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        assertEquals("you path already exist!", soulClientRegisterService.registerSofa(dto));
    }

    private SpringMvcRegisterDTO buildSpringMvcRegisterDTO() {
        SpringMvcRegisterDTO springMvcRegisterDTO = new SpringMvcRegisterDTO();
        springMvcRegisterDTO.setAppName("appName1");
        springMvcRegisterDTO.setContext("content1");
        springMvcRegisterDTO.setPath("path1");
        springMvcRegisterDTO.setPathDesc("pathDesc1");
        springMvcRegisterDTO.setRpcType("rpcType1");
        springMvcRegisterDTO.setHost("localhost1");
        springMvcRegisterDTO.setPort(1234);
        springMvcRegisterDTO.setRuleName("ruleName1");
        springMvcRegisterDTO.setEnabled(true);
        springMvcRegisterDTO.setRegisterMetaData(true);
        return springMvcRegisterDTO;
    }

    private MetaDataDO buildMetaDataDO() {
        MetaDataDO metaDataDO = new MetaDataDO();
        metaDataDO.setAppName("appNameMetaData");
        metaDataDO.setPath("pathMetaData");
        metaDataDO.setPathDesc("pathDescMetaData");
        metaDataDO.setRpcType("rpcTypeMetaData");
        metaDataDO.setServiceName("serviceNameMetaData");
        metaDataDO.setMethodName("methodNameMetaData");
        metaDataDO.setParameterTypes("parameterTypesMetaData");
        metaDataDO.setRpcExt("rpcExtMetaData");
        metaDataDO.setEnabled(true);
        return metaDataDO;
    }

    private SelectorDO buildSelectorDO() {
        return new SelectorDO();
    }

    private SelectorData buildSelectorData() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId("5");
        selectorData.setPluginId("pluginId");
        selectorData.setName("name");
        selectorData.setMatchMode(0);
        selectorData.setType(1);
        selectorData.setSort(1);
        selectorData.setEnabled(true);
        selectorData.setLoged(true);
        selectorData.setContinued(false);
        selectorData.setHandle("handle");
        return selectorData;
    }

    private RuleDO buildRuleDO() {
        RuleDO ruleDO = new RuleDO();
        ruleDO.setId("5");
        ruleDO.setSelectorId("selectorId");
        ruleDO.setMatchMode(0);
        ruleDO.setName("name");
        ruleDO.setLoged(true);
        ruleDO.setSort(1);
        ruleDO.setHandle("handle");
        return ruleDO;
    }

    private SpringCloudRegisterDTO buildCloudRegisterDTO() {
        SpringCloudRegisterDTO springCloudRegisterDTO = new SpringCloudRegisterDTO();
        springCloudRegisterDTO.setAppName("appName2");
        springCloudRegisterDTO.setContext("content2");
        springCloudRegisterDTO.setPath("path2");
        springCloudRegisterDTO.setPathDesc("pathDesc2");
        springCloudRegisterDTO.setRpcType("rpcType2");
        springCloudRegisterDTO.setRuleName("ruleName2");
        springCloudRegisterDTO.setEnabled(false);
        return springCloudRegisterDTO;
    }

    private MetaDataDTO buildMetaDataD() {
        MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setId("6");
        metaDataDTO.setAppName("appName3");
        metaDataDTO.setContextPath("content3");
        metaDataDTO.setPath("path3");
        metaDataDTO.setRuleName("ruleName3");
        metaDataDTO.setPathDesc("pathDesc3");
        metaDataDTO.setRpcType("rpcType3");
        metaDataDTO.setServiceName("serviceName3");
        metaDataDTO.setMethodName("methodName3");
        metaDataDTO.setParameterTypes("parameterTypes3");
        metaDataDTO.setRpcExt("rpcExt3");
        metaDataDTO.setEnabled(false);
        return metaDataDTO;
    }
}
