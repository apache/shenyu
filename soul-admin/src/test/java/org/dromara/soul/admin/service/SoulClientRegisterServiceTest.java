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
import org.dromara.soul.admin.dto.PluginDTO;
import org.dromara.soul.admin.dto.SpringCloudRegisterDTO;
import org.dromara.soul.admin.dto.SpringMvcRegisterDTO;
import org.dromara.soul.admin.entity.MetaDataDO;
import org.dromara.soul.admin.entity.PluginDO;
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
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.h2.engine.Role;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Objects;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

/**
 * Test cases for SoulClientRegisterService.
 *
 * @author sakiila
 */
@RunWith(MockitoJUnitRunner.Silent.class)
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
    public void testRegisterSpringMvcNotExist() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        SelectorDO selectorDO = buildSelectorDO();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        SelectorData selectorData = buildSelectorData();
        given(selectorService.buildByName(any())).willReturn(selectorData);
        given(selectorMapper.updateSelective(any())).willReturn(1);
        given(ruleMapper.findByName(any())).willReturn(null);
        SpringMvcRegisterDTO dto = buildSpringMvcRegisterDTO();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringMvc(dto));
    }

    @Test
    public void testRegisterSpringMvcMetaDataFindByPathIsNull() {
        given(metaDataMapper.findByPath(any())).willReturn(null);
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
    public void testRegisterSpringMvcSelectorFindByNameIsNull() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(selectorService.findByName(any())).willReturn(null);
        SelectorData selectorData = buildSelectorData();
        given(selectorService.buildByName(any())).willReturn(selectorData);
        given(selectorMapper.updateSelective(any())).willReturn(1);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        final PluginDTO pluginDTO = buildPluginDTO();
        final PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        SpringMvcRegisterDTO dto = buildSpringMvcRegisterDTO();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringMvc(dto));

        dto = buildSpringMvcRegisterDTO(RpcTypeEnum.DUBBO);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringMvc(dto));

        dto = buildSpringMvcRegisterDTO(RpcTypeEnum.SOFA);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringMvc(dto));

        dto = buildSpringMvcRegisterDTO(RpcTypeEnum.SPRING_CLOUD);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringMvc(dto));

        dto = buildSpringMvcRegisterDTO(RpcTypeEnum.TARS);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringMvc(dto));
    }

    @Test
    public void testRegisterSpringMvcWithHandle() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        SelectorDO selectorDO = SelectorDO.builder()
                .handle("[{\"upstreamHost\":\"localhost:8080\",\"protocol\":\"http\",\"upstreamUrl\":\"/upstreamUrl\",\"weight\":\"10\"}]")
                .build();
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
    public void testRegisterSpringMvcWithHandleExistUpstream() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        SelectorDO selectorDO = SelectorDO.builder()
                .handle("[{\"upstreamHost\":\"localhost:8080\",\"protocol\":\"http\",\"upstreamUrl\":\"localhost1:1234\",\"weight\":\"10\"}]")
                .build();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        SelectorData selectorData = buildSelectorData();
        given(selectorService.buildByName(any())).willReturn(selectorData);
        given(selectorMapper.updateSelective(any())).willReturn(1);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        SpringMvcRegisterDTO dto = buildSpringMvcRegisterDTO();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringMvc(dto));
    }

    private PluginDTO buildPluginDTO() {
        final PluginDTO pluginDTO = new PluginDTO();
        pluginDTO.setEnabled(true);
        pluginDTO.setConfig("test-config");
        pluginDTO.setRole(Role.USER);
        pluginDTO.setName("test-name");
        return pluginDTO;
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
    public void testRegisterSpringCloudWithEmptyMetaData() {
        given(metaDataMapper.findByPath(any())).willReturn(null);
        SelectorDO selectorDO = buildSelectorDO();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        SpringCloudRegisterDTO dto = buildCloudRegisterDTO();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringCloud(dto));
    }

    @Test
    public void testRegisterSpringCloudWithEmptyRule() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        SelectorDO selectorDO = buildSelectorDO();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        given(ruleMapper.findByName(any())).willReturn(null);
        SpringCloudRegisterDTO dto = buildCloudRegisterDTO();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringCloud(dto));
    }

    @Test
    public void testRegisterSpringCloudWithEmptySelector() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(selectorService.findByName(any())).willReturn(null);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        final PluginDTO pluginDTO = buildPluginDTO();
        final PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);

        SpringCloudRegisterDTO dto = buildCloudRegisterDTO();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringCloud(dto));

        dto = buildCloudRegisterDTO(RpcTypeEnum.DUBBO);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringCloud(dto));

        dto = buildCloudRegisterDTO(RpcTypeEnum.SOFA);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringCloud(dto));

        dto = buildCloudRegisterDTO(RpcTypeEnum.SPRING_CLOUD);
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerSpringCloud(dto));

        dto = buildCloudRegisterDTO(RpcTypeEnum.TARS);
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
    public void testRegisterDubboNotExistMetaData() {
        given(metaDataMapper.findByPath(any())).willReturn(null);
        given(metaDataMapper.update(any())).willReturn(1);
        SelectorDO selectorDO = buildSelectorDO();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        MetaDataDTO dto = buildMetaDataD();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerDubbo(dto));
    }

    @Test
    public void testRegisterDubboWithEmptySelector() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.update(any())).willReturn(1);
        given(selectorService.findByName(any())).willReturn(null);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        final PluginDTO pluginDTO = buildPluginDTO();
        final PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        MetaDataDTO dto = buildMetaDataD();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerDubbo(dto));
    }

    @Test
    public void testRegisterDubboWithEmptyRule() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.update(any())).willReturn(1);
        given(selectorService.findByName(any())).willReturn(null);
        given(ruleMapper.findByName(any())).willReturn(null);
        final PluginDTO pluginDTO = buildPluginDTO();
        final PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        MetaDataDTO dto = buildMetaDataD();
        assertEquals(SoulResultMessage.SUCCESS, soulClientRegisterService.registerDubbo(dto));
    }

    @Test
    public void testRegisterSofaAlreadyExist() {
        final MetaDataDTO dto = buildMetaDataD();
        MetaDataDO metaDataDO = buildMetaDataDO();
        metaDataDO.setServiceName("serviceName33");
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        final PluginDTO pluginDTO = buildPluginDTO();
        final PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        assertEquals("you path already exist!", soulClientRegisterService.registerSofa(dto));
    }

    @Test
    public void testRegisterSofaNotExistMetaData() {
        final MetaDataDTO dto = buildMetaDataD();
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.findByServiceNameAndMethod(any(), any())).willReturn(null);
        final PluginDTO pluginDTO = buildPluginDTO();
        final PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        assertEquals("success", soulClientRegisterService.registerSofa(dto));
    }

    @Test
    public void testRegisterSofaWithExistMetaData() {
        final MetaDataDTO dto = buildMetaDataD();
        final MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.findByServiceNameAndMethod(any(), any())).willReturn(metaDataDO);
        final PluginDTO pluginDTO = buildPluginDTO();
        final PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        assertEquals("success", soulClientRegisterService.registerSofa(dto));
    }

    @Test
    public void testRegisterTarsAlreadyExist() {
        final MetaDataDTO dto = buildMetaDataD();
        MetaDataDO metaDataDO = buildMetaDataDO();
        metaDataDO.setServiceName("serviceName33");
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        final PluginDTO pluginDTO = buildPluginDTO();
        final PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        assertEquals("you path already exist!", soulClientRegisterService.registerTars(dto));
    }

    @Test
    public void testRegisterTarsNotExistMetaData() {
        final MetaDataDTO dto = buildMetaDataD();
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.findByServiceNameAndMethod(any(), any())).willReturn(null);
        final PluginDTO pluginDTO = buildPluginDTO();
        final PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        assertEquals("success", soulClientRegisterService.registerTars(dto));
    }

    @Test
    public void testRegisterTarsWithExistMetaData() {
        final MetaDataDTO dto = buildMetaDataD();
        final MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.findByServiceNameAndMethod(any(), any())).willReturn(metaDataDO);
        final PluginDTO pluginDTO = buildPluginDTO();
        final PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        assertEquals("success", soulClientRegisterService.registerTars(dto));
    }

    private SpringMvcRegisterDTO buildSpringMvcRegisterDTO() {
        return this.buildSpringMvcRegisterDTO(RpcTypeEnum.HTTP);
    }

    private SpringMvcRegisterDTO buildSpringMvcRegisterDTO(final RpcTypeEnum rpcTypeEnum) {
        SpringMvcRegisterDTO springMvcRegisterDTO = new SpringMvcRegisterDTO();
        springMvcRegisterDTO.setAppName("appName1");
        springMvcRegisterDTO.setContext("content1");
        springMvcRegisterDTO.setPath("path1");
        springMvcRegisterDTO.setPathDesc("pathDesc1");
        if (Objects.isNull(rpcTypeEnum)) {
            springMvcRegisterDTO.setRpcType("http");
        } else {
            springMvcRegisterDTO.setRpcType(rpcTypeEnum.getName());
        }
        springMvcRegisterDTO.setHost("localhost1");
        springMvcRegisterDTO.setPort(1234);
        springMvcRegisterDTO.setRuleName("ruleName1");
        springMvcRegisterDTO.setEnabled(true);
        springMvcRegisterDTO.setRegisterMetaData(true);
        return springMvcRegisterDTO;
    }

    private MetaDataDO buildMetaDataDO() {
        return MetaDataDO.builder()
                .appName("appNameMetaData")
                .path("pathMetaData")
                .pathDesc("pathDescMetaData")
                .rpcType("rpcTypeMetaData")
                .serviceName("serviceName3")
                .methodName("methodName3")
                .parameterTypes("parameterTypesMetaData")
                .rpcExt("rpcExtMetaData")
                .enabled(true)
                .build();
    }

    private SelectorDO buildSelectorDO() {
        return SelectorDO.builder().build();
    }

    private SelectorData buildSelectorData() {
        return SelectorData.builder()
                .id("5")
                .pluginId("pluginId")
                .name("name")
                .matchMode(0)
                .type(1)
                .sort(1)
                .enabled(true)
                .loged(true)
                .continued(false)
                .handle("handle")
                .build();
    }

    private RuleDO buildRuleDO() {
        return RuleDO.builder()
                .id("5")
                .selectorId("selectorId")
                .matchMode(0)
                .name("name")
                .loged(true)
                .sort(1)
                .handle("handle")
                .build();
    }

    private SpringCloudRegisterDTO buildCloudRegisterDTO() {
        return this.buildCloudRegisterDTO(null);
    }

    private SpringCloudRegisterDTO buildCloudRegisterDTO(final RpcTypeEnum rpcTypeEnum) {
        SpringCloudRegisterDTO springCloudRegisterDTO = new SpringCloudRegisterDTO();
        springCloudRegisterDTO.setAppName("appName2");
        springCloudRegisterDTO.setContext("content2");
        springCloudRegisterDTO.setPath("path2");
        springCloudRegisterDTO.setPathDesc("pathDesc2");
        if (Objects.isNull(rpcTypeEnum)) {
            springCloudRegisterDTO.setRpcType(RpcTypeEnum.HTTP.getName());
        } else {
            springCloudRegisterDTO.setRpcType(rpcTypeEnum.getName());
        }
        springCloudRegisterDTO.setRuleName("ruleName2");
        springCloudRegisterDTO.setEnabled(false);
        return springCloudRegisterDTO;
    }

    private MetaDataDTO buildMetaDataD() {
        MetaDataDTO metaDataDTO = new MetaDataDTO();
        metaDataDTO.setId("6");
        metaDataDTO.setAppName("appName3");
        metaDataDTO.setContextPath("content3");
        metaDataDTO.setPath("path3/*");
        metaDataDTO.setRuleName("ruleName3");
        metaDataDTO.setPathDesc("pathDesc3");
        metaDataDTO.setRpcType(RpcTypeEnum.HTTP.getName());
        metaDataDTO.setServiceName("serviceName3");
        metaDataDTO.setMethodName("methodName3");
        metaDataDTO.setParameterTypes("parameterTypes3");
        metaDataDTO.setRpcExt("rpcExt3");
        metaDataDTO.setEnabled(false);
        return metaDataDTO;
    }
}
