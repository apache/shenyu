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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.listener.DataChangedEventDispatcher;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.listener.zookeeper.ZookeeperDataChangedListener;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.impl.ShenyuClientRegisterServiceImpl;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;

/**
 * Test cases for ShenyuClientRegisterService.
 */
@RunWith(MockitoJUnitRunner.Silent.class)
public final class ShenyuClientRegisterServiceTest {

    @InjectMocks
    private DataChangedEventDispatcher dataChangedEventDispatcher;

    @Mock
    private ZookeeperDataChangedListener zookeeperDataChangedListener;

    @Mock
    private ApplicationContext applicationContext;

    @Mock
    private ShenyuClientRegisterServiceImpl shenyuClientRegisterService;

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
        shenyuClientRegisterService = new ShenyuClientRegisterServiceImpl(metaDataMapper, eventPublisher, selectorService,
                ruleService, ruleMapper, upstreamCheckService, selectorMapper, pluginMapper);

        Map<String, DataChangedListener> listenerMap = new HashMap<>();
        listenerMap.put("zookeeperDataChangedListener", zookeeperDataChangedListener);
        when(applicationContext.getBeansOfType(DataChangedListener.class)).thenReturn(listenerMap);
        dataChangedEventDispatcher.afterPropertiesSet();
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
        MetaDataRegisterDTO dto = buildSpringMvcRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringMvc(dto));
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
        MetaDataRegisterDTO dto = buildSpringMvcRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringMvc(dto));
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
        MetaDataRegisterDTO dto = buildSpringMvcRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringMvc(dto));
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
        MetaDataRegisterDTO dto = buildSpringMvcRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringMvc(dto));

        dto = buildSpringMvcRegisterDTO(RpcTypeEnum.DUBBO);
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringMvc(dto));

        dto = buildSpringMvcRegisterDTO(RpcTypeEnum.SOFA);
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringMvc(dto));

        dto = buildSpringMvcRegisterDTO(RpcTypeEnum.SPRING_CLOUD);
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringMvc(dto));

        dto = buildSpringMvcRegisterDTO(RpcTypeEnum.TARS);
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringMvc(dto));
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
        MetaDataRegisterDTO dto = buildSpringMvcRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringMvc(dto));
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
        MetaDataRegisterDTO dto = buildSpringMvcRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringMvc(dto));
    }

    private PluginDTO buildPluginDTO() {
        final PluginDTO pluginDTO = new PluginDTO();
        pluginDTO.setEnabled(true);
        pluginDTO.setConfig("test-config");
        pluginDTO.setRole("1");
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
        MetaDataRegisterDTO dto = buildCloudRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringCloud(dto));
    }

    @Test
    public void testRegisterSpringCloudWithEmptyMetaData() {
        given(metaDataMapper.findByPath(any())).willReturn(null);
        SelectorDO selectorDO = buildSelectorDO();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        MetaDataRegisterDTO dto = buildCloudRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringCloud(dto));
    }

    @Test
    public void testRegisterSpringCloudWithEmptyRule() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        SelectorDO selectorDO = buildSelectorDO();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        given(ruleMapper.findByName(any())).willReturn(null);
        MetaDataRegisterDTO dto = buildCloudRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringCloud(dto));
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

        MetaDataRegisterDTO dto = buildCloudRegisterDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringCloud(dto));

        dto = buildCloudRegisterDTO(RpcTypeEnum.DUBBO);
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringCloud(dto));

        dto = buildCloudRegisterDTO(RpcTypeEnum.SOFA);
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringCloud(dto));

        dto = buildCloudRegisterDTO(RpcTypeEnum.SPRING_CLOUD);
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringCloud(dto));

        dto = buildCloudRegisterDTO(RpcTypeEnum.TARS);
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerSpringCloud(dto));
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
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerDubbo(dto));
    }

    /**
     * The above testRegisterDubbo only test the main logic about registration of dubbo, but the event not covered.
     * */
    @Test
    public void testRegisterDubboAndEvenhandle() {
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        DataChangedEvent dataChangedEvent = new DataChangedEvent(ConfigGroupEnum.META_DATA,
                null, Collections.singletonList(dto));
        dataChangedEventDispatcher.onApplicationEvent(dataChangedEvent);
    }

    @Test
    public void testRegisterDubboNotExistMetaData() {
        given(metaDataMapper.findByPath(any())).willReturn(null);
        given(metaDataMapper.update(any())).willReturn(1);
        SelectorDO selectorDO = buildSelectorDO();
        given(selectorService.findByName(any())).willReturn(selectorDO);
        RuleDO ruleDO = buildRuleDO();
        given(ruleMapper.findByName(any())).willReturn(ruleDO);
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerDubbo(dto));
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
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerDubbo(dto));
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
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterService.registerDubbo(dto));
    }

    @Test
    public void testRegisterSofaAlreadyExist() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        metaDataDO.setServiceName("serviceName33");
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        PluginDTO pluginDTO = buildPluginDTO();
        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        assertEquals("you path already exist!", shenyuClientRegisterService.registerSofa(dto));
    }

    @Test
    public void testRegisterSofaNotExistMetaData() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.findByServiceNameAndMethod(any(), any())).willReturn(null);
        PluginDTO pluginDTO = buildPluginDTO();
        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        assertEquals("success", shenyuClientRegisterService.registerSofa(dto));
    }

    @Test
    public void testRegisterSofaWithExistMetaData() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.findByServiceNameAndMethod(any(), any())).willReturn(metaDataDO);
        PluginDTO pluginDTO = buildPluginDTO();
        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        assertEquals("success", shenyuClientRegisterService.registerSofa(dto));
    }

    @Test
    public void testRegisterTarsAlreadyExist() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        metaDataDO.setServiceName("serviceName33");
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        PluginDTO pluginDTO = buildPluginDTO();
        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        assertEquals("you path already exist!", shenyuClientRegisterService.registerTars(dto));
    }

    @Test
    public void testRegisterTarsNotExistMetaData() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.findByServiceNameAndMethod(any(), any())).willReturn(null);
        PluginDTO pluginDTO = buildPluginDTO();
        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        assertEquals("success", shenyuClientRegisterService.registerTars(dto));
    }

    @Test
    public void testRegisterTarsWithExistMetaData() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataMapper.findByPath(any())).willReturn(metaDataDO);
        given(metaDataMapper.findByServiceNameAndMethod(any(), any())).willReturn(metaDataDO);
        PluginDTO pluginDTO = buildPluginDTO();
        PluginDO pluginDO = PluginDO.buildPluginDO(pluginDTO);
        given(pluginMapper.selectByName(any())).willReturn(pluginDO);
        MetaDataRegisterDTO dto = buildMetaDataDTO();
        assertEquals("success", shenyuClientRegisterService.registerTars(dto));
    }

    private MetaDataRegisterDTO buildSpringMvcRegisterDTO() {
        return this.buildSpringMvcRegisterDTO(RpcTypeEnum.HTTP);
    }

    private MetaDataRegisterDTO buildSpringMvcRegisterDTO(final RpcTypeEnum rpcTypeEnum) {
        MetaDataRegisterDTO springMvcRegisterDTO = new MetaDataRegisterDTO();
        springMvcRegisterDTO.setAppName("appName1");
        springMvcRegisterDTO.setContextPath("content1");
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
                .logged(true)
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

    private MetaDataRegisterDTO buildCloudRegisterDTO() {
        return this.buildCloudRegisterDTO(null);
    }

    private MetaDataRegisterDTO buildCloudRegisterDTO(final RpcTypeEnum rpcTypeEnum) {
        MetaDataRegisterDTO springCloudRegisterDTO = new MetaDataRegisterDTO();
        springCloudRegisterDTO.setAppName("appName2");
        springCloudRegisterDTO.setContextPath("content2");
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

    private MetaDataRegisterDTO buildMetaDataDTO() {
        MetaDataRegisterDTO metaDataDTO = new MetaDataRegisterDTO();
        metaDataDTO.setAppName("appName3");
        metaDataDTO.setContextPath("content3");
        metaDataDTO.setPath("path3/*");
        metaDataDTO.setRuleName("ruleName3");
        metaDataDTO.setPathDesc("pathDesc3");
        metaDataDTO.setRpcType(RpcTypeEnum.HTTP.getName());
        metaDataDTO.setServiceName("serviceName3");
        metaDataDTO.setMethodName("methodName3");
        metaDataDTO.setParameterTypes("parameterTypes3");
        metaDataDTO.setRpcExt("{\"loadbalance\":\"random\"}");
        metaDataDTO.setEnabled(false);
        return metaDataDTO;
    }
}
