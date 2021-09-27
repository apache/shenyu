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

package org.apache.shenyu.admin.service.register;

import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.springframework.context.ApplicationEventPublisher;

import java.util.Objects;
import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.anyBoolean;

/**
 * Test cases for ShenyuClientRegisterDubboServiceImpl.
 */
@PrepareForTest(MetaDataServiceImpl.class)
@RunWith(MockitoJUnitRunner.Silent.class)
public final class ShenyuClientRegisterDubboServiceImplTest {

    @Mock
    private MetaDataMapper metaDataMapper;

    @InjectMocks
    private MetaDataServiceImpl metaDataService;

    @Mock
    private SelectorService selectorService;

    @Mock
    private PluginService pluginService;

    @Mock
    private RuleService ruleService;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    private ShenyuClientRegisterDubboServiceImpl shenyuClientRegisterDubboService;

    @Before
    public void setUp() {
        shenyuClientRegisterDubboService = new ShenyuClientRegisterDubboServiceImpl(metaDataService, selectorService, ruleService, pluginService);
    }

    @Test
    public void testSaveOrUpdateMetaData() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        MetaDataRegisterDTO dubboMetaDataRegisterDTO = buildDubboMetaDataRegisterDTO(RpcTypeEnum.DUBBO);
        shenyuClientRegisterDubboService.saveOrUpdateMetaData(metaDataDO, dubboMetaDataRegisterDTO);
        verify(eventPublisher, times(1)).publishEvent(any());
    }

    @Test
    public void testHandlerSelector() {
        String selectorId = UUID.randomUUID().toString();
        MetaDataRegisterDTO dubboMetaDataRegisterDTO = buildDubboMetaDataRegisterDTO(RpcTypeEnum.DUBBO);
        given(selectorService.handlerSelectorNeedUpstreamCheck(any(), eq(PluginEnum.DUBBO.getName()))).willReturn(selectorId);
        Assert.assertEquals(shenyuClientRegisterDubboService.handlerSelector(dubboMetaDataRegisterDTO), selectorId);
    }

    @Test
    public void testHandlerRule() {
        String selectorId = UUID.randomUUID().toString();
        MetaDataDO metaDataDO = buildMetaDataDO();
        MetaDataRegisterDTO dubboMetaDataRegisterDTO = buildDubboMetaDataRegisterDTO(RpcTypeEnum.DUBBO);
        given(ruleService.register(any(), anyString(), anyBoolean())).willReturn(UUIDUtils.getInstance().generateShortUuid());
        shenyuClientRegisterDubboService.handlerRule(selectorId, dubboMetaDataRegisterDTO, metaDataDO);
    }

    @Test
    public void testRegisterRule() {
        String selectorId = UUID.randomUUID().toString();
        MetaDataRegisterDTO dubboMetaDataRegisterDTO = buildDubboMetaDataRegisterDTO(RpcTypeEnum.DUBBO);
        dubboMetaDataRegisterDTO.setPath("path*");
        RuleDTO result = shenyuClientRegisterDubboService.registerRule(selectorId, dubboMetaDataRegisterDTO, PluginEnum.DUBBO.getName());
        Assert.assertNotNull(result);
        Assert.assertEquals(result.getSelectorId(), selectorId);
        Assert.assertEquals(result.getName(), dubboMetaDataRegisterDTO.getRuleName());
        Assert.assertNotNull(result.getRuleConditions());
        Assert.assertEquals(result.getRuleConditions().size(), 1);
        Assert.assertEquals(result.getRuleConditions().get(0).getOperator(), OperatorEnum.MATCH.getAlias());

        dubboMetaDataRegisterDTO.setPath("path");
        result = shenyuClientRegisterDubboService.registerRule(selectorId, dubboMetaDataRegisterDTO, PluginEnum.DUBBO.getName());
        Assert.assertEquals(result.getRuleConditions().get(0).getOperator(), OperatorEnum.EQ.getAlias());
    }

    @Test
    public void testRegister() {
        String selectorId = UUID.randomUUID().toString();

        MetaDataRegisterDTO dubboMetaDataRegisterDTO = buildDubboMetaDataRegisterDTO(RpcTypeEnum.DUBBO);
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataService.findByPath(any())).willReturn(metaDataDO);
        given(selectorService.handlerSelectorNeedUpstreamCheck(any(), eq(PluginEnum.DUBBO.getName()))).willReturn(selectorId);
        Assert.assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterDubboService.register(dubboMetaDataRegisterDTO));
        verify(eventPublisher, times(1)).publishEvent(any());
        verify(selectorService, times(1)).handlerSelectorNeedUpstreamCheck(any(), eq(PluginEnum.DUBBO.getName()));
        verify(ruleService, times(1)).register(any(), anyString(), anyBoolean());
    }

    private MetaDataRegisterDTO buildDubboMetaDataRegisterDTO(final RpcTypeEnum rpcTypeEnum) {
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
        springMvcRegisterDTO.setRegisterMetaData(false);
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
}
