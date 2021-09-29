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
import org.apache.shenyu.admin.model.dto.SelectorDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.impl.MetaDataServiceImpl;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
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

import java.util.UUID;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * ShenyuClientRegisterSpringCloudServiceImplTest.
 */
@PrepareForTest(MetaDataServiceImpl.class)
@RunWith(MockitoJUnitRunner.Silent.class)
public class ShenyuClientRegisterSpringCloudServiceImplTest { 

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
    
    @Mock
    private UpstreamCheckService upstreamCheckService;
    
    private AbstractShenyuClientRegisterServiceImpl shenyuClientRegisterSpringCloudService;
    
    @Before
    public void setUp() {
        shenyuClientRegisterSpringCloudService = new ShenyuClientRegisterSpringCloudServiceImpl(metaDataService, eventPublisher, selectorService, ruleService, pluginService, upstreamCheckService);
    }

    @Test
    public void testSaveOrUpdateMetaData() {
        MetaDataDO metaDataDO = buildMetaDataDO();
        MetaDataRegisterDTO metaDataRegisterDTO = buildScMetaDataRegisterDTO();
        shenyuClientRegisterSpringCloudService.saveOrUpdateMetaData(metaDataDO, metaDataRegisterDTO);
        verify(eventPublisher, times(1)).publishEvent(any());
    }

    @Test
    public void testHandlerSelector() {
        String selectorId = UUID.randomUUID().toString();
        MetaDataRegisterDTO metaDataRegisterDTO = buildScMetaDataRegisterDTO();
        given(selectorService.register(any())).willReturn(selectorId);
        Assert.assertEquals(shenyuClientRegisterSpringCloudService.handlerSelector(metaDataRegisterDTO), selectorId);
    }

    @Test
    public void testHandlerRule() {
        String selectorId = UUID.randomUUID().toString();
        MetaDataDO metaDataDO = buildMetaDataDO();
        MetaDataRegisterDTO metaDataRegisterDTO = buildScMetaDataRegisterDTO();
        given(ruleService.register(any(), anyString(), anyBoolean())).willReturn(UUIDUtils.getInstance().generateShortUuid());
        shenyuClientRegisterSpringCloudService.handlerRule(selectorId, metaDataRegisterDTO, metaDataDO);
    }

    @Test
    public void testRegisterRule() {
        String selectorId = UUID.randomUUID().toString();
        final MetaDataRegisterDTO metaDataRegisterDTO = buildScMetaDataRegisterDTO();
        metaDataRegisterDTO.setPath("path*");
        RuleDTO result = shenyuClientRegisterSpringCloudService.registerRule(selectorId, metaDataRegisterDTO, PluginEnum.SPRING_CLOUD.getName());
        Assert.assertNotNull(result);
        Assert.assertEquals(result.getSelectorId(), selectorId);
        Assert.assertEquals(result.getName(), metaDataRegisterDTO.getRuleName());
        Assert.assertNotNull(result.getRuleConditions());
        Assert.assertEquals(result.getRuleConditions().size(), 1);
        Assert.assertEquals(result.getRuleConditions().get(0).getOperator(), OperatorEnum.MATCH.getAlias());

        metaDataRegisterDTO.setPath("path");
        result = shenyuClientRegisterSpringCloudService.registerRule(selectorId, metaDataRegisterDTO, PluginEnum.SPRING_CLOUD.getName());
        Assert.assertEquals(result.getRuleConditions().get(0).getOperator(), OperatorEnum.EQ.getAlias());
    }

    @Test
    public void testRegister() {
        String selectorId = UUID.randomUUID().toString();

        final MetaDataRegisterDTO metaDataRegisterDTO = buildScMetaDataRegisterDTO();
        MetaDataDO metaDataDO = buildMetaDataDO();
        given(metaDataService.findByPath(any())).willReturn(metaDataDO);
        given(selectorService.register(SelectorDTO.builder().build())).willReturn(selectorId);
        given(selectorService.handlerSelectorNeedUpstreamCheck(any(), eq(PluginEnum.SPRING_CLOUD.getName()))).willReturn(selectorId);
        Assert.assertEquals(ShenyuResultMessage.SUCCESS, shenyuClientRegisterSpringCloudService.register(metaDataRegisterDTO));
    }

    private MetaDataRegisterDTO buildScMetaDataRegisterDTO() {
        MetaDataRegisterDTO springMvcRegisterDTO = new MetaDataRegisterDTO();
        springMvcRegisterDTO.setAppName("appName1");
        springMvcRegisterDTO.setContextPath("content1");
        springMvcRegisterDTO.setPath("path1");
        springMvcRegisterDTO.setPathDesc("pathDesc1");
        springMvcRegisterDTO.setRpcType(RpcTypeEnum.SPRING_CLOUD.getName());
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
