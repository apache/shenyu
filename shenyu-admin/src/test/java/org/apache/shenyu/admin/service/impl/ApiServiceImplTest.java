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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.disruptor.RegisterClientServerDisruptorPublisher;
import org.apache.shenyu.admin.disruptor.executor.RegisterServerConsumerExecutor.RegisterServerExecutorFactory;
import org.apache.shenyu.admin.disruptor.subscriber.MetadataExecutorSubscriber;
import org.apache.shenyu.admin.mapper.ApiMapper;
import org.apache.shenyu.admin.mapper.TagMapper;
import org.apache.shenyu.admin.mapper.TagRelationMapper;
import org.apache.shenyu.admin.model.dto.ApiDTO;
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.query.RuleQueryCondition;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.ApiSourceEnum;
import org.apache.shenyu.common.enums.ApiStateEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link ApiServiceImpl} registration and cleanup.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ApiServiceImplTest {

    @Mock
    private SelectorService selectorService;

    @Mock
    private RuleService ruleService;

    @Mock
    private MetaDataService metaDataService;

    @Mock
    private ApiMapper apiMapper;

    @Mock
    private TagRelationMapper tagRelationMapper;

    @Mock
    private TagMapper tagMapper;

    private ApiServiceImpl apiServiceImpl;

    @BeforeEach
    public void setUp() {
        this.apiServiceImpl = new ApiServiceImpl(selectorService, ruleService, metaDataService,
                apiMapper, tagRelationMapper, tagMapper);
    }

    @Test
    public void createDispatchesApiRegistrationInTheDefaultNamespace() {
        final ApiDTO api = new ApiDTO();
        api.setContextPath("/orders");
        api.setApiPath("/orders/list");
        api.setHttpMethod(ApiHttpMethodEnum.GET.getValue());
        api.setState(ApiStateEnum.UNPUBLISHED.getState());
        api.setApiSource(ApiSourceEnum.SWAGGER.getValue());
        api.setRpcType(RpcTypeEnum.HTTP.getName());
        api.setDocument("{}");
        api.setExt("{\"host\":\"127.0.0.1\",\"port\":8080,\"serviceName\":\"orders\",\"methodName\":\"list\"}");
        when(apiMapper.insertSelective(any(ApiDO.class))).thenReturn(1);

        final RegisterClientServerDisruptorPublisher publisher = mock(RegisterClientServerDisruptorPublisher.class);
        final ArgumentCaptor<MetaDataRegisterDTO> eventCaptor = ArgumentCaptor.forClass(MetaDataRegisterDTO.class);
        try (MockedStatic<RegisterClientServerDisruptorPublisher> singleton = mockStatic(RegisterClientServerDisruptorPublisher.class)) {
            singleton.when(RegisterClientServerDisruptorPublisher::getInstance).thenReturn(publisher);
            assertEquals(ShenyuResultMessage.CREATE_SUCCESS, apiServiceImpl.createOrUpdate(api));
            verify(publisher).publish(eventCaptor.capture());
        }

        final MetaDataRegisterDTO event = eventCaptor.getValue();
        final ShenyuClientRegisterService registration = mock(ShenyuClientRegisterService.class);
        final var executor = new RegisterServerExecutorFactory()
                .addSubscribers(new MetadataExecutorSubscriber(Map.of(RpcTypeEnum.HTTP.getName(), registration))).create();
        executor.setData(List.of(event));
        executor.run();

        verify(registration).register(event);
        assertEquals(Constants.SYS_DEFAULT_NAMESPACE_ID, event.getNamespaceId());
    }

    @Test
    public void deleteRemovesOnlyTheExactRuleInTheRegisteredSelector() {
        final ApiDO apiDO = ApiDO.builder()
                .id("api-1")
                .apiPath("/order/get")
                .contextPath("/order")
                .rpcType(RpcTypeEnum.HTTP.getName())
                .build();
        when(apiMapper.selectByIds(anyList())).thenReturn(Collections.singletonList(apiDO));
        when(apiMapper.deleteByIds(anyList())).thenReturn(1);
        when(tagRelationMapper.deleteByApiIds(anyList())).thenReturn(1);

        // the rule search matches by substring, so it also returns the rule of the
        // longer sibling path /order/get/detail — that rule belongs to another API
        final RuleVO exactRule = new RuleVO();
        exactRule.setId("rule-exact");
        exactRule.setName("/order/get");
        final RuleVO longerRule = new RuleVO();
        longerRule.setId("rule-longer");
        longerRule.setName("/order/get/detail");
        final RuleVO sameNameInOtherSelector = new RuleVO();
        sameNameInOtherSelector.setId("rule-other-selector");
        sameNameInOtherSelector.setName("/order/get");
        sameNameInOtherSelector.setSelectorId("other-selector");
        when(ruleService.searchByCondition(any(RuleQueryCondition.class))).thenReturn(Arrays.asList(exactRule, longerRule, sameNameInOtherSelector));
        when(selectorService.findByNameAndPluginNameAndNamespaceId("/order", PluginEnum.DIVIDE.getName(), Constants.SYS_DEFAULT_NAMESPACE_ID))
                .thenReturn(SelectorDO.builder().id("registered-selector").build());
        when(ruleService.findBySelectorIdAndName("registered-selector", "/order/get"))
                .thenReturn(RuleDO.builder().id("rule-exact").ruleName("/order/get").build());
        when(ruleService.findBySelectorId("registered-selector")).thenReturn(List.of(RuleData.builder().id("rule-longer").build()));

        apiServiceImpl.delete(Collections.singletonList("api-1"));

        final ArgumentCaptor<List<String>> ruleIdsCaptor = ArgumentCaptor.forClass(List.class);
        verify(ruleService).deleteByIdsAndNamespaceId(ruleIdsCaptor.capture(), anyString());
        assertEquals(Collections.singletonList("rule-exact"), ruleIdsCaptor.getValue(),
                "deleting one API must not delete longer paths or rules in another selector");
        verify(selectorService, never()).deleteByNamespaceId(anyList(), anyString());
    }

    @Test
    public void deleteDoesNotSearchOtherSelectorsWhenRegistrationSelectorIsMissing() {
        final ApiDO apiDO = ApiDO.builder().id("api-1").apiPath("/order/get").contextPath("/order")
                .rpcType(RpcTypeEnum.GRPC.getName()).build();
        when(apiMapper.selectByIds(List.of("api-1"))).thenReturn(List.of(apiDO));
        when(apiMapper.deleteByIds(List.of("api-1"))).thenReturn(1);

        apiServiceImpl.delete(List.of("api-1"));

        verify(selectorService).findByNameAndPluginNameAndNamespaceId("/order", PluginEnum.GRPC.getName(), Constants.SYS_DEFAULT_NAMESPACE_ID);
        verifyNoInteractions(ruleService);
        verify(selectorService, never()).deleteByNamespaceId(anyList(), anyString());
    }

    @Test
    public void deleteKeepsDifferentlyCasedRuleReturnedByTheDatabase() {
        final ApiDO apiDO = ApiDO.builder().id("api-1").apiPath("/order/get").contextPath("/order")
                .rpcType(RpcTypeEnum.HTTP.getName()).build();
        when(apiMapper.selectByIds(List.of("api-1"))).thenReturn(List.of(apiDO));
        when(apiMapper.deleteByIds(List.of("api-1"))).thenReturn(1);
        when(selectorService.findByNameAndPluginNameAndNamespaceId("/order", PluginEnum.DIVIDE.getName(), Constants.SYS_DEFAULT_NAMESPACE_ID))
                .thenReturn(SelectorDO.builder().id("registered-selector").build());
        when(ruleService.findBySelectorIdAndName("registered-selector", "/order/get"))
                .thenReturn(RuleDO.builder().id("rule-uppercase").ruleName("/order/GET").build());

        apiServiceImpl.delete(List.of("api-1"));

        verify(ruleService, never()).deleteByIdsAndNamespaceId(anyList(), anyString());
        verify(selectorService, never()).deleteByNamespaceId(anyList(), anyString());
    }
}
