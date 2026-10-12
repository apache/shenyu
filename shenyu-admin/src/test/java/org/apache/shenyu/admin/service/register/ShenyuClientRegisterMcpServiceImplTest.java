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

import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.apache.shenyu.register.common.dto.McpToolsRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.Mock;
import org.springframework.test.util.ReflectionTestUtils;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/** Tests for {@link ShenyuClientRegisterMcpServiceImpl}. */
@ExtendWith(MockitoExtension.class)
public final class ShenyuClientRegisterMcpServiceImplTest {
    @InjectMocks
    private ShenyuClientRegisterMcpServiceImpl service;

    @Mock
    private MetaDataService metaDataService;

    @Mock
    private SelectorService selectorService;

    @Mock
    private RuleService ruleService;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Test
    public void testRpcTypeAndRegisterTools() {
        ReflectionTestUtils.setField(service, "metaDataService", metaDataService);
        ReflectionTestUtils.setField(service, "selectorService", selectorService);
        ReflectionTestUtils.setField(service, "ruleService", ruleService);
        ShenyuClientRegisterMcpServiceImpl spy = org.mockito.Mockito.spy(service);
        doNothing().when(spy).checkNamespacePluginRel(anyString(), anyString());
        MetaDataRegisterDTO metadata = MetaDataRegisterDTO.builder().path("/tool").ruleName("tool")
                .namespaceId("ns").contextPath("").build();
        McpToolsRegisterDTO dto = new McpToolsRegisterDTO();
        dto.setMetaDataRegisterDTO(metadata);
        dto.setMcpConfig("{}");
        dto.setNamespaceId("ns");
        when(selectorService.registerDefault(any(), anyString(), anyString())).thenReturn("selector");
        when(metaDataService.findByPathAndNamespaceId("/tool", "ns")).thenReturn(null);
        spy.registerMcpTools(dto);
        assertEquals(RpcTypeEnum.MCP.getName(), spy.rpcType());
        verify(spy).checkNamespacePluginRel("ns", PluginNameAdapter.rpcTypeAdapter(RpcTypeEnum.MCP.getName()));
        verify(selectorService).registerDefault(any(), anyString(), anyString());
        ArgumentCaptor<RuleDTO> captor = ArgumentCaptor.forClass(RuleDTO.class);
        verify(ruleService).registerDefault(captor.capture());
        RuleDTO rule = captor.getValue();
        assertEquals("selector", rule.getSelectorId());
        assertEquals("tool", rule.getName());
        assertEquals("{}", rule.getHandle());
        assertEquals("ns", rule.getNamespaceId());
        assertEquals(MatchModeEnum.AND.getCode(), rule.getMatchMode());
        assertEquals(Boolean.TRUE, rule.getEnabled());
        assertEquals(Boolean.TRUE, rule.getLoged());
        assertEquals(Boolean.FALSE, rule.getMatchRestful());
        assertEquals(1, rule.getSort());
        assertEquals(1, rule.getRuleConditions().size());
        RuleConditionDTO condition = rule.getRuleConditions().get(0);
        assertEquals(ParamTypeEnum.URI.getName(), condition.getParamType());
        assertEquals("/", condition.getParamName());
        assertEquals("/**", condition.getParamValue());
        assertEquals(OperatorEnum.MATCH.getAlias(), condition.getOperator());
        verify(metaDataService).saveOrUpdateMetaData(null, metadata);
    }

    @Test
    public void testMcpExtensionHandlersReturnEmptyImplementations() {
        assertNull(service.selectorHandler(MetaDataRegisterDTO.builder().build()));
        assertNull(service.ruleHandler());
        assertNull(service.buildHandle(java.util.Collections.emptyList(), new org.apache.shenyu.admin.model.entity.SelectorDO()));
    }

    @Test
    public void testRegisterToolsUsesDefaultNamespaceAndContextPath() {
        ReflectionTestUtils.setField(service, "metaDataService", metaDataService);
        ReflectionTestUtils.setField(service, "selectorService", selectorService);
        ReflectionTestUtils.setField(service, "ruleService", ruleService);
        ShenyuClientRegisterMcpServiceImpl spy = org.mockito.Mockito.spy(service);
        doNothing().when(spy).checkNamespacePluginRel(anyString(), anyString());
        doNothing().when(spy).registerContextPath(any());
        MetaDataRegisterDTO metadata = MetaDataRegisterDTO.builder().path("/tool").ruleName("tool")
                .namespaceId(org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID).contextPath("/ctx").build();
        McpToolsRegisterDTO dto = new McpToolsRegisterDTO();
        dto.setMetaDataRegisterDTO(metadata);
        dto.setMcpConfig("{}");
        when(selectorService.registerDefault(any(), anyString(), anyString())).thenReturn("selector");
        MetaDataDO existing = MetaDataDO.builder().build();
        when(metaDataService.findByPathAndNamespaceId("/tool", org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID))
                .thenReturn(existing);
        spy.registerMcpTools(dto);
        assertEquals(org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID, dto.getNamespaceId());
        verify(spy).checkNamespacePluginRel(org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID,
                org.apache.shenyu.common.utils.PluginNameAdapter.rpcTypeAdapter(RpcTypeEnum.MCP.getName()));
        verify(spy).registerContextPath(metadata);
        verify(metaDataService).saveOrUpdateMetaData(existing, metadata);
    }

    @Test
    public void testRegisterToolsStopsWhenPluginDoesNotExist() {
        ReflectionTestUtils.setField(service, "pluginMapper", pluginMapper);
        ReflectionTestUtils.setField(service, "namespacePluginRelMapper", namespacePluginRelMapper);
        McpToolsRegisterDTO dto = new McpToolsRegisterDTO();
        dto.setNamespaceId("ns");
        dto.setMetaDataRegisterDTO(MetaDataRegisterDTO.builder().build());
        assertThrows(IllegalArgumentException.class, () -> service.registerMcpTools(dto));
        verify(pluginMapper).selectByName(PluginNameAdapter.rpcTypeAdapter(RpcTypeEnum.MCP.getName()));
        verifyNoInteractions(namespacePluginRelMapper, selectorService, ruleService, metaDataService);
    }

    @Test
    public void testRegisterToolsStopsWhenPluginIsNotEnabledInNamespace() {
        ReflectionTestUtils.setField(service, "pluginMapper", pluginMapper);
        ReflectionTestUtils.setField(service, "namespacePluginRelMapper", namespacePluginRelMapper);
        when(pluginMapper.selectByName(PluginNameAdapter.rpcTypeAdapter(RpcTypeEnum.MCP.getName())))
                .thenReturn(PluginDO.builder().id("plugin").build());
        McpToolsRegisterDTO dto = new McpToolsRegisterDTO();
        dto.setNamespaceId("ns");
        dto.setMetaDataRegisterDTO(MetaDataRegisterDTO.builder().build());
        assertThrows(IllegalArgumentException.class, () -> service.registerMcpTools(dto));
        verify(namespacePluginRelMapper).selectByPluginIdAndNamespaceId("plugin", "ns");
        verifyNoInteractions(selectorService, ruleService, metaDataService);
    }
}
