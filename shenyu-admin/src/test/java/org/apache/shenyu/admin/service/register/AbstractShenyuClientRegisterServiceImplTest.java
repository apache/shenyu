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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.ApiService;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.service.manager.RegisterApiDocService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.register.common.dto.ApiDocRegisterDTO;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.context.ApplicationEventPublisher;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;

/**
 * Test cases for AbstractShenyuClientRegisterServiceImpl.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class AbstractShenyuClientRegisterServiceImplTest {

    @InjectMocks
    private MockAbstractShenyuClientRegisterServiceImpl abstractShenyuClientRegisterService = new MockAbstractShenyuClientRegisterServiceImpl();

    @Mock
    private SelectorService selectorService;

    @Mock
    private RuleService ruleService;

    @Mock
    private ApiService apiService;

    @Mock
    private TagService tagService;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private MetaDataService metaDataService;

    @Mock
    private UpstreamCheckService upstreamCheckService;

    @Mock
    private RegisterApiDocService registerApiDocService;

    @Mock
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Mock
    private PluginMapper pluginMapper;

    @Test
    public void testRegister() {
        MetaDataRegisterDTO dto = MetaDataRegisterDTO.builder().build();
        dto.setContextPath("Context_Path");
        dto.setPath("Path");
        dto.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        PluginDO pluginDO = new PluginDO();
        pluginDO.setId("1");
        pluginDO.setName(PluginEnum.GRPC.getName());
        when(pluginMapper.selectByName(PluginEnum.GRPC.getName())).thenReturn(pluginDO);
        NamespacePluginVO namespacePluginVO = new NamespacePluginVO();
        namespacePluginVO.setPluginId("1");
        namespacePluginVO.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        when(namespacePluginRelMapper.selectByPluginIdAndNamespaceId(pluginDO.getId(), Constants.SYS_DEFAULT_NAMESPACE_ID)).thenReturn(namespacePluginVO);
        when(selectorService.registerDefault(any(), any(), any())).thenReturn("SELECTOR_ID");
        assertEquals(ShenyuResultMessage.SUCCESS, abstractShenyuClientRegisterService.register(dto));

        dto.setPath("Path/");
        assertEquals(ShenyuResultMessage.SUCCESS, abstractShenyuClientRegisterService.register(dto));

        dto.setPath("Path/**");
        assertEquals(ShenyuResultMessage.SUCCESS, abstractShenyuClientRegisterService.register(dto));

        dto.setPath("Path/*{id}");
        assertEquals(ShenyuResultMessage.SUCCESS, abstractShenyuClientRegisterService.register(dto));
    }

    @Test
    public void testRegisterApiDoc() {
        ApiDocRegisterDTO apiDocRegisterDTO = ApiDocRegisterDTO.builder().build();
        apiDocRegisterDTO.setEventType(EventType.REGISTER);
        apiDocRegisterDTO.setApiPath("Api_Path");
        apiDocRegisterDTO.setApiSource(2);
        apiDocRegisterDTO.setDocument("Document");
        apiDocRegisterDTO.setExt("Ext");
        apiDocRegisterDTO.setVersion("Version");
        apiDocRegisterDTO.setRpcType("Rpc_Type");
        apiDocRegisterDTO.setConsume("Consume");
        apiDocRegisterDTO.setProduce("Produce");
        apiDocRegisterDTO.setContextPath("Context_Path");
        apiDocRegisterDTO.setHttpMethod(3);
        apiDocRegisterDTO.setState(1);
        apiDocRegisterDTO.setApiDesc("Api_Desc");
        apiDocRegisterDTO.setTags(Collections.singletonList("Tag"));
        assertEquals(ShenyuResultMessage.SUCCESS, abstractShenyuClientRegisterService.registerApiDoc(apiDocRegisterDTO));

        TagVO tagVO = new TagVO();
        tagVO.setId("123");
        when(tagService.findByQuery(any())).thenReturn(Collections.singletonList(tagVO));
        assertEquals(Collections.singletonList(tagVO), tagService.findByQuery(any()));

        apiDocRegisterDTO.setEventType(EventType.OFFLINE);
        assertEquals(ShenyuResultMessage.SUCCESS, abstractShenyuClientRegisterService.registerApiDoc(apiDocRegisterDTO));
    }

    @Test
    public void testDoRegisterURI() {
        PluginDO pluginDO = new PluginDO();
        pluginDO.setId("1");
        pluginDO.setName(PluginEnum.GRPC.getName());
        when(pluginMapper.selectByName(PluginEnum.GRPC.getName())).thenReturn(pluginDO);
        NamespacePluginVO namespacePluginVO = new NamespacePluginVO();
        namespacePluginVO.setPluginId("1");
        namespacePluginVO.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        when(namespacePluginRelMapper.selectByPluginIdAndNamespaceId(pluginDO.getId(), Constants.SYS_DEFAULT_NAMESPACE_ID)).thenReturn(namespacePluginVO);
        assertEquals(StringUtils.EMPTY, abstractShenyuClientRegisterService.doRegisterURI("Selector_Name", new ArrayList<>(), SYS_DEFAULT_NAMESPACE_ID));

        URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder().build();
        uriRegisterDTO.setPort(8080);
        uriRegisterDTO.setHost("Host");
        assertThrows(ShenyuException.class, () -> abstractShenyuClientRegisterService.doRegisterURI("Selector_Name", Collections.singletonList(uriRegisterDTO), SYS_DEFAULT_NAMESPACE_ID));

        SelectorDO selectorDO = mock(SelectorDO.class);
        when(selectorService.findByNameAndPluginNameAndNamespaceId(any(), any(), any())).thenReturn(selectorDO);
        SelectorData selectorData = new SelectorData();
        when(selectorService.buildByNameAndPluginNameAndNamespaceId(any(), any(), any())).thenReturn(selectorData);
        PluginDO plugin = new PluginDO();
        plugin.setId("1");
        plugin.setName(PluginEnum.GRPC.getName());
        when(pluginMapper.selectByName(PluginEnum.GRPC.getName())).thenReturn(plugin);
        NamespacePluginVO namespacePlugin = new NamespacePluginVO();
        namespacePlugin.setPluginId("1");
        namespacePlugin.setNamespaceId(Constants.SYS_DEFAULT_NAMESPACE_ID);
        when(namespacePluginRelMapper.selectByPluginIdAndNamespaceId(pluginDO.getId(), Constants.SYS_DEFAULT_NAMESPACE_ID)).thenReturn(namespacePlugin);
        assertEquals(ShenyuResultMessage.SUCCESS, abstractShenyuClientRegisterService.doRegisterURI("Selector_Name", Collections.singletonList(uriRegisterDTO), SYS_DEFAULT_NAMESPACE_ID));
    }

    @Test
    public void testGetMetaDataService() {
        assertEquals(metaDataService, abstractShenyuClientRegisterService.getMetaDataService());
    }

    @Test
    public void testGetSelectorService() {
        assertEquals(selectorService, abstractShenyuClientRegisterService.getSelectorService());
    }

    @Test
    public void testGetRuleService() {
        assertEquals(ruleService, abstractShenyuClientRegisterService.getRuleService());
    }

    @Test
    public void testDoSubmit() {
        assertTrue(abstractShenyuClientRegisterService.doSubmit("Selector_Id", new ArrayList<>()));

        CommonUpstream commonUpstream = new CommonUpstream();
        when(upstreamCheckService.checkAndSubmit(any(), any())).thenReturn(true);
        assertTrue(abstractShenyuClientRegisterService.doSubmit("Selector_Id", Collections.singletonList(commonUpstream)));
    }

    @Test
    public void testBuildContextPathDefaultRuleDTO() {
        MetaDataRegisterDTO metaDataRegisterDTO = MetaDataRegisterDTO.builder().build();
        metaDataRegisterDTO.setContextPath("Context_Path");
        RuleDTO ruleDTO = abstractShenyuClientRegisterService.buildContextPathDefaultRuleDTO("Selector_Id", metaDataRegisterDTO, "Rule_Handler");
        assertEquals("Selector_Id", ruleDTO.getSelectorId());
        assertEquals("Context_Path", ruleDTO.getName());
        assertTrue(ruleDTO.getEnabled());
        assertTrue(ruleDTO.getLoged());
        assertEquals(1, ruleDTO.getSort());
        assertEquals("Rule_Handler", ruleDTO.getHandle());
    }

    static class MockAbstractShenyuClientRegisterServiceImpl extends AbstractShenyuClientRegisterServiceImpl {

        @Override
        protected String selectorHandler(final MetaDataRegisterDTO metaDataDTO) {
            return "handler";
        }

        @Override
        protected String ruleHandler() {
            return "ruleHandler";
        }

        @Override
        protected void registerMetadata(final MetaDataRegisterDTO metaDataDTO) {

        }

        @Override
        protected String buildHandle(final List<URIRegisterDTO> uriList, final SelectorDO selectorDO) {
            return "buildHandle";
        }

        @Override
        protected void doDiscoveryLocal(final SelectorDO selectorDO, final String pluginName, final List<URIRegisterDTO> uriList) {

        }

        @Override
        protected DiscoverySyncData fetch(final String selectorId, final String selectorName, final String pluginName, final String namespaceId) {
            return new DiscoverySyncData();
        }

        @Override
        protected void removeDiscoveryUpstream(final String selectorId, final String url) {
        }

        @Override
        public String rpcType() {
            return "grpc";
        }
    }
}
