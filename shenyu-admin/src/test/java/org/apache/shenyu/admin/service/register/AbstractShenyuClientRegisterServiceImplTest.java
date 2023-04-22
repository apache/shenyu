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
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.ApiService;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
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
import org.springframework.context.ApplicationEventPublisher;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.mock;

/**
 * Test cases for AbstractShenyuClientRegisterServiceImpl.
 */
@ExtendWith(MockitoExtension.class)
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

    @Test
    public void testRegister() {
        MetaDataRegisterDTO dto = MetaDataRegisterDTO.builder().build();
        dto.setContextPath("Context_Path");
        dto.setPath("Path");
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
        assertEquals(ShenyuResultMessage.SUCCESS, abstractShenyuClientRegisterService.registerApiDoc(apiDocRegisterDTO));

        apiDocRegisterDTO.setEventType(EventType.OFFLINE);
        assertEquals(ShenyuResultMessage.SUCCESS, abstractShenyuClientRegisterService.registerApiDoc(apiDocRegisterDTO));
    }

    @Test
    public void testDoRegisterURI() {
        assertEquals(StringUtils.EMPTY, abstractShenyuClientRegisterService.doRegisterURI("Selector_Name", new ArrayList<>()));

        URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder().build();
        uriRegisterDTO.setPort(8080);
        uriRegisterDTO.setHost("Host");
        assertThrows(ShenyuException.class, () -> abstractShenyuClientRegisterService.doRegisterURI("Selector_Name", Collections.singletonList(uriRegisterDTO)));

        SelectorDO selectorDO = mock(SelectorDO.class);
        when(selectorService.findByNameAndPluginName(any(), any())).thenReturn(selectorDO);
        SelectorData selectorData = new SelectorData();
        when(selectorService.buildByName(any(), any())).thenReturn(selectorData);
        assertEquals(ShenyuResultMessage.SUCCESS, abstractShenyuClientRegisterService.doRegisterURI("Selector_Name", Collections.singletonList(uriRegisterDTO)));
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
        assertFalse(abstractShenyuClientRegisterService.doSubmit("Selector_Id", new ArrayList<>()));

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
        public String rpcType() {
            return "grpc";
        }
    }
}
