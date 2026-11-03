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

import org.apache.shenyu.admin.mapper.DiscoveryHandlerMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.DiscoveryRelMapper;
import org.apache.shenyu.admin.mapper.DiscoveryUpstreamMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.SelectorExportDTO;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shiro.authz.UnauthorizedException;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Verify selective export boundaries before producing any ZIP.
 */
@ExtendWith(MockitoExtension.class)
public class SelectiveConfigsExportServiceTest {

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private RuleMapper ruleMapper;

    @Mock
    private SelectorService selectorService;

    @Mock
    private RuleService ruleService;

    @Mock
    private PluginService pluginService;

    @Mock
    private NamespacePluginService namespacePluginService;

    @Mock
    private PluginHandleService pluginHandleService;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private DiscoveryHandlerMapper handlerMapper;

    @Mock
    private DiscoveryRelMapper relMapper;

    @Mock
    private DiscoveryUpstreamMapper upstreamMapper;

    @InjectMocks
    private SelectiveConfigsExportService service;

    @Test
    public void testInlineHandleWithoutLocalDiscoveryIsRejected() {
        selector();
        PluginVO template = new PluginVO();
        template.setId("divide");
        template.setName("divide");
        when(pluginService.findById("divide")).thenReturn(template);
        assertTrue(assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders")))).getMessage().contains("inline-only"));
        verifyNoInteractions(discoveryMapper, relMapper, upstreamMapper, ruleMapper, namespacePluginService);
    }

    @Test
    public void testAllNamespaceMembershipIsCheckedBeforeConfigurationReads() {
        when(selectorMapper.selectNamespaceIdById("orders")).thenReturn("source");
        when(selectorMapper.selectNamespaceIdById("foreign")).thenReturn("other");
        assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders", "foreign"))));
        verifyNoInteractions(selectorService, pluginService, ruleService, handlerMapper);
    }

    @Test
    public void testDeniedSelectorDoesNotReadDependencies() {
        when(selectorMapper.selectNamespaceIdById("orders")).thenReturn("source");
        when(selectorService.findById("orders")).thenThrow(new UnauthorizedException("denied"));
        assertThrows(UnauthorizedException.class, () -> service.export(request(List.of("orders"))));
        verifyNoInteractions(pluginService, ruleService, handlerMapper);
    }

    @Test
    public void testUnsupportedPluginIsRejected() {
        selector();
        PluginVO plugin = new PluginVO();
        plugin.setName("dubbo");
        when(pluginService.findById("divide")).thenReturn(plugin);
        assertTrue(assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders")))).getMessage().contains("Only divide"));
        verifyNoInteractions(handlerMapper, ruleMapper);
    }

    @Test
    public void testInvalidSelectionsFailBeforeQueries() {
        assertThrows(ShenyuException.class, () -> service.export(request(Collections.emptyList())));
        assertThrows(ShenyuException.class, () -> service.export(request(Collections.nCopies(101, "orders"))));
        assertThrows(ShenyuException.class, () -> service.export(request(List.of("orders", "orders"))));
        assertThrows(ShenyuException.class, () -> service.export(request(List.of(" "))));
        verifyNoInteractions(selectorMapper, selectorService);
    }

    private void selector() {
        when(selectorMapper.selectNamespaceIdById("orders")).thenReturn("source");
        SelectorVO selector = new SelectorVO();
        selector.setId("orders");
        selector.setName("orders");
        selector.setPluginId("divide");
        selector.setNamespaceId("source");
        selector.setHandle("[{\"protocol\":\"http\",\"upstreamUrl\":\"127.0.0.1:8080\",\"weight\":50}]");
        when(selectorService.findById("orders")).thenReturn(selector);
    }

    private SelectorExportDTO request(final List<String> ids) {
        SelectorExportDTO request = new SelectorExportDTO();
        request.setNamespace("source");
        request.setSelectorIds(ids);
        return request;
    }
}
