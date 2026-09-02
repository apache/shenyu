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

package org.apache.shenyu.admin.controller;

import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.dto.ProxyApiKeyDTO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.result.ShenyuAdminResult;
import org.apache.shenyu.admin.service.AiProxyApiKeyService;
import org.apache.shenyu.common.constant.AdminConstants;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link AiProxyApiKeyController}.
 */
@ExtendWith(MockitoExtension.class)
public final class AiProxyApiKeyControllerTest {

    @Mock
    private AiProxyApiKeyService aiProxyApiKeyService;

    @Mock
    private SelectorMapper selectorMapper;

    @InjectMocks
    private AiProxyApiKeyController controller;

    @Test
    public void shouldRejectCreateWhenSelectorDoesNotExist() {
        final ProxyApiKeyDTO dto = new ProxyApiKeyDTO();
        dto.setNamespaceId("client-namespace");
        when(selectorMapper.selectById("missing-selector")).thenReturn(null);

        final ShenyuAdminResult result = controller.create("missing-selector", dto);

        assertEquals(AdminConstants.ID_NOT_EXIST, result.getMessage());
        assertEquals("client-namespace", dto.getNamespaceId());
        verify(aiProxyApiKeyService, never()).create(any(ProxyApiKeyDTO.class), any(String.class));
    }

    @Test
    public void shouldDeriveNamespaceAndDelegateWhenSelectorExists() {
        final ProxyApiKeyDTO dto = new ProxyApiKeyDTO();
        dto.setNamespaceId("client-namespace");
        final SelectorDO selector = SelectorDO.builder().namespaceId("selector-namespace").build();
        when(selectorMapper.selectById("selector-1")).thenReturn(selector);
        when(aiProxyApiKeyService.create(dto, "selector-1")).thenReturn(0);

        controller.create("selector-1", dto);

        assertEquals("selector-namespace", dto.getNamespaceId());
        verify(aiProxyApiKeyService).create(eq(dto), eq("selector-1"));
    }
}
