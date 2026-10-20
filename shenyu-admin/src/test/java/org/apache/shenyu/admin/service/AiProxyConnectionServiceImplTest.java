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
import org.apache.shenyu.admin.mapper.AiProxyApiKeyMapper;
import org.apache.shenyu.admin.model.entity.ProxyApiKeyDO;
import org.apache.shenyu.admin.service.impl.AiProxyConnectionServiceImpl;
import org.apache.shenyu.admin.service.support.AiProxyRealKeyResolver;
import org.apache.shenyu.common.dto.ProxyApiKeyData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.Mock;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/** Tests for {@link AiProxyConnectionServiceImpl}. */
@ExtendWith(MockitoExtension.class)
public final class AiProxyConnectionServiceImplTest {

    @Mock
    private AiProxyRealKeyResolver resolver;

    @Mock
    private AiProxyApiKeyMapper mapper;

    @Mock
    private org.springframework.context.ApplicationEventPublisher publisher;

    @InjectMocks
    private AiProxyConnectionServiceImpl service;

    @Test
    public void testRefreshWithNoKeysOnlyInvalidates() {
        when(mapper.selectBySelectorId("s1")).thenReturn(Collections.emptyList());
        service.refreshApiKeysBySelectorId("s1");
        verify(resolver).invalidate("s1");
        verifyNoInteractions(publisher);
    }

    @Test
    public void testRefreshPublishesResolvedData() {
        ProxyApiKeyDO key = new ProxyApiKeyDO();
        key.setSelectorId("s1");
        key.setNamespaceId(null);
        key.setProxyApiKey("proxy");
        key.setEnabled(Boolean.TRUE);
        key.setDescription("desc");
        when(mapper.selectBySelectorId("s1")).thenReturn(Collections.singletonList(key));
        when(resolver.resolveRealKey("s1")).thenReturn(Optional.of("real"));
        service.refreshApiKeysBySelectorId("s1");
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(publisher).publishEvent(captor.capture());
        ProxyApiKeyData data = (ProxyApiKeyData) ((java.util.List<?>) captor.getValue().getSource()).get(0);
        assertEquals("real", data.getRealApiKey());
        assertEquals("proxy", data.getProxyApiKey());
        assertEquals(Boolean.TRUE, data.getEnabled());
        assertEquals("s1", data.getSelectorId());
        assertEquals("desc", data.getDescription());
        assertEquals(org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID, data.getNamespaceId());
        assertEquals(ConfigGroupEnum.AI_PROXY_API_KEY, captor.getValue().getGroupKey());
        assertEquals(DataEventTypeEnum.UPDATE, ReflectionTestUtils.getField(captor.getValue(), "eventType"));
    }

    @Test
    public void testRefreshHandlesMissingRealKeyAndDisabledMapping() {
        ProxyApiKeyDO key = new ProxyApiKeyDO();
        key.setSelectorId("s1");
        key.setNamespaceId("custom");
        key.setProxyApiKey("proxy");
        key.setEnabled(Boolean.FALSE);
        when(mapper.selectBySelectorId("s1")).thenReturn(Collections.singletonList(key));
        when(resolver.resolveRealKey("s1")).thenReturn(Optional.empty());
        service.refreshApiKeysBySelectorId("s1");
        ArgumentCaptor<DataChangedEvent> captor = ArgumentCaptor.forClass(DataChangedEvent.class);
        verify(publisher).publishEvent(captor.capture());
        ProxyApiKeyData data = (ProxyApiKeyData) ((java.util.List<?>) captor.getValue().getSource()).get(0);
        assertNull(data.getRealApiKey());
        assertEquals(Boolean.FALSE, data.getEnabled());
        assertEquals("custom", data.getNamespaceId());
    }
}
