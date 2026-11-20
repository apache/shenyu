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

package org.apache.shenyu.admin.disruptor.subscriber;

import org.apache.shenyu.admin.service.register.ClientRegisterProvisioner;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterService;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.type.DataType;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link MetadataExecutorSubscriber}.
 */
public class MetadataExecutorSubscriberTest {

    private ShenyuClientRegisterService registerService;

    private Map<String, ShenyuClientRegisterService> registerServiceMap;

    @BeforeEach
    public void setUp() {
        registerService = mock(ShenyuClientRegisterService.class);
        registerServiceMap = new HashMap<>();
        registerServiceMap.put("http", registerService);
    }

    private MetaDataRegisterDTO buildMetaData() {
        return MetaDataRegisterDTO.builder()
                .rpcType("http")
                .contextPath("/bcsp/iam")
                .path("/bcsp/iam/api/auth/login/pc/login")
                .build();
    }

    @Test
    public void testGetType() {
        Assertions.assertEquals(DataType.META_DATA, new MetadataExecutorSubscriber(registerServiceMap).getType());
    }

    @Test
    public void testExecutorRegistersByRpcType() {
        MetadataExecutorSubscriber subscriber = new MetadataExecutorSubscriber(registerServiceMap);
        MetaDataRegisterDTO metaDataRegisterDTO = buildMetaData();
        subscriber.executor(Collections.singletonList(metaDataRegisterDTO));
        verify(registerService).register(metaDataRegisterDTO);
    }

    @Test
    public void testProvisionersInvokedAfterRegister() {
        ClientRegisterProvisioner provisioner = mock(ClientRegisterProvisioner.class);
        when(provisioner.pluginName()).thenReturn("bcspAuthn");
        when(provisioner.supports(any())).thenReturn(true);
        MetadataExecutorSubscriber subscriber =
                new MetadataExecutorSubscriber(registerServiceMap, List.of(provisioner));
        MetaDataRegisterDTO metaDataRegisterDTO = buildMetaData();
        subscriber.executor(Collections.singletonList(metaDataRegisterDTO));
        verify(registerService).register(metaDataRegisterDTO);
        verify(provisioner).provision(metaDataRegisterDTO);
    }

    @Test
    public void testProvisionerNotSupportsSkipped() {
        ClientRegisterProvisioner provisioner = mock(ClientRegisterProvisioner.class);
        when(provisioner.supports(any())).thenReturn(false);
        MetadataExecutorSubscriber subscriber =
                new MetadataExecutorSubscriber(registerServiceMap, List.of(provisioner));
        subscriber.executor(Collections.singletonList(buildMetaData()));
        verify(provisioner, never()).provision(any());
    }

    @Test
    public void testProvisionerFailureIsolated() {
        ClientRegisterProvisioner failing = mock(ClientRegisterProvisioner.class);
        when(failing.pluginName()).thenReturn("broken");
        doThrow(new IllegalStateException("boom")).when(failing).provision(any());
        ClientRegisterProvisioner healthy = mock(ClientRegisterProvisioner.class);
        when(healthy.pluginName()).thenReturn("healthy");
        when(healthy.supports(any())).thenReturn(true);
        MetadataExecutorSubscriber subscriber =
                new MetadataExecutorSubscriber(registerServiceMap, List.of(failing, healthy));
        subscriber.executor(Collections.singletonList(buildMetaData()));
        verify(healthy).provision(any());
    }

    @Test
    public void testUnknownRpcTypeSkipsRegisterAndProvisioners() {
        ClientRegisterProvisioner provisioner = mock(ClientRegisterProvisioner.class);
        MetadataExecutorSubscriber subscriber =
                new MetadataExecutorSubscriber(registerServiceMap, List.of(provisioner));
        MetaDataRegisterDTO metaDataRegisterDTO = buildMetaData();
        metaDataRegisterDTO.setRpcType("websocket");
        subscriber.executor(Collections.singletonList(metaDataRegisterDTO));
        verify(registerService, never()).register(any());
        verify(provisioner, never()).provision(any());
    }
}
