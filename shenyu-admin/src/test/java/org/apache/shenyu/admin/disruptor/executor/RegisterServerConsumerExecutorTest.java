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

package org.apache.shenyu.admin.disruptor.executor;

import org.apache.shenyu.disruptor.consumer.QueueConsumerExecutor;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.subsriber.ExecutorSubscriber;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * Test cases for {@link RegisterServerConsumerExecutor}.
 */
@ExtendWith(MockitoExtension.class)
class RegisterServerConsumerExecutorTest {

    @Mock
    private ExecutorTypeSubscriber<DataTypeParent> mockSubscriber;

    @Mock
    private ExecutorSubscriber<DataTypeParent> mockExecutorSubscriber;

    private RegisterServerConsumerExecutor.RegisterServerExecutorFactory factory;

    @BeforeEach
    void setUp() {
        factory = new RegisterServerConsumerExecutor.RegisterServerExecutorFactory();
    }

    @Test
    void testFactoryCreate() {
        when(mockSubscriber.getType()).thenReturn(DataType.META_DATA);
        factory.addSubscribers(mockSubscriber);

        QueueConsumerExecutor<Collection<DataTypeParent>> executor = factory.create();

        assertNotNull(executor);
        assertTrue(executor instanceof RegisterServerConsumerExecutor);
    }

    @Test
    void testFactoryFixName() {
        assertEquals("shenyu_register_server", factory.fixName());
    }

    @Test
    void testFactoryAddSubscribers() {
        RegisterServerConsumerExecutor.RegisterServerExecutorFactory result = factory.addSubscribers(mockSubscriber);

        assertEquals(factory, result);
        assertEquals(1, factory.getSubscribers().size());
        assertTrue(factory.getSubscribers().contains(mockSubscriber));
    }

    @Test
    void testFactoryGetSubscribers() {
        factory.addSubscribers(mockSubscriber);

        assertEquals(1, factory.getSubscribers().size());
    }

    @Test
    void testRunWithEmptyData() {
        when(mockSubscriber.getType()).thenReturn(DataType.META_DATA);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        executor.setData(Collections.emptyList());
        executor.run();

        verify(mockSubscriber, never()).executor(any());
    }

    @Test
    void testRunWithValidURIData() {
        when(mockSubscriber.getType()).thenReturn(DataType.URI);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        URIRegisterDTO uriDTO = URIRegisterDTO.builder()
                .contextPath("/test")
                .rpcType("http")
                .namespaceId("default")
                .build();

        List<DataTypeParent> data = new ArrayList<>();
        data.add(uriDTO);

        executor.setData(data);
        executor.run();

        verify(mockSubscriber, times(1)).executor(any());
    }

    @Test
    void testRunWithValidMetaData() {
        when(mockSubscriber.getType()).thenReturn(DataType.META_DATA);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        MetaDataRegisterDTO metaDTO = MetaDataRegisterDTO.builder()
                .appName("testApp")
                .path("/test/path")
                .ruleName("testRule")
                .rpcType("http")
                .namespaceId("default")
                .build();

        List<DataTypeParent> data = new ArrayList<>();
        data.add(metaDTO);

        executor.setData(data);
        executor.run();

        verify(mockSubscriber, times(1)).executor(any());
    }

    @Test
    void testRunWithInvalidURIData() {
        when(mockSubscriber.getType()).thenReturn(DataType.URI);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        URIRegisterDTO uriDTO = URIRegisterDTO.builder()
                .contextPath("")
                .rpcType("http")
                .namespaceId("default")
                .build();

        List<DataTypeParent> data = new ArrayList<>();
        data.add(uriDTO);

        executor.setData(data);
        executor.run();

        verify(mockSubscriber, never()).executor(any());
    }

    @Test
    void testRunWithInvalidMetaData() {
        when(mockSubscriber.getType()).thenReturn(DataType.META_DATA);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        MetaDataRegisterDTO metaDTO = MetaDataRegisterDTO.builder()
                .appName("")
                .path("/test/path")
                .ruleName("testRule")
                .rpcType("http")
                .namespaceId("default")
                .build();

        List<DataTypeParent> data = new ArrayList<>();
        data.add(metaDTO);

        executor.setData(data);
        executor.run();

        verify(mockSubscriber, never()).executor(any());
    }

    @Test
    void testIsValidDataWithValidURI() throws Exception {
        when(mockSubscriber.getType()).thenReturn(DataType.URI);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        URIRegisterDTO uriDTO = URIRegisterDTO.builder()
                .contextPath("/test")
                .rpcType("http")
                .namespaceId("default")
                .build();

        Method method = RegisterServerConsumerExecutor.class.getDeclaredMethod("isValidData", Object.class);
        method.setAccessible(true);
        boolean result = (boolean) method.invoke(executor, uriDTO);

        assertTrue(result);
    }

    @Test
    void testIsValidDataWithInvalidURI() throws Exception {
        when(mockSubscriber.getType()).thenReturn(DataType.URI);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        URIRegisterDTO uriDTO = URIRegisterDTO.builder()
                .contextPath("")
                .rpcType("http")
                .namespaceId("default")
                .build();

        Method method = RegisterServerConsumerExecutor.class.getDeclaredMethod("isValidData", Object.class);
        method.setAccessible(true);
        boolean result = (boolean) method.invoke(executor, uriDTO);

        assertFalse(result);
    }

    @Test
    void testIsValidDataWithValidMetaData() throws Exception {
        when(mockSubscriber.getType()).thenReturn(DataType.META_DATA);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        MetaDataRegisterDTO metaDTO = MetaDataRegisterDTO.builder()
                .appName("testApp")
                .path("/test/path")
                .ruleName("testRule")
                .rpcType("http")
                .namespaceId("default")
                .build();

        Method method = RegisterServerConsumerExecutor.class.getDeclaredMethod("isValidData", Object.class);
        method.setAccessible(true);
        boolean result = (boolean) method.invoke(executor, metaDTO);

        assertTrue(result);
    }

    @Test
    void testIsValidDataWithInvalidMetaData() throws Exception {
        when(mockSubscriber.getType()).thenReturn(DataType.META_DATA);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        MetaDataRegisterDTO metaDTO = MetaDataRegisterDTO.builder()
                .appName("")
                .path("/test/path")
                .ruleName("testRule")
                .rpcType("http")
                .namespaceId("default")
                .build();

        Method method = RegisterServerConsumerExecutor.class.getDeclaredMethod("isValidData", Object.class);
        method.setAccessible(true);
        boolean result = (boolean) method.invoke(executor, metaDTO);

        assertFalse(result);
    }

    @Test
    void testIsValidDataWithOtherType() throws Exception {
        when(mockSubscriber.getType()).thenReturn(DataType.META_DATA);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        DataTypeParent otherData = mock(DataTypeParent.class);

        Method method = RegisterServerConsumerExecutor.class.getDeclaredMethod("isValidData", Object.class);
        method.setAccessible(true);
        boolean result = (boolean) method.invoke(executor, otherData);

        assertTrue(result);
    }

    @Test
    void testSelectExecutorWithEmptyList() throws Exception {
        when(mockSubscriber.getType()).thenReturn(DataType.META_DATA);
        factory.addSubscribers(mockSubscriber);
        RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();

        Method method = RegisterServerConsumerExecutor.class.getDeclaredMethod("selectExecutor", Collection.class);
        method.setAccessible(true);

        assertThrows(Exception.class, () -> {
            method.invoke(executor, Collections.emptyList());
        });
    }

    @Test
    void testRunWithMixedValidAndInvalidData() {
        when(mockSubscriber.getType()).thenReturn(DataType.URI);
        factory.addSubscribers(mockSubscriber);

        URIRegisterDTO validURI = URIRegisterDTO.builder()
                .contextPath("/test")
                .rpcType("http")
                .namespaceId("default")
                .build();

        URIRegisterDTO invalidURI = URIRegisterDTO.builder()
                .contextPath("")
                .rpcType("http")
                .namespaceId("default")
                .build();

        List<DataTypeParent> data = new ArrayList<>();
        data.add(validURI);
        data.add(invalidURI);

        final RegisterServerConsumerExecutor executor = (RegisterServerConsumerExecutor) factory.create();
        executor.setData(data);
        executor.run();

        verify(mockSubscriber, times(1)).executor(any());
    }
}
