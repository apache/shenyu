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

package org.apache.shenyu.register.client.etcd;

import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doAnswer;

/**
 * test for EtcdClientRegisterRepository.
 */
public class EtcdClientRegisterRepositoryTest {

    private EtcdClientRegisterRepository repository;

    private final Map<String, String> etcdBroker = new HashMap<>();

    @BeforeEach
    public void setUp() throws IllegalAccessException, NoSuchFieldException {
        this.repository = new EtcdClientRegisterRepository();
        Class<? extends EtcdClientRegisterRepository> clazz = this.repository.getClass();

        String fieldString = "client";
        Field field = clazz.getDeclaredField(fieldString);
        field.setAccessible(true);
        field.set(repository, mockEtcdClient());

        etcdBroker.clear();
    }

    private EtcdClient mockEtcdClient() {
        EtcdClient etcdClient = mock(EtcdClient.class);

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            String value = invocationOnMock.getArgument(1);
            etcdBroker.put(key, value);
            return null;
        }).when(etcdClient).putEphemeral(anyString(), anyString());

        doAnswer(invocationOnMock -> {
            etcdBroker.clear();
            return null;
        }).when(etcdClient).close();
        return etcdClient;
    }

    @Test
    public void testPersistInterface() {
        final MetaDataRegisterDTO data = MetaDataRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .contextPath("/context")
                .ruleName("ruleName")
                .build();

        repository.persistInterface(data);
        String metadataPath = "/shenyu/register/metadata/http/context/context-ruleName";
        assertTrue(etcdBroker.containsKey(metadataPath));
        assertEquals(etcdBroker.get(metadataPath), GsonUtils.getInstance().toJson(data));
        repository.close();
    }
    
    @Test
    public void testPersistUri() {
        final URIRegisterDTO data = URIRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .contextPath("/context")
                .build();
        repository.persistURI(data);
        String uriPath = "/shenyu/register/uri/http/context/host:80";
        assertTrue(etcdBroker.containsKey(uriPath));
        assertEquals(etcdBroker.get(uriPath), GsonUtils.getInstance().toJson(data));
    }

    @Test
    public void testPersistInterface4Other() {
        final MetaDataRegisterDTO data = MetaDataRegisterDTO.builder()
                .rpcType("grpc")
                .host("host")
                .port(80)
                .contextPath("/context")
                .ruleName("ruleName")
                .serviceName("testService")
                .methodName("testMethod")
                .build();

        repository.persistInterface(data);
        String metadataPath = "/shenyu/register/metadata/grpc/context/testService.testMethod";
        assertTrue(etcdBroker.containsKey(metadataPath));
        assertEquals(etcdBroker.get(metadataPath), GsonUtils.getInstance().toJson(data));
        repository.close();
    }
}
