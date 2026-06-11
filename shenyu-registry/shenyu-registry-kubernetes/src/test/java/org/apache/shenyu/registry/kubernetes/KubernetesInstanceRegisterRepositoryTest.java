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

package org.apache.shenyu.registry.kubernetes;

import org.apache.shenyu.registry.api.config.RegisterConfig;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for KubernetesInstanceRegisterRepository.
 */
public final class KubernetesInstanceRegisterRepositoryTest {

    private KubernetesInstanceRegisterRepository repository;

    private RestTemplate restTemplate;

    private final String serverUrl = "http://localhost:8761";

    private final String namespace = "default,shenyu";

    @BeforeEach
    public void setUp() throws Exception {
        repository = new KubernetesInstanceRegisterRepository();

        RegisterConfig config = new RegisterConfig();
        config.setServerLists(serverUrl);
        config.setEnabled(true);
        Properties properties = new Properties();
        properties.setProperty("namespaces", namespace);
        config.setProps(properties);

        repository.init(config);

        // Mock the RestTemplate
        restTemplate = mock(RestTemplate.class);
        Class<? extends KubernetesInstanceRegisterRepository> clazz = repository.getClass();
        Field kubernetesClientField = clazz.getDeclaredField("kubernetesClient");
        kubernetesClientField.setAccessible(true);
        KubernetesClient kubernetesClient = (KubernetesClient) kubernetesClientField.get(repository);

        Class<? extends KubernetesClient> clientClazz = kubernetesClient.getClass();
        Field restField = clientClazz.getDeclaredField("rest");
        restField.setAccessible(true);
        restField.set(kubernetesClient, restTemplate);
    }

    @Test
    public void testInit() {
        RegisterConfig config = new RegisterConfig();
        config.setServerLists("http://test:8080");
        config.setEnabled(true);
        Properties properties = new Properties();
        properties.setProperty("namespaces", "test-namespace");
        config.setProps(properties);

        KubernetesInstanceRegisterRepository newRepository = new KubernetesInstanceRegisterRepository();
        newRepository.init(config);

        assertNotNull(newRepository);
    }

    @Test
    public void testPersistInstance() {
        InstanceEntity instance = InstanceEntity.builder()
                .appName("test-service")
                .host("192.168.1.1")
                .port(8080)
                .build();

        // persistInstance is currently empty implementation
        repository.persistInstance(instance);
        // No exception means success
        assertTrue(true);
    }

    @Test
    public void testSelectInstances() {
        String serviceId = "test-service";
        KubernetesInstance instance1 = createKubernetesInstance("instance-1", serviceId, "192.168.1.1", 8080, false, "default");
        KubernetesInstance instance2 = createKubernetesInstance("instance-2", serviceId, "192.168.1.2", 8081, false, "shenyu");
        KubernetesInstance[] instances = new KubernetesInstance[]{instance1, instance2};

        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(instances, HttpStatus.OK);
        when(restTemplate.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        List<InstanceEntity> result = repository.selectInstances(serviceId);

        assertEquals(2, result.size());
        assertEquals(serviceId, result.get(0).getAppName());
        assertEquals("192.168.1.1", result.get(0).getHost());
        assertEquals(8080, result.get(0).getPort());
        assertEquals("http://192.168.1.1:8080", result.get(0).getUri().toString());
    }

    @Test
    public void testSelectInstancesWithSecure() {
        String serviceId = "test-service";
        KubernetesInstance instance = createKubernetesInstance("instance-1", serviceId, "192.168.1.1", 8443, true, "default");
        KubernetesInstance[] instances = new KubernetesInstance[]{instance};

        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(instances, HttpStatus.OK);
        when(restTemplate.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        List<InstanceEntity> result = repository.selectInstances(serviceId);

        assertEquals(1, result.size());
        assertEquals("https://192.168.1.1:8443", result.get(0).getUri().toString());
    }

    @Test
    public void testSelectInstancesWithDefaultPort() {
        String serviceId = "test-service";
        KubernetesInstance instance1 = createKubernetesInstance("instance-1", serviceId, "192.168.1.1", 0, false, "default");
        KubernetesInstance instance2 = createKubernetesInstance("instance-2", serviceId, "192.168.1.2", -1, true, "default");
        KubernetesInstance[] instances = new KubernetesInstance[]{instance1, instance2};

        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(instances, HttpStatus.OK);
        when(restTemplate.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        List<InstanceEntity> result = repository.selectInstances(serviceId);

        assertEquals(2, result.size());
        assertEquals("http://192.168.1.1:80", result.get(0).getUri().toString());
        assertEquals("https://192.168.1.2:443", result.get(1).getUri().toString());
    }

    @Test
    public void testSelectInstancesEmpty() {
        String serviceId = "test-service";
        KubernetesInstance[] instances = new KubernetesInstance[]{};

        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(instances, HttpStatus.OK);
        when(restTemplate.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        List<InstanceEntity> result = repository.selectInstances(serviceId);

        assertTrue(result.isEmpty());
    }

    @Test
    public void testSelectInstancesWithNullResponse() {
        String serviceId = "test-service";

        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(null, HttpStatus.OK);
        when(restTemplate.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        List<InstanceEntity> result = repository.selectInstances(serviceId);

        assertTrue(result.isEmpty());
    }

    @Test
    public void testSelectInstancesFilteredByNamespace() {
        String serviceId = "test-service";
        KubernetesInstance instance1 = createKubernetesInstance("instance-1", serviceId, "192.168.1.1", 8080, false, "default");
        KubernetesInstance instance2 = createKubernetesInstance("instance-2", serviceId, "192.168.1.2", 8081, false, "other-namespace");
        KubernetesInstance[] instances = new KubernetesInstance[]{instance1, instance2};

        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(instances, HttpStatus.OK);
        when(restTemplate.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        List<InstanceEntity> result = repository.selectInstances(serviceId);

        // Only instance1 should be returned as it's in "default" namespace
        assertEquals(1, result.size());
        assertEquals("192.168.1.1", result.get(0).getHost());
    }

    @Test
    public void testClose() {
        // close() calls super.close() which is a default implementation
        repository.close();
        // No exception means success
        assertTrue(true);
    }

    private KubernetesInstance createKubernetesInstance(final String instanceId, final String serviceId,
                                                         final String host, final int port, final boolean secure,
                                                         final String namespace) {
        KubernetesInstance instance = new KubernetesInstance();
        instance.setInstanceId(instanceId);
        instance.setServiceId(serviceId);
        instance.setHost(host);
        instance.setPort(port);
        instance.setSecure(secure);
        instance.setNamespace(namespace);
        return instance;
    }
}
