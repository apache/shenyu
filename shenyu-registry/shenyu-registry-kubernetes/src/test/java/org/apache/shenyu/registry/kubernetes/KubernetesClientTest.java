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

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Test for KubernetesClient.
 */
public final class KubernetesClientTest {

    private KubernetesClient kubernetesClient;

    private RestTemplate restTemplate;

    private KubernetesConfig kubernetesConfig;

    @BeforeEach
    public void setUp() throws Exception {
        kubernetesConfig = new KubernetesConfig();
        kubernetesConfig.setDiscoveryServerUrl("http://localhost:8761");
        kubernetesConfig.setEnabled(true);
        kubernetesConfig.setNamespaces(Arrays.asList("default", "shenyu"));

        kubernetesClient = new KubernetesClient(kubernetesConfig);

        // Mock the RestTemplate
        restTemplate = mock(RestTemplate.class);
        Class<? extends KubernetesClient> clazz = kubernetesClient.getClass();
        Field restField = clazz.getDeclaredField("rest");
        restField.setAccessible(true);
        restField.set(kubernetesClient, restTemplate);
    }

    @Test
    public void testSelectInstances() {
        String serviceId = "test-service";
        KubernetesInstance instance1 = createKubernetesInstance("instance-1", serviceId, "192.168.1.1", 8080, "default");
        KubernetesInstance instance2 = createKubernetesInstance("instance-2", serviceId, "192.168.1.2", 8081, "shenyu");
        KubernetesInstance[] instances = new KubernetesInstance[]{instance1, instance2};

        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(instances, HttpStatus.OK);
        when(restTemplate.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        List<KubernetesInstance> result = kubernetesClient.selectInstances(serviceId);

        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("192.168.1.1", result.get(0).getHost());
        assertEquals(8080, result.get(0).getPort());
    }

    @Test
    public void testSelectInstancesWithNamespaceFilter() {
        String serviceId = "test-service";
        KubernetesInstance instance1 = createKubernetesInstance("instance-1", serviceId, "192.168.1.1", 8080, "default");
        KubernetesInstance instance2 = createKubernetesInstance("instance-2", serviceId, "192.168.1.2", 8081, "other-namespace");
        KubernetesInstance instance3 = createKubernetesInstance("instance-3", serviceId, "192.168.1.3", 8082, "shenyu");
        KubernetesInstance[] instances = new KubernetesInstance[]{instance1, instance2, instance3};

        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(instances, HttpStatus.OK);
        when(restTemplate.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        List<KubernetesInstance> result = kubernetesClient.selectInstances(serviceId);

        // Only instances in "default" and "shenyu" namespaces should be returned
        assertEquals(2, result.size());
        assertTrue(result.stream().anyMatch(i -> "default".equals(i.getNamespace())));
        assertTrue(result.stream().anyMatch(i -> "shenyu".equals(i.getNamespace())));
        assertTrue(result.stream().noneMatch(i -> "other-namespace".equals(i.getNamespace())));
    }

    @Test
    public void testSelectInstancesWithEmptyNamespaceFilter() {
        KubernetesConfig config = new KubernetesConfig();
        config.setDiscoveryServerUrl("http://localhost:8761");
        config.setEnabled(true);
        config.setNamespaces(Arrays.asList());

        KubernetesClient client = new KubernetesClient(config);

        String serviceId = "test-service";
        KubernetesInstance instance1 = createKubernetesInstance("instance-1", serviceId, "192.168.1.1", 8080, "any-namespace");
        KubernetesInstance instance2 = createKubernetesInstance("instance-2", serviceId, "192.168.1.2", 8081, "other-namespace");
        KubernetesInstance[] instances = new KubernetesInstance[]{instance1, instance2};

        RestTemplate mockRest = mock(RestTemplate.class);
        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(instances, HttpStatus.OK);
        when(mockRest.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        try {
            Field restField = client.getClass().getDeclaredField("rest");
            restField.setAccessible(true);
            restField.set(client, mockRest);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }

        List<KubernetesInstance> result = client.selectInstances(serviceId);

        // All instances should be returned when namespace filter is empty
        assertEquals(2, result.size());
    }

    @Test
    public void testSelectInstancesWithNullResponse() {
        String serviceId = "test-service";

        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(null, HttpStatus.OK);
        when(restTemplate.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        List<KubernetesInstance> result = kubernetesClient.selectInstances(serviceId);

        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    public void testSelectInstancesWithEmptyResponse() {
        String serviceId = "test-service";
        KubernetesInstance[] instances = new KubernetesInstance[]{};

        ResponseEntity<KubernetesInstance[]> responseEntity = new ResponseEntity<>(instances, HttpStatus.OK);
        when(restTemplate.getForEntity(anyString(), eq(KubernetesInstance[].class), any(Object[].class)))
                .thenReturn(responseEntity);

        List<KubernetesInstance> result = kubernetesClient.selectInstances(serviceId);

        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    private KubernetesInstance createKubernetesInstance(final String instanceId, final String serviceId,
                                                         final String host, final int port, final String namespace) {
        KubernetesInstance instance = new KubernetesInstance();
        instance.setInstanceId(instanceId);
        instance.setServiceId(serviceId);
        instance.setHost(host);
        instance.setPort(port);
        instance.setNamespace(namespace);
        return instance;
    }
}
