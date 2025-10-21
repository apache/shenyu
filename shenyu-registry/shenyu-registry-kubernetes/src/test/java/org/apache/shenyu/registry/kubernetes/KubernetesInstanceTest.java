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

import org.junit.jupiter.api.Test;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test for KubernetesInstance.
 */
public final class KubernetesInstanceTest {

    @Test
    public void testDefaultConstructor() {
        KubernetesInstance instance = new KubernetesInstance();

        assertNotNull(instance);
        assertNull(instance.getInstanceId());
        assertNull(instance.getServiceId());
        assertNull(instance.getHost());
        assertEquals(0, instance.getPort());
        assertFalse(instance.isSecure());
    }

    @Test
    public void testParameterizedConstructor() {
        Map<String, String> metadata = new HashMap<>();
        metadata.put("key1", "value1");
        metadata.put("key2", "value2");

        URI uri = URI.create("http://localhost:8080");

        KubernetesInstance instance = new KubernetesInstance(
                "instance-1",
                "test-service",
                "192.168.1.1",
                8080,
                false,
                uri,
                metadata,
                "http",
                "default"
        );

        assertEquals("instance-1", instance.getInstanceId());
        assertEquals("test-service", instance.getServiceId());
        assertEquals("192.168.1.1", instance.getHost());
        assertEquals(8080, instance.getPort());
        assertFalse(instance.isSecure());
        assertEquals(uri, instance.getUri());
        assertEquals(metadata, instance.getMetadata());
        assertEquals("http", instance.getScheme());
        assertEquals("default", instance.getNamespace());
    }

    @Test
    public void testSetAndGetInstanceId() {
        KubernetesInstance instance = new KubernetesInstance();
        String instanceId = "instance-123";

        instance.setInstanceId(instanceId);

        assertEquals(instanceId, instance.getInstanceId());
    }

    @Test
    public void testSetAndGetServiceId() {
        KubernetesInstance instance = new KubernetesInstance();
        String serviceId = "test-service";

        instance.setServiceId(serviceId);

        assertEquals(serviceId, instance.getServiceId());
    }

    @Test
    public void testSetAndGetHost() {
        KubernetesInstance instance = new KubernetesInstance();
        String host = "192.168.1.100";

        instance.setHost(host);

        assertEquals(host, instance.getHost());
    }

    @Test
    public void testSetAndGetPort() {
        KubernetesInstance instance = new KubernetesInstance();
        int port = 9090;

        instance.setPort(port);

        assertEquals(port, instance.getPort());
    }

    @Test
    public void testSetAndGetSecure() {
        KubernetesInstance instance = new KubernetesInstance();

        instance.setSecure(true);
        assertTrue(instance.isSecure());

        instance.setSecure(false);
        assertFalse(instance.isSecure());
    }

    @Test
    public void testSetAndGetUri() {
        KubernetesInstance instance = new KubernetesInstance();
        URI uri = URI.create("https://example.com:8443");

        instance.setUri(uri);

        assertEquals(uri, instance.getUri());
    }

    @Test
    public void testSetAndGetMetadata() {
        KubernetesInstance instance = new KubernetesInstance();
        Map<String, String> metadata = new HashMap<>();
        metadata.put("env", "production");
        metadata.put("version", "1.0.0");

        instance.setMetadata(metadata);

        assertEquals(2, instance.getMetadata().size());
        assertEquals("production", instance.getMetadata().get("env"));
        assertEquals("1.0.0", instance.getMetadata().get("version"));
    }

    @Test
    public void testSetAndGetScheme() {
        KubernetesInstance instance = new KubernetesInstance();
        String scheme = "https";

        instance.setScheme(scheme);

        assertEquals(scheme, instance.getScheme());
    }

    @Test
    public void testSetAndGetNamespace() {
        KubernetesInstance instance = new KubernetesInstance();
        String namespace = "shenyu-namespace";

        instance.setNamespace(namespace);

        assertEquals(namespace, instance.getNamespace());
    }

    @Test
    public void testAllSettersAndGetters() {
        KubernetesInstance instance = new KubernetesInstance();
        Map<String, String> metadata = new HashMap<>();
        metadata.put("test", "value");
        URI uri = URI.create("http://test:8080");

        instance.setInstanceId("id-1");
        instance.setServiceId("service-1");
        instance.setHost("10.0.0.1");
        instance.setPort(8888);
        instance.setSecure(true);
        instance.setUri(uri);
        instance.setMetadata(metadata);
        instance.setScheme("https");
        instance.setNamespace("test-namespace");

        assertEquals("id-1", instance.getInstanceId());
        assertEquals("service-1", instance.getServiceId());
        assertEquals("10.0.0.1", instance.getHost());
        assertEquals(8888, instance.getPort());
        assertTrue(instance.isSecure());
        assertEquals(uri, instance.getUri());
        assertEquals(metadata, instance.getMetadata());
        assertEquals("https", instance.getScheme());
        assertEquals("test-namespace", instance.getNamespace());
    }

    @Test
    public void testEmptyMetadata() {
        KubernetesInstance instance = new KubernetesInstance();
        Map<String, String> emptyMetadata = new HashMap<>();

        instance.setMetadata(emptyMetadata);

        assertNotNull(instance.getMetadata());
        assertTrue(instance.getMetadata().isEmpty());
    }
}
