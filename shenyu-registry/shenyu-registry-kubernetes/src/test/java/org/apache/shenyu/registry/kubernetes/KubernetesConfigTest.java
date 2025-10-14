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

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test for KubernetesConfig.
 */
public final class KubernetesConfigTest {

    @Test
    public void testDefaultConstructor() {
        KubernetesConfig config = new KubernetesConfig();

        assertNotNull(config);
        assertTrue(config.isEnabled());
        assertNotNull(config.getNamespaces());
        assertTrue(config.getNamespaces().isEmpty());
    }

    @Test
    public void testSetAndGetDiscoveryServerUrl() {
        KubernetesConfig config = new KubernetesConfig();
        String url = "http://localhost:8761";

        config.setDiscoveryServerUrl(url);

        assertEquals(url, config.getDiscoveryServerUrl());
    }

    @Test
    public void testSetAndGetEnabled() {
        KubernetesConfig config = new KubernetesConfig();

        config.setEnabled(false);
        assertFalse(config.isEnabled());

        config.setEnabled(true);
        assertTrue(config.isEnabled());
    }

    @Test
    public void testSetAndGetNamespaces() {
        KubernetesConfig config = new KubernetesConfig();
        List<String> namespaces = Arrays.asList("default", "shenyu", "test");

        config.setNamespaces(namespaces);

        assertEquals(3, config.getNamespaces().size());
        assertTrue(config.getNamespaces().contains("default"));
        assertTrue(config.getNamespaces().contains("shenyu"));
        assertTrue(config.getNamespaces().contains("test"));
    }

    @Test
    public void testSetEmptyNamespaces() {
        KubernetesConfig config = new KubernetesConfig();
        List<String> namespaces = Arrays.asList();

        config.setNamespaces(namespaces);

        assertNotNull(config.getNamespaces());
        assertTrue(config.getNamespaces().isEmpty());
    }

    @Test
    public void testMultipleSetAndGet() {
        KubernetesConfig config = new KubernetesConfig();

        config.setDiscoveryServerUrl("http://test:8080");
        config.setEnabled(false);
        config.setNamespaces(Arrays.asList("namespace1", "namespace2"));

        assertEquals("http://test:8080", config.getDiscoveryServerUrl());
        assertFalse(config.isEnabled());
        assertEquals(2, config.getNamespaces().size());
    }
}
