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

package org.apache.shenyu.plugin.httpclient.config;

import org.junit.jupiter.api.Test;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.netty.http.client.HttpClient;
import reactor.netty.resources.ConnectionProvider;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

/**
 * Tests for the startup-built registry of named upstream pool groups.
 */
public final class DefaultUpstreamPoolGroupRegistryTest {

    @Test
    public void lookupsAreCaseInsensitiveAndReadOnly() {
        HttpClient client = HttpClient.newConnection();
        WebClient webClient = WebClient.builder().build();
        ConnectionProvider provider = mock(ConnectionProvider.class);
        try (DefaultUpstreamPoolGroupRegistry registry = new DefaultUpstreamPoolGroupRegistry(
                Map.of("bulk", client), Map.of("bulk", webClient), List.of(provider))) {
            assertTrue(registry.contains("bulk"));
            assertTrue(registry.contains(" BULK "));
            assertFalse(registry.contains("ghost"));
            assertFalse(registry.contains(null));
            assertSame(client, registry.httpClientFor("bulk"));
            assertSame(webClient, registry.webClientFor("BULK"));
            assertThrows(IllegalStateException.class, () -> registry.httpClientFor("ghost"));
            assertThrows(IllegalStateException.class, () -> registry.webClientFor("ghost"));
            assertThrows(NullPointerException.class, () -> registry.httpClientFor(null));
        }
        verify(provider, times(1)).dispose();
        verifyNoMoreInteractions(provider);
    }

    @Test
    public void closeDisposesEachProviderExactlyOnce() {
        ConnectionProvider first = mock(ConnectionProvider.class);
        ConnectionProvider second = mock(ConnectionProvider.class);
        DefaultUpstreamPoolGroupRegistry registry = new DefaultUpstreamPoolGroupRegistry(
                Map.of(), Map.of(), List.of(first, second));
        registry.close();
        registry.close();
        verify(first, times(1)).dispose();
        verify(second, times(1)).dispose();
    }

    @Test
    public void closeDisposesRemainingProvidersWhenOneThrows() {
        ConnectionProvider failing = mock(ConnectionProvider.class);
        ConnectionProvider remaining = mock(ConnectionProvider.class);
        doThrow(new IllegalStateException("dispose failed")).when(failing).dispose();
        DefaultUpstreamPoolGroupRegistry registry = new DefaultUpstreamPoolGroupRegistry(
                Map.of(), Map.of(), List.of(failing, remaining));

        registry.close();

        verify(failing, times(1)).dispose();
        verify(remaining, times(1)).dispose();
    }

    @Test
    public void closedRegistryRejectsEveryLookup() {
        DefaultUpstreamPoolGroupRegistry registry = new DefaultUpstreamPoolGroupRegistry(
                Map.of("bulk", HttpClient.newConnection()), Map.of("bulk", WebClient.builder().build()), List.of());

        registry.close();

        assertThrows(IllegalStateException.class, () -> registry.contains("bulk"));
        assertThrows(IllegalStateException.class, () -> registry.httpClientFor("bulk"));
        assertThrows(IllegalStateException.class, () -> registry.webClientFor("bulk"));
    }

    @Test
    public void emptyRegistryRejectsEverything() {
        UpstreamPoolGroupRegistry registry = UpstreamPoolGroupRegistry.empty();
        assertFalse(registry.contains("bulk"));
        assertThrows(IllegalStateException.class, () -> registry.httpClientFor("bulk"));
        assertThrows(IllegalStateException.class, () -> registry.webClientFor("bulk"));
        registry.close();
    }
}
