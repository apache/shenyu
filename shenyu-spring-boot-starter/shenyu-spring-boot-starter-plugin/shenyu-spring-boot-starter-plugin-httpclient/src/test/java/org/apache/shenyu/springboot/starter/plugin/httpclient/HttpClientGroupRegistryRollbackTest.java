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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import org.apache.shenyu.plugin.httpclient.config.HttpClientProperties;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import reactor.netty.resources.ConnectionProvider;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Startup rollback tests for the named upstream pool groups.
 */
public final class HttpClientGroupRegistryRollbackTest {

    @Test
    public void partialBuildFailureDisposesAlreadyBuiltProviders() {
        HttpClientProperties properties = properties(
                group("bulk", 1),
                group("interactive", 1));
        AtomicReference<ConnectionProvider> built = new AtomicReference<>();
        AtomicInteger calls = new AtomicInteger();
        HttpClientFactory factory = new HttpClientFactory(properties, null, new ServerProperties()) {
            @Override
            ConnectionProvider buildConnectionProvider(final HttpClientProperties.Pool pool) {
                if (calls.incrementAndGet() == 1) {
                    ConnectionProvider provider = ConnectionProvider.create("rollback-test", 1);
                    built.set(provider);
                    return provider;
                }
                throw new IllegalStateException("second group failed");
            }
        };

        assertThrows(IllegalStateException.class, factory::buildGroupRegistry);

        assertTrue(built.get().isDisposed(), "the provider built before the failure must be disposed");
    }

    @Test
    public void validationRunsBeforeAnyProviderIsBuilt() {
        HttpClientProperties.Pool invalid = group("interactive", 1);
        invalid.setMaxConnections(0);
        HttpClientProperties properties = properties(group("bulk", 1), invalid);
        AtomicInteger calls = new AtomicInteger();
        HttpClientFactory factory = new HttpClientFactory(properties, null, new ServerProperties()) {
            @Override
            ConnectionProvider buildConnectionProvider(final HttpClientProperties.Pool pool) {
                calls.incrementAndGet();
                return ConnectionProvider.create("rollback-test", 1);
            }
        };

        assertThrows(IllegalArgumentException.class, factory::buildGroupRegistry);

        assertEquals(0, calls.get(), "invalid groups must be rejected before any provider is built");
    }

    private static HttpClientProperties properties(final HttpClientProperties.Pool... groups) {
        HttpClientProperties properties = new HttpClientProperties();
        properties.setPoolGroups(List.of(groups));
        return properties;
    }

    private static HttpClientProperties.Pool group(final String name, final int maxConnections) {
        HttpClientProperties.Pool group = new HttpClientProperties.Pool();
        group.setName(name);
        group.setMaxConnections(maxConnections);
        return group;
    }
}
