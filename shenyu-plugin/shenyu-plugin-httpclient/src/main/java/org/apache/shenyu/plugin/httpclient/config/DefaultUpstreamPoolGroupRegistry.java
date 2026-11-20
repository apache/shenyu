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

import reactor.netty.http.client.HttpClient;
import reactor.netty.resources.ConnectionProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Immutable registry of the startup-configured pool groups.
 *
 * <p>The maps are built once from trusted configuration and never mutated afterwards.
 * The registry owns the group connection providers and disposes each one exactly once
 * on close; it never disposes the shared default provider. After close every lookup is
 * rejected, so a late request cannot use a client backed by a disposed provider.</p>
 */
public final class DefaultUpstreamPoolGroupRegistry implements UpstreamPoolGroupRegistry, AutoCloseable {

    private static final Logger LOG = LoggerFactory.getLogger(DefaultUpstreamPoolGroupRegistry.class);

    private final Map<String, HttpClient> httpClients;

    private final Map<String, WebClient> webClients;

    private final List<ConnectionProvider> ownedProviders;

    private final AtomicBoolean closed = new AtomicBoolean(false);

    public DefaultUpstreamPoolGroupRegistry(final Map<String, HttpClient> httpClients,
                                            final Map<String, WebClient> webClients,
                                            final List<ConnectionProvider> ownedProviders) {
        this.httpClients = Map.copyOf(httpClients);
        this.webClients = Map.copyOf(webClients);
        this.ownedProviders = List.copyOf(ownedProviders);
    }

    @Override
    public boolean contains(final String name) {
        ensureOpen();
        return Objects.nonNull(name) && httpClients.containsKey(normalize(name));
    }

    @Override
    public HttpClient httpClientFor(final String name) {
        ensureOpen();
        HttpClient client = httpClients.get(normalize(Objects.requireNonNull(name, "name")));
        if (Objects.isNull(client)) {
            throw new IllegalStateException("No HttpClient built for upstream pool group " + name);
        }
        return client;
    }

    @Override
    public WebClient webClientFor(final String name) {
        ensureOpen();
        WebClient client = webClients.get(normalize(Objects.requireNonNull(name, "name")));
        if (Objects.isNull(client)) {
            throw new IllegalStateException("No WebClient built for upstream pool group " + name);
        }
        return client;
    }

    @Override
    public void close() {
        if (closed.compareAndSet(false, true)) {
            for (ConnectionProvider provider : ownedProviders) {
                try {
                    provider.dispose();
                } catch (RuntimeException ex) {
                    LOG.warn("Failed to dispose upstream pool group provider {}", provider, ex);
                }
            }
        }
    }

    private void ensureOpen() {
        if (closed.get()) {
            throw new IllegalStateException("Upstream pool group registry is closed");
        }
    }

    private static String normalize(final String name) {
        return name.trim().toLowerCase(Locale.ROOT);
    }
}
