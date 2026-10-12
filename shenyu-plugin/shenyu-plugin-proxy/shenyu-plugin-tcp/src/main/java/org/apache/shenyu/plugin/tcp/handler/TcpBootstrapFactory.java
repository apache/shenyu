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

package org.apache.shenyu.plugin.tcp.handler;

import com.google.common.eventbus.EventBus;
import org.apache.shenyu.protocol.tcp.BootstrapServer;
import org.apache.shenyu.protocol.tcp.TcpBootstrapServer;
import org.apache.shenyu.protocol.tcp.TcpServerConfiguration;
import org.apache.shenyu.protocol.tcp.UpstreamProvider;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.HashSet;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.locks.ReentrantLock;

/**
 * TcpBootstrapFactory.
 */
public final class TcpBootstrapFactory {

    private static final Logger LOG = LoggerFactory.getLogger(TcpBootstrapFactory.class);

    private static final TcpBootstrapFactory SINGLETON = new TcpBootstrapFactory();

    private final ConcurrentMap<String, CachedServer> cache = new ConcurrentHashMap<>();

    private final ConcurrentMap<String, CompletableFuture<BootstrapServer>> creations = new ConcurrentHashMap<>();

    // Initial creation uses single-flight; only replacement and removal share these locks.
    // Retain locks so waiting callers always synchronize on the same instance.
    private final ConcurrentMap<String, ReentrantLock> selectorLocks = new ConcurrentHashMap<>();

    private TcpBootstrapFactory() {
    }

    /**
     * getSingleton.
     *
     * @return TcpBootstrapFactory
     */
    public static TcpBootstrapFactory getSingleton() {
        return SINGLETON;
    }

    /**
     * createBootstrapServer.
     *
     * @param configuration configuration
     * @return BootstrapServer
     */
    public BootstrapServer createBootstrapServer(final TcpServerConfiguration configuration) {
        EventBus eventBus = new EventBus();
        BootstrapServer bootstrapServer = new TcpBootstrapServer(eventBus);
        try {
            bootstrapServer.start(configuration);
        } catch (RuntimeException ex) {
            // Initialization can fail before start() enters its bind failure cleanup.
            try {
                bootstrapServer.shutdown();
            } catch (RuntimeException cleanupFailure) {
                ex.addSuppressed(cleanupFailure);
            }
            throw ex;
        }
        return bootstrapServer;
    }

    /**
     * Create a bootstrap server or replace it when its listener configuration changes.
     *
     * @param configuration configuration
     * @return true if a bootstrap server was created or replaced
     */
    public boolean createOrUpdateBootstrapServer(final TcpServerConfiguration configuration) {
        String selectorName = configuration.getPluginSelectorName();
        TcpServerConfiguration snapshot = snapshot(configuration);
        while (true) {
            CachedServer cachedServer = cache.get(selectorName);
            if (Objects.isNull(cachedServer)) {
                if (createBootstrapServerIfAbsent(snapshot)) {
                    return true;
                }
                // Another creation may have applied a different configuration.
                continue;
            }
            ReentrantLock lock = selectorLocks.computeIfAbsent(selectorName, key -> new ReentrantLock());
            lock.lock();
            try {
                // A replacement or removal may have changed the cached listener while this caller waited.
                cachedServer = cache.get(selectorName);
                if (Objects.isNull(cachedServer)) {
                    continue;
                }
                if (cachedServer.matches(snapshot)) {
                    return false;
                }
                if (cachedServer.configuration.getPort() == snapshot.getPort()) {
                    replaceOnSamePort(selectorName, cachedServer, snapshot);
                } else {
                    // Different port: keep the current listener available until the replacement starts.
                    BootstrapServer replacement = createBootstrapServer(snapshot);
                    cache.put(selectorName, new CachedServer(replacement, snapshot));
                    try {
                        cachedServer.server.shutdown();
                    } catch (RuntimeException ex) {
                        LOG.error("Failed to shutdown replaced TcpBootstrapServer for selector {}", selectorName, ex);
                    }
                }
                return true;
            } finally {
                lock.unlock();
            }
        }
    }

    /**
     * Create and cache a bootstrap server if absent, sharing the in-flight creation result.
     *
     * @param configuration configuration
     * @return true if a bootstrap server was created
     */
    public boolean createBootstrapServerIfAbsent(final TcpServerConfiguration configuration) {
        String selectorName = configuration.getPluginSelectorName();
        if (cache.containsKey(selectorName)) {
            return false;
        }
        CompletableFuture<BootstrapServer> creation = new CompletableFuture<>();
        CompletableFuture<BootstrapServer> existingCreation = creations.putIfAbsent(selectorName, creation);
        if (Objects.nonNull(existingCreation)) {
            awaitCreation(existingCreation);
            return false;
        }
        UpstreamProvider upstreamProvider = UpstreamProvider.getSingleton();
        boolean initializeUpstreams = false;
        try {
            CachedServer cachedServer = cache.get(selectorName);
            if (Objects.nonNull(cachedServer)) {
                creation.complete(cachedServer.server);
                return false;
            }
            // Discovery upstreams can arrive before the listener; preserve that existing entry.
            initializeUpstreams = !upstreamProvider.inCache(selectorName);
            if (initializeUpstreams) {
                upstreamProvider.createUpstreams(selectorName, Collections.emptyList());
            }
            TcpServerConfiguration snapshot = snapshot(configuration);
            BootstrapServer bootstrapServer = createBootstrapServer(snapshot);
            CachedServer existingServer = cache.putIfAbsent(selectorName, new CachedServer(bootstrapServer, snapshot));
            if (Objects.nonNull(existingServer)) {
                bootstrapServer.shutdown();
                creation.complete(existingServer.server);
                return false;
            }
            creation.complete(bootstrapServer);
            return true;
        } catch (RuntimeException ex) {
            if (initializeUpstreams) {
                upstreamProvider.removeUpstreams(selectorName);
            }
            creation.completeExceptionally(ex);
            throw ex;
        } finally {
            creations.remove(selectorName, creation);
        }
    }

    private void replaceOnSamePort(final String selectorName, final CachedServer previous, final TcpServerConfiguration configuration) {
        try {
            // Same port: release the current listener before the replacement can bind.
            previous.server.shutdown();
            cache.put(selectorName, new CachedServer(createBootstrapServer(configuration), configuration));
        } catch (RuntimeException ex) {
            try {
                cache.put(selectorName, new CachedServer(createBootstrapServer(previous.configuration), previous.configuration));
            } catch (RuntimeException recoveryFailure) {
                cache.remove(selectorName);
                ex.addSuppressed(recoveryFailure);
            }
            throw ex;
        }
    }

    private static void awaitCreation(final CompletableFuture<BootstrapServer> creation) {
        try {
            creation.join();
        } catch (CompletionException ex) {
            Throwable cause = ex.getCause();
            if (cause instanceof RuntimeException) {
                throw (RuntimeException) cause;
            }
            if (cause instanceof Error) {
                throw (Error) cause;
            }
            throw ex;
        }
    }

    private static TcpServerConfiguration snapshot(final TcpServerConfiguration configuration) {
        TcpServerConfiguration snapshot = new TcpServerConfiguration();
        snapshot.setPluginSelectorName(configuration.getPluginSelectorName());
        snapshot.setPort(configuration.getPort());
        Properties props = new Properties();
        if (Objects.nonNull(configuration.getProps())) {
            props.putAll(configuration.getProps());
        }
        snapshot.setProps(props);
        return snapshot;
    }

    /**
     * Cache a bootstrap server with its listener configuration.
     *
     * @param configuration configuration
     * @param bootstrapServer bootstrapServer
     */
    public void cache(final TcpServerConfiguration configuration, final BootstrapServer bootstrapServer) {
        cache.put(configuration.getPluginSelectorName(), new CachedServer(bootstrapServer, snapshot(configuration)));
    }

    /**
     * inCache.
     *
     * @param selectorName selectorName
     * @return is selectorName has been cached
     */
    public Boolean inCache(final String selectorName) {
        return cache.containsKey(selectorName);
    }

    /**
     * removeCache.
     *
     * @param selectorName selectorName
     * @return BootstrapServer
     */
    public BootstrapServer removeCache(final String selectorName) {
        CachedServer cachedServer = cache.remove(selectorName);
        return Objects.isNull(cachedServer) ? null : cachedServer.server;
    }

    /**
     * Remove and shutdown a bootstrap server.
     *
     * @param selectorName selectorName
     * @return true if a bootstrap server was removed
     */
    public boolean removeAndShutdown(final String selectorName) {
        ReentrantLock lock = selectorLocks.computeIfAbsent(selectorName, key -> new ReentrantLock());
        lock.lock();
        try {
            BootstrapServer bootstrapServer = removeCache(selectorName);
            UpstreamProvider.getSingleton().removeUpstreams(selectorName);
            if (Objects.isNull(bootstrapServer)) {
                return false;
            }
            bootstrapServer.shutdown();
            return true;
        } finally {
            lock.unlock();
        }
    }

    /**
     * Clear cache.
     */
    public void clearCache() {
        Set<String> selectorNames = new HashSet<>(cache.keySet());
        selectorNames.forEach(selectorName -> {
            try {
                removeAndShutdown(selectorName);
            } catch (RuntimeException ex) {
                LOG.error("Failed to shutdown TcpBootstrapServer for selector {}", selectorName, ex);
            }
        });
        UpstreamProvider.getSingleton().clear();
    }

    /**
     * getCache.
     *
     * @param selectorName selectorName
     * @return BootstrapServer
     */
    public BootstrapServer getCache(final String selectorName) {
        CachedServer cachedServer = cache.get(selectorName);
        return Objects.isNull(cachedServer) ? null : cachedServer.server;
    }

    private static final class CachedServer {

        private final BootstrapServer server;

        private final TcpServerConfiguration configuration;

        private CachedServer(final BootstrapServer server, final TcpServerConfiguration configuration) {
            this.server = server;
            this.configuration = configuration;
        }

        private boolean matches(final TcpServerConfiguration incoming) {
            return configuration.getPort() == incoming.getPort() && configuration.getProps().equals(incoming.getProps());
        }
    }

}
