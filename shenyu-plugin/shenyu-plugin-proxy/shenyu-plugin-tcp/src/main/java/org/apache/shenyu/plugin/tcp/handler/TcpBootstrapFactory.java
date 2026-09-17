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
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

/**
 * TcpBootstrapFactory.
 */
public final class TcpBootstrapFactory {

    private static final Logger LOG = LoggerFactory.getLogger(TcpBootstrapFactory.class);

    private static final TcpBootstrapFactory SINGLETON = new TcpBootstrapFactory();

    private final ConcurrentMap<String, BootstrapServer> cache = new ConcurrentHashMap<>();

    private final ConcurrentMap<String, CompletableFuture<BootstrapServer>> creations = new ConcurrentHashMap<>();

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
        bootstrapServer.start(configuration);
        return bootstrapServer;
    }

    /**
     * Create and cache a bootstrap server if absent.
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
        try {
            BootstrapServer cachedServer = cache.get(selectorName);
            if (Objects.nonNull(cachedServer)) {
                creation.complete(cachedServer);
                return false;
            }
            UpstreamProvider.getSingleton().createUpstreams(selectorName, Collections.emptyList());
            BootstrapServer bootstrapServer = createBootstrapServer(configuration);
            BootstrapServer existingServer = cache.putIfAbsent(selectorName, bootstrapServer);
            if (Objects.nonNull(existingServer)) {
                bootstrapServer.shutdown();
                creation.complete(existingServer);
                return false;
            }
            creation.complete(bootstrapServer);
            return true;
        } catch (RuntimeException ex) {
            creation.completeExceptionally(ex);
            throw ex;
        } finally {
            creations.remove(selectorName, creation);
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

    /**
     * cache bootstrapServer by selectorName.
     *
     * @param selectorName    selectorName
     * @param bootstrapServer bootstrapServer
     */
    public void cache(final String selectorName, final BootstrapServer bootstrapServer) {
        cache.put(selectorName, bootstrapServer);
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
        return cache.remove(selectorName);
    }

    /**
     * Remove and shutdown a bootstrap server.
     *
     * @param selectorName selectorName
     * @return true if a bootstrap server was removed
     */
    public boolean removeAndShutdown(final String selectorName) {
        BootstrapServer bootstrapServer = cache.remove(selectorName);
        if (Objects.isNull(bootstrapServer)) {
            return false;
        }
        bootstrapServer.shutdown();
        return true;
    }

    /**
     * Clear cache.
     */
    public void clearCache() {
        cache.forEach((selectorName, bootstrapServer) -> {
            if (cache.remove(selectorName, bootstrapServer)) {
                try {
                    bootstrapServer.shutdown();
                } catch (RuntimeException ex) {
                    LOG.error("Failed to shutdown TcpBootstrapServer for selector {}", selectorName, ex);
                }
            }
        });
    }

    /**
     * getCache.
     *
     * @param selectorName selectorName
     * @return BootstrapServer
     */
    public BootstrapServer getCache(final String selectorName) {
        return cache.get(selectorName);
    }

}
