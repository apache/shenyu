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

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * TcpBootstrapFactory.
 */
public final class TcpBootstrapFactory {

    private static final TcpBootstrapFactory SINGLETON = new TcpBootstrapFactory();

    private final Map<String, BootstrapServer> cache = new ConcurrentHashMap<>();

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
     * getCache.
     *
     * @param selectorName selectorName
     * @return BootstrapServer
     */
    public BootstrapServer getCache(final String selectorName) {
        return cache.get(selectorName);
    }

}
