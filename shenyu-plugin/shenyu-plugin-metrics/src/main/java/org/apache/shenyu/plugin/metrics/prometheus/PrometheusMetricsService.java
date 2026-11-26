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

package org.apache.shenyu.plugin.metrics.prometheus;

import io.prometheus.client.Collector;
import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.exporter.HTTPServer;
import io.prometheus.client.hotspot.DefaultExports;
import io.prometheus.jmx.JmxCollector;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.config.ShenyuConfig.MetricsConfig;
import org.apache.shenyu.plugin.metrics.spi.MetricsRegister;
import org.apache.shenyu.plugin.metrics.spi.MetricsService;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.management.MalformedObjectNameException;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Prometheus metric service.
 */
@Join
public final class PrometheusMetricsService implements MetricsService {

    private static final Logger LOG = LoggerFactory.getLogger(PrometheusMetricsService.class);

    private final List<Collector> ownedCollectors = new ArrayList<>();

    private HTTPServer server;
    
    private final AtomicBoolean registered = new AtomicBoolean(false);
    
    @Override
    public synchronized void start(final MetricsConfig metricsConfig, final MetricsRegister metricsRegister) {
        if (Objects.isNull(server)) {
            startServer(metricsConfig);
        }
    }

    @Override
    public synchronized void stop() {
        if (Objects.nonNull(server)) {
            server.close();
            server = null;
        }
        // Keep the shared registry and the reporter-owned metrics available across restarts.
        unregisterOwnedCollectors();
    }

    /**
     * Gets server.
     *
     * @return the server
     */
    public HTTPServer getServer() {
        return server;
    }

    /**
     * Gets registered.
     *
     * @return the registered
     */
    public AtomicBoolean getRegistered() {
        return registered;
    }

    private void startServer(final MetricsConfig config) {
        int port = config.getPort();
        String host = config.getHost();
        InetSocketAddress inetSocketAddress;
        if (StringUtils.isEmpty(host)) {
            inetSocketAddress = new InetSocketAddress(port);
        } else {
            inetSocketAddress = new InetSocketAddress(host, port);
        }
        register(config);
        try {
            server = new HTTPServer(inetSocketAddress, CollectorRegistry.defaultRegistry, true);
            LOG.info(String.format("Prometheus metrics HTTP server `%s:%s` start success.", inetSocketAddress.getHostString(), inetSocketAddress.getPort()));
        } catch (final IOException ex) {
            unregisterOwnedCollectors();
            LOG.error("Prometheus metrics HTTP server start fail", ex);
        }
    }
    
    private void register(final MetricsConfig config) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        try {
            registerConfiguredCollectors(config);
        } catch (RuntimeException ex) {
            unregisterOwnedCollectors();
            throw ex;
        }
    }

    private void registerConfiguredCollectors(final MetricsConfig config) {
        registerCollector(new LoggingQueueMetricsCollector());
        registerCollector(new AgentCatalogMetricsCollector());
        String jvmEnabled = String.valueOf(config.getProps().getProperty("jvm_enabled"));
        if (StringUtils.isNotEmpty(jvmEnabled)) {
            boolean enabled = Boolean.parseBoolean(jvmEnabled);
            if (enabled) {
                // DefaultExports is initialized once for the shared JVM registry.
                DefaultExports.initialize();
            }
        }
        try {
            String jmxConfig = config.getJmxConfig();
            if (!"null".equals(jmxConfig) && StringUtils.isNotEmpty(jmxConfig)) {
                registerCollector(new JmxCollector(jmxConfig));
            }
        } catch (MalformedObjectNameException e) {
            LOG.error("init jmx collector error", e);
        }
    }

    private void registerCollector(final Collector collector) {
        collector.register();
        ownedCollectors.add(collector);
    }

    private void unregisterOwnedCollectors() {
        ownedCollectors.forEach(CollectorRegistry.defaultRegistry::unregister);
        ownedCollectors.clear();
        registered.set(false);
    }
}

