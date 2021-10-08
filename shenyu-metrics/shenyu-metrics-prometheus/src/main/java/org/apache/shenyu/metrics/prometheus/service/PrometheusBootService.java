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

package org.apache.shenyu.metrics.prometheus.service;

import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.exporter.HTTPServer;
import io.prometheus.client.hotspot.DefaultExports;
import io.prometheus.jmx.JmxCollector;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.metrics.config.MetricsConfig;
import org.apache.shenyu.metrics.prometheus.collector.BuildInfoCollector;
import org.apache.shenyu.metrics.reporter.MetricsReporter;
import org.apache.shenyu.metrics.spi.MetricsBootService;
import org.apache.shenyu.metrics.spi.MetricsRegister;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.management.MalformedObjectNameException;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Prometheus metrics tracker manager.
 */
@Join
public final class PrometheusBootService implements MetricsBootService {

    private static final Logger LOG = LoggerFactory.getLogger(PrometheusBootService.class);

    private HTTPServer server;
    
    private volatile AtomicBoolean registered = new AtomicBoolean(false);
    
    @Override
    public void start(final MetricsConfig metricsConfig, final MetricsRegister metricsRegister) {
        startServer(metricsConfig);
        MetricsReporter.register(metricsRegister);
    }
    
    @Override
    public void stop() {
        if (server != null) {
            server.stop();
            registered.set(false);
            CollectorRegistry.defaultRegistry.clear();
            MetricsReporter.clean();
        }
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

    private void startServer(final MetricsConfig metricsConfig) {
        register(metricsConfig.getJmxConfig());
        int port = metricsConfig.getPort();
        String host = metricsConfig.getHost();
        InetSocketAddress inetSocketAddress;
        if (null == host || "".equalsIgnoreCase(host)) {
            inetSocketAddress = new InetSocketAddress(port);
        } else {
            inetSocketAddress = new InetSocketAddress(host, port);
        }
        try {
            server = new HTTPServer(inetSocketAddress, CollectorRegistry.defaultRegistry, true);
            LOG.info(String.format("Prometheus metrics HTTP server `%s:%s` start success.", inetSocketAddress.getHostString(), inetSocketAddress.getPort()));
        } catch (final IOException ex) {
            LOG.error("Prometheus metrics HTTP server start fail", ex);
        }
    }
    
    private void register(final String jmxConfig) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        new BuildInfoCollector().register();
        DefaultExports.initialize();
        try {
            if (StringUtils.isNotEmpty(jmxConfig)) {
                new JmxCollector(jmxConfig).register();
            }
        } catch (MalformedObjectNameException e) {
            LOG.error("init jmx collector error", e);
        }
    }
}

