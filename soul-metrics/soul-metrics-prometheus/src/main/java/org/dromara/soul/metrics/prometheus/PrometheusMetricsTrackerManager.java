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

package org.dromara.soul.metrics.prometheus;

import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.exporter.HTTPServer;
import io.prometheus.client.hotspot.DefaultExports;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.concurrent.atomic.AtomicBoolean;
import javax.management.MalformedObjectNameException;
import lombok.Getter;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.metrics.api.MetricsTrackerFactory;
import org.dromara.soul.metrics.config.MetricsConfig;
import org.dromara.soul.metrics.prometheus.impl.collector.BuildInfoCollector;
import org.dromara.soul.metrics.prometheus.impl.collector.JmxCollector;
import org.dromara.soul.metrics.spi.MetricsTrackerManager;
import org.dromara.soul.spi.Join;

/**
 * Prometheus metrics tracker manager.
 */
@Getter
@Slf4j
@Join
public final class PrometheusMetricsTrackerManager implements MetricsTrackerManager {

    private final MetricsTrackerFactory metricsTrackerFactory = new PrometheusMetricsTrackerFactory();

    private HTTPServer server;

    private volatile AtomicBoolean registered = new AtomicBoolean(false);

    @SneakyThrows(IOException.class)
    @Override
    public void start(final MetricsConfig metricsConfig) {
        register(metricsConfig.getJmxConfig());
        InetSocketAddress inetSocketAddress;
        if ("".equals(metricsConfig.getHost()) || null == metricsConfig.getHost()) {
            inetSocketAddress = new InetSocketAddress(metricsConfig.getPort());
        } else {
            inetSocketAddress = new InetSocketAddress(metricsConfig.getHost(), metricsConfig.getPort());
        }
        server = new HTTPServer(inetSocketAddress, CollectorRegistry.defaultRegistry, true);
        log.info("you start prometheus metrics http server  host is :{}, port is :{} ", inetSocketAddress.getHostString(), inetSocketAddress.getPort());
    }

    @Override
    public void stop() {
        if (server != null) {
            server.stop();
        }
    }

    private void register(final String jmxConfig) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        new BuildInfoCollector().register();
        try {
            new JmxCollector(jmxConfig).register();
            DefaultExports.initialize();
        } catch (MalformedObjectNameException e) {
            log.error("init jxm collector error", e);
        }
    }
}

