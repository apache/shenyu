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

package org.apache.shenyu.agent.plugin.metrics.prometheus.boot;

import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.exporter.HTTPServer;
import io.prometheus.client.hotspot.DefaultExports;
import io.prometheus.jmx.BuildInfoCollector;
import io.prometheus.jmx.JmxCollector;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.agent.api.config.AgentPluginConfig;
import org.apache.shenyu.agent.api.spi.AgentPluginBootService;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.management.MalformedObjectNameException;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Prometheus plugin boot service.
 */
@Join
public final class PrometheusPluginBootService implements AgentPluginBootService {

    private static final Logger LOG = LoggerFactory.getLogger(PrometheusPluginBootService.class);

    private HTTPServer server;
    
    private final AtomicBoolean registered = new AtomicBoolean(false);
    
    @Override
    public void start(final AgentPluginConfig agentPluginConfig) {
        startServer(agentPluginConfig);
    }

    private void startServer(final AgentPluginConfig config) {
        register(config.getProps());
        int port = config.getPort();
        String host = config.getHost();
        InetSocketAddress inetSocketAddress;
        try {
            if (StringUtils.isNotEmpty(host)) {
                inetSocketAddress = new InetSocketAddress(host, port);
            } else {
                inetSocketAddress = new InetSocketAddress(port);
            }
            server = new HTTPServer(inetSocketAddress, CollectorRegistry.defaultRegistry, true);
            LOG.info(String.format("Prometheus metrics HTTP server `%s:%s` start success.", inetSocketAddress.getHostString(), inetSocketAddress.getPort()));
        } catch (final IOException ex) {
            LOG.error("Prometheus metrics HTTP server start fail", ex);
        }
    }
    
    private void register(final Properties props) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        new BuildInfoCollector().register();
        boolean enabled = Boolean.parseBoolean(props.getProperty("jvm_enabled"));
        if (enabled) {
            DefaultExports.initialize();
        }
        try {
            String jmxConfig = String.valueOf(props.get("jmx_config"));
            if (StringUtils.isNotEmpty(jmxConfig)) {
                new JmxCollector(jmxConfig).register();
            }
        } catch (MalformedObjectNameException e) {
            LOG.error("init jmx collector error", e);
        }
    }
    
    @Override
    public void close() {
        if (Objects.nonNull(server)) {
            server.stop();
            registered.set(false);
            CollectorRegistry.defaultRegistry.clear();
        }
    }
}

