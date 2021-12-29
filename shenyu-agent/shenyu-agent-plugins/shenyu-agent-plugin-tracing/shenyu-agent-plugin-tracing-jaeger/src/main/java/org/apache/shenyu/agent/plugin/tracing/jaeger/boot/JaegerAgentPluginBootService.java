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

package org.apache.shenyu.agent.plugin.tracing.jaeger.boot;

import io.jaegertracing.Configuration;
import io.opentracing.util.GlobalTracer;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.agent.api.config.AgentPluginConfig;
import org.apache.shenyu.agent.api.exception.ShenyuAgentException;
import org.apache.shenyu.agent.api.spi.AgentPluginBootService;
import org.apache.shenyu.spi.Join;

import java.util.Optional;

/**
 * The type Jaeger agent plugin boot service.
 */
@Join
public class JaegerAgentPluginBootService implements AgentPluginBootService {
    
    private Configuration configuration;
    
    @Override
    public void start(final AgentPluginConfig agentPluginConfig) {
        if (StringUtils.isEmpty(agentPluginConfig.getHost()) || agentPluginConfig.getPort() < 0) {
            throw new ShenyuAgentException("jaeger config error, host can not config or port is %s", agentPluginConfig.getPort());
        }
        agentPluginConfig.getProps().forEach((key, value) -> System.setProperty(String.valueOf(key), String.valueOf(value)));
        Configuration.SamplerConfiguration samplerConfig = Configuration.SamplerConfiguration.fromEnv();
        Configuration.ReporterConfiguration reporterConfig = Configuration.ReporterConfiguration.fromEnv()
                .withSender(Configuration.SenderConfiguration.fromEnv().withAgentHost(agentPluginConfig.getHost()).withAgentPort(agentPluginConfig.getPort()));
        String serviceName = Optional.ofNullable(agentPluginConfig.getProps().getProperty("SERVICE_NAME")).orElse("shenyu-agent");
        configuration = new Configuration(serviceName).withSampler(samplerConfig).withReporter(reporterConfig);
        if (!GlobalTracer.isRegistered()) {
            GlobalTracer.registerIfAbsent(configuration.getTracer());
        }
    }
    
    @Override
    public void close() {
        Optional.ofNullable(configuration).ifPresent(Configuration::closeTracer);
    }
}
