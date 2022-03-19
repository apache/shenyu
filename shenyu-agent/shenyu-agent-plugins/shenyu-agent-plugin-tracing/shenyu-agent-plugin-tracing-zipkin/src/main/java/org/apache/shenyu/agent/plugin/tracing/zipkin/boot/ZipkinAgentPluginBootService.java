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

package org.apache.shenyu.agent.plugin.tracing.zipkin.boot;

import brave.Tracing;
import brave.sampler.BoundarySampler;
import brave.sampler.RateLimitingSampler;
import brave.sampler.Sampler;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.agent.api.config.AgentPluginConfig;
import org.apache.shenyu.agent.api.exception.ShenyuAgentException;
import org.apache.shenyu.agent.api.spi.AgentPluginBootService;
import org.apache.shenyu.agent.plugin.tracing.zipkin.enums.SamplerTypeEnum;
import org.apache.shenyu.spi.Join;
import zipkin2.reporter.brave.AsyncZipkinSpanHandler;
import zipkin2.reporter.okhttp3.OkHttpSender;

import java.util.Objects;
import java.util.Optional;
import java.util.Properties;

/**
 * The type Zipkin agent plugin boot service.
 */
@Join
public class ZipkinAgentPluginBootService implements AgentPluginBootService {
    
    private static final String DEFAULT_URL_VERSION = "/api/v2/spans";

    private static final String DEFAULT_SERVICE_NAME = "shenyu-agent";

    private static final String DEFAULT_SAMPLER_TYPE = "const";

    private static final String DEFAULT_SAMPLER_PARAM = "1";

    private AsyncZipkinSpanHandler zipkinSpanHandler;

    private OkHttpSender sender;

    private Tracing tracing;

    /**
     * Zipkin start.
     *
     * @param agentPluginConfig the agent plugin config
     */
    @Override
    public void start(final AgentPluginConfig agentPluginConfig) {
        if (StringUtils.isEmpty(agentPluginConfig.getHost()) || agentPluginConfig.getPort() < 0) {
            throw new ShenyuAgentException("zipkin config error, host can not config or port is %s", agentPluginConfig.getPort());
        }
        Properties props = agentPluginConfig.getProps();
        String urlVersion = Optional.ofNullable(props.getProperty("version")).orElse(DEFAULT_URL_VERSION);
        sender = OkHttpSender.create(String.format("http://%s:%s%s", agentPluginConfig.getHost(), agentPluginConfig.getPort(), urlVersion));
        zipkinSpanHandler = AsyncZipkinSpanHandler.create(sender);

        Sampler sampler = createSampler(agentPluginConfig);
        String serviceName = Optional.ofNullable(props.getProperty("name")).orElse(DEFAULT_SERVICE_NAME);
        tracing = Tracing.newBuilder()
                .localServiceName(serviceName)
                .sampler(sampler)
                .addSpanHandler(zipkinSpanHandler)
                .build();

    }

    @Override
    public void close() {
        if (Objects.nonNull(tracing)) {
            tracing.close();
        }
        if (Objects.nonNull(zipkinSpanHandler)) {
            zipkinSpanHandler.close();
        }
        if (Objects.nonNull(sender)) {
            sender.close();
        }
    }

    /**
     * Create sampler by agent plugin config info.
     *
     * @param agentPluginConfig agent plugin config info.
     * @return sample.
     */
    private Sampler createSampler(final AgentPluginConfig agentPluginConfig) {
        String type = Optional.ofNullable(agentPluginConfig.getProps().getProperty("type")).orElse(DEFAULT_SAMPLER_TYPE);
        String param = Optional.ofNullable(agentPluginConfig.getProps().getProperty("param")).orElse(DEFAULT_SAMPLER_PARAM);
        SamplerTypeEnum typeEnum = SamplerTypeEnum.getEnumByName(type);
        switch (typeEnum) {
            case CONS:
                if (Objects.equals(param, "0")) {
                    return Sampler.NEVER_SAMPLE;
                }
                return Sampler.ALWAYS_SAMPLE;
            case COUNTING:
                return Sampler.create(Float.parseFloat(param));
            case RATE_LIMITING:
                return RateLimitingSampler.create(Integer.parseInt(param));
            case BOUNDARY:
                return BoundarySampler.create(Float.parseFloat(param));
            default:
                return Sampler.ALWAYS_SAMPLE;
        }
    }
}
