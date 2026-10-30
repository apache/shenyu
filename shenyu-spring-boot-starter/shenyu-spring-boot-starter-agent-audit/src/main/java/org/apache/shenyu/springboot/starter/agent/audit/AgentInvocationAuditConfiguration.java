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

package org.apache.shenyu.springboot.starter.agent.audit;

import org.apache.shenyu.common.audit.AgentInvocationAuditObserver;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.env.Environment;


/**
 * Opt-in, metadata-only audit delivery shared by agent traffic boundaries.
 */
@Configuration(proxyBeanMethods = false)
@ConditionalOnProperty(value = "shenyu.plugins.agent.audit.enabled", havingValue = "true")
public class AgentInvocationAuditConfiguration {

    /**
     * Expose the trusted sink to authentication plugins that run before Agent Gateway.
     * @param observer application or managed observer
     * @return metadata-only filter, without a second logical terminal owner
     */
    @Bean
    public org.springframework.web.server.WebFilter agentInvocationAuditFilter(final AgentInvocationAuditObserver observer) {
        return (exchange, chain) -> {
            exchange.getAttributes().put(AgentInvocationAuditObserver.EXCHANGE_ATTRIBUTE, observer);
            return chain.filter(exchange);
        };
    }

    /**
     * Create a bounded collector unless the application supplies its own nonblocking sink.
     *
     * @param environment deployment-owned queue settings
     * @param consumers optional application-provided delivery consumer
     * @return shared audit observer
     */
    @Bean(initMethod = "start", destroyMethod = "close")
    @ConditionalOnMissingBean(AgentInvocationAuditObserver.class)
    public AgentInvocationAuditCollector agentInvocationAuditCollector(final Environment environment,
                                                                              final ObjectProvider<AgentInvocationAuditConsumer> consumers) {
        int capacity = environment.getProperty("shenyu.plugins.agent.audit.queue-capacity", Integer.class, 8192);
        AgentInvocationAuditConsumer consumer = consumers.getIfAvailable(() -> AgentInvocationAuditConsumer.LOGGING);
        return new AgentInvocationAuditCollector(capacity, consumer);
    }
}
