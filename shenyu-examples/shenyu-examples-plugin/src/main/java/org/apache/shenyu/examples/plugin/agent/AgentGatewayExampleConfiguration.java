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

package org.apache.shenyu.examples.plugin.agent;

import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpPrincipalSecurityResolver;
import org.apache.shenyu.plugin.agent.gateway.security.AgentMcpSecurityResolver;
import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Explicit, disabled-by-default demo beans for the real gateway bootstrap.
 * Use only synthetic data and an isolated loopback deployment.
 */
@Configuration(proxyBeanMethods = false)
@ConditionalOnProperty(name = "shenyu.examples.agent.gateway.enabled", havingValue = "true")
public class AgentGatewayExampleConfiguration {

    @Bean
    public AgentToolProvider orderStatusTool() {
        return new OrderStatusTool();
    }

    @Bean
    public AgentMcpSecurityResolver exampleMcpSecurityResolver() {
        return new AgentMcpPrincipalSecurityResolver();
    }

    @Bean
    public ExampleMcpAuthenticationFilter exampleMcpAuthenticationFilter(
            @Value("${shenyu.examples.agent.gateway.jwt-secret:}") final String secret,
            @Value("${server.address:}") final String address) {
        if (!"127.0.0.1".equals(address) && !"::1".equals(address)) {
            throw new IllegalArgumentException("The example requires an explicit loopback server address");
        }
        return new ExampleMcpAuthenticationFilter(secret);
    }
}
