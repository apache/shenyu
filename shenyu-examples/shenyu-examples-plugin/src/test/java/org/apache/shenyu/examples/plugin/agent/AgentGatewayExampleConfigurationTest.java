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

import org.apache.shenyu.plugin.agent.gateway.tool.AgentToolProvider;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * No example credentials or tools are assembled without explicit local opt-in.
 */
class AgentGatewayExampleConfigurationTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner().withUserConfiguration(AgentGatewayExampleConfiguration.class);

    @Test
    void shouldRemainDisabledByDefault() {
        runner.run(context -> assertFalse(context.containsBean("orderStatusTool")));
    }

    @Test
    void shouldRequireExplicitLoopbackAndKey() {
        runner.withPropertyValues("shenyu.examples.agent.gateway.enabled=true", "server.address=127.0.0.1")
                .run(context -> assertNotNull(context.getStartupFailure()));
        runner.withPropertyValues("shenyu.examples.agent.gateway.enabled=true", "server.address=0.0.0.0",
                "shenyu.examples.agent.gateway.jwt-secret=example-test-key-01234567890123456789")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }

    @Test
    void shouldAssembleOnlyTheExplicitExample() {
        runner.withPropertyValues("shenyu.examples.agent.gateway.enabled=true", "server.address=127.0.0.1",
                "shenyu.examples.agent.gateway.jwt-secret=example-test-key-01234567890123456789")
                .run(context -> assertEquals("order_status", context.getBean(AgentToolProvider.class).getName()));
    }
}
