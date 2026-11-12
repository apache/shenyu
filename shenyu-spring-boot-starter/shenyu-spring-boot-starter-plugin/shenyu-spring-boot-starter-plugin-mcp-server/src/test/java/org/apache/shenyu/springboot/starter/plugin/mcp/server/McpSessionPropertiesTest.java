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

package org.apache.shenyu.springboot.starter.plugin.mcp.server;

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.plugin.mcp.server.manager.ShenyuMcpServerManager;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.http.codec.ServerCodecConfigurer;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

class McpSessionPropertiesTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner()
            .withUserConfiguration(McpServerPluginConfiguration.class)
            .withBean(ShenyuConfig.class, ShenyuConfig::new)
            .withBean(ServerCodecConfigurer.class, ServerCodecConfigurer::create);

    @Test
    void bindsExplicitPolicyAndClosesManagerOnContextShutdown() {
        final ShenyuMcpServerManager[] captured = new ShenyuMcpServerManager[1];
        runner.withPropertyValues("shenyu.plugins.mcp.server.session.max-sessions=3", "shenyu.plugins.mcp.server.session.idle-timeout=10s")
                .run(context -> {
                    assertTrue(context.isRunning());
                    final McpSessionProperties policy = context.getBean(McpSessionProperties.class);
                    assertEquals(3, policy.maxSessions());
                    assertEquals(Duration.ofSeconds(10), policy.idleTimeout());
                    captured[0] = context.getBean(ShenyuMcpServerManager.class);
                    captured[0].getOrCreateStreamableHttpTransport("/bounded/streamablehttp");
                });
        assertThrows(IllegalStateException.class, () -> captured[0].getOrCreateStreamableHttpTransport("/new/streamablehttp"));
    }

    @Test
    void preservesDefaultLimits() {
        runner.run(context -> {
            final McpSessionProperties properties = context.getBean(McpSessionProperties.class);
            assertEquals(0, properties.maxSessions());
            assertTrue(properties.idleTimeout().isZero());
        });
    }

    @Test
    void invalidConfigurationFailsBeforeAnyServerIsCreated() {
        for (String property : new String[]{"max-sessions=-1", "idle-timeout=-1s", "sweep-interval=0s", "unknown=true", "max-sessions=oops"}) {
            runner.withPropertyValues("shenyu.plugins.mcp.server.session." + property).run(context -> assertNotNull(context.getStartupFailure(), property));
        }
        runner.withPropertyValues("shenyu.plugins.mcp.server.session.idle-timeout=1s", "shenyu.plugins.mcp.server.session.sweep-interval=2s")
                .run(context -> assertNotNull(context.getStartupFailure()));
    }
}
