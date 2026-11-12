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

package org.apache.shenyu.springboot.starter.plugin.httpclient;

import org.apache.shenyu.plugin.httpclient.config.UpstreamPoolGroupRegistry;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.web.ServerProperties;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import reactor.netty.http.client.HttpClient;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Startup wiring tests for the named upstream pool groups: the registry is built only
 * from trusted startup properties, rejects duplicate names and oversized group counts,
 * and stays empty when no groups are configured.
 */
@Configuration
@EnableConfigurationProperties
public class HttpClientPoolGroupConfigTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
                .withConfiguration(AutoConfigurations.of(HttpClientPluginConfiguration.class))
                .withBean(HttpClientPoolGroupConfigTest.class)
                .withBean(ServerProperties.class);
    }

    @Test
    public void buildsRegistryFromStartupProperties() {
        applicationContextRunner
                .withPropertyValues(
                        "shenyu.httpclient.pool.maxConnections=2",
                        "shenyu.httpclient.pool-groups[0].name=bulk",
                        "shenyu.httpclient.pool-groups[0].maxConnections=1",
                        "shenyu.httpclient.pool-groups[0].acquireTimeout=300",
                        "shenyu.httpclient.pool-groups[1].name=interactive",
                        "shenyu.httpclient.pool-groups[1].maxConnections=4"
                )
                .run(context -> {
                    UpstreamPoolGroupRegistry registry = context.getBean(UpstreamPoolGroupRegistry.class);
                    assertTrue(registry.contains("bulk"));
                    assertTrue(registry.contains("INTERACTIVE"));
                    assertFalse(registry.contains("ghost"));
                });
    }

    @Test
    public void emptyConfigurationKeepsTheDefaultPath() {
        applicationContextRunner.run(context -> {
            UpstreamPoolGroupRegistry registry = context.getBean(UpstreamPoolGroupRegistry.class);
            assertFalse(registry.contains("bulk"));
            registry.close();
        });
    }

    @Test
    public void duplicateNamesFailTheStartup() {
        applicationContextRunner
                .withPropertyValues(
                        "shenyu.httpclient.pool-groups[0].name=bulk",
                        "shenyu.httpclient.pool-groups[1].name=BULK"
                )
                .run(context -> assertThrows(RuntimeException.class, () -> context.getBean(UpstreamPoolGroupRegistry.class)));
    }

    @Test
    public void oversizedGroupCountFailsTheStartup() {
        List<String> values = new ArrayList<>();
        IntStream.range(0, HttpClientFactory.MAX_POOL_GROUPS + 1)
                .forEach(i -> values.add("shenyu.httpclient.pool-groups[" + i + "].name=group-" + i));
        applicationContextRunner
                .withPropertyValues(values.toArray(new String[0]))
                .run(context -> assertThrows(RuntimeException.class, () -> context.getBean(UpstreamPoolGroupRegistry.class)));
    }

    @Test
    public void blankGroupNameFailsTheStartup() {
        applicationContextRunner
                .withPropertyValues("shenyu.httpclient.pool-groups[0].name=")
                .run(context -> assertThrows(RuntimeException.class, () -> context.getBean(UpstreamPoolGroupRegistry.class)));
    }

    @ParameterizedTest
    @ValueSource(strings = {"DISABLED", "ELASTIC"})
    public void groupTypeMustBeFixed(final String type) {
        applicationContextRunner
                .withPropertyValues(
                        "shenyu.httpclient.pool-groups[0].name=bulk",
                        "shenyu.httpclient.pool-groups[0].type=" + type)
                .run(context -> assertThrows(RuntimeException.class, () -> context.getBean(UpstreamPoolGroupRegistry.class)));
    }

    @ParameterizedTest
    @ValueSource(strings = {"0", "-1"})
    public void groupMaxConnectionsMustBePositive(final String maxConnections) {
        applicationContextRunner
                .withPropertyValues(
                        "shenyu.httpclient.pool-groups[0].name=bulk",
                        "shenyu.httpclient.pool-groups[0].maxConnections=" + maxConnections)
                .run(context -> assertThrows(RuntimeException.class, () -> context.getBean(UpstreamPoolGroupRegistry.class)));
    }

    @ParameterizedTest
    @ValueSource(strings = {"0", "-1"})
    public void groupMaxPendingAcquiresMustBePositiveOrOmitted(final String maxPendingAcquires) {
        applicationContextRunner
                .withPropertyValues(
                        "shenyu.httpclient.pool-groups[0].name=bulk",
                        "shenyu.httpclient.pool-groups[0].maxPendingAcquires=" + maxPendingAcquires)
                .run(context -> assertThrows(RuntimeException.class, () -> context.getBean(UpstreamPoolGroupRegistry.class)));
    }

    @Test
    public void omittedGroupNameFailsTheStartup() {
        applicationContextRunner
                .withPropertyValues("shenyu.httpclient.pool-groups[0].maxConnections=1")
                .run(context -> assertThrows(RuntimeException.class, () -> context.getBean(UpstreamPoolGroupRegistry.class)));
    }

    @Test
    public void sentinelGroupNameFailsTheStartup() {
        applicationContextRunner
                .withPropertyValues("shenyu.httpclient.pool-groups[0].name=SHeNyu-Proxy")
                .run(context -> assertThrows(RuntimeException.class, () -> context.getBean(UpstreamPoolGroupRegistry.class)));
    }

    @Test
    public void webClientStrategyBuildsPerGroupWebClients() {
        applicationContextRunner
                .withPropertyValues(
                        "shenyu.httpclient.strategy=webClient",
                        "shenyu.httpclient.pool-groups[0].name=bulk")
                .run(context -> {
                    UpstreamPoolGroupRegistry registry = context.getBean(UpstreamPoolGroupRegistry.class);
                    assertNotNull(registry.httpClientFor("bulk"));
                    assertNotNull(registry.webClientFor("bulk"));
                });
    }

    @Test
    public void customHttpClientWithGroupsFailsTheStartup() {
        applicationContextRunner
                .withBean(HttpClient.class, () -> HttpClient.create())
                .withPropertyValues("shenyu.httpclient.pool-groups[0].name=bulk")
                .run(context -> assertThrows(RuntimeException.class, () -> context.getBean(UpstreamPoolGroupRegistry.class)));
    }

    @Test
    public void customHttpClientWithoutGroupsKeepsTheDefaultPath() {
        applicationContextRunner
                .withBean(HttpClient.class, () -> HttpClient.create())
                .run(context -> {
                    UpstreamPoolGroupRegistry registry = context.getBean(UpstreamPoolGroupRegistry.class);
                    assertFalse(registry.contains("bulk"));
                });
    }
}
