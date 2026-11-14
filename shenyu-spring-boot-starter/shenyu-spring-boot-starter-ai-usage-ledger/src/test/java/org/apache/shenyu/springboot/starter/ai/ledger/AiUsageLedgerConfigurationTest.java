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

package org.apache.shenyu.springboot.starter.ai.ledger;

import org.apache.shenyu.common.metrics.AiCallObserver;
import org.apache.shenyu.common.metrics.AiUsage;
import org.apache.shenyu.plugin.ai.proxy.enhanced.AiProxyPlugin;
import org.apache.shenyu.springboot.starter.plugin.ai.proxy.AiProxyPluginConfiguration;
import org.apache.shenyu.springboot.starter.plugin.agent.gateway.AgentGatewayPluginConfiguration;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.autoconfigure.context.ConfigurationPropertiesAutoConfiguration;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import reactor.core.publisher.Mono;

import java.util.concurrent.atomic.AtomicInteger;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;

class AiUsageLedgerConfigurationTest {

    private final ApplicationContextRunner runner = new ApplicationContextRunner().withConfiguration(AutoConfigurations.of(
            ConfigurationPropertiesAutoConfiguration.class, AiUsageLedgerConfiguration.class, AiProxyPluginConfiguration.class, AgentGatewayPluginConfiguration.class));

    @Test
    void disabledByDefaultAndExplicitlyEnabledWithoutTakingOverExistingSink() {
        runner.run(context -> assertThat(context).doesNotHaveBean(UsageLedgerCollector.class));
        AtomicInteger observed = new AtomicInteger();
        runner.withPropertyValues("shenyu.ai.usage-ledger.enabled=true", "shenyu.ai.usage-ledger.namespace=namespace")
                .withBean("existing", AiCallObserver.class, () -> event -> observed.incrementAndGet())
                .run(context -> {
                    assertThat(context).hasNotFailed().hasSingleBean(AiProxyPlugin.class).hasSingleBean(UsageLedgerCollector.class);
                    AiCallObserver selected = AiCallObserver.select(context.getBeansOfType(AiCallObserver.class).values().stream().toList());
                    selected.record(LedgerTestFixtures.completed("request", "a", 0, AiUsage.UNKNOWN));
                    assertEquals(1, observed.get());
                });
    }

    @ParameterizedTest
    @ValueSource(strings = {"queue-capacity=0", "queue-capacity=16385", "store-capacity=0", "store-capacity=32769",
        "retention=PT0S", "retention=P8D", "prices={}", "prices=[{}]", "prices=[] []", "namespace= "})
    void invalidDeploymentConfigurationFailsBeforeTraffic(final String option) {
        runner.withPropertyValues("shenyu.ai.usage-ledger.enabled=true", "shenyu.ai.usage-ledger." + option)
                .run(context -> assertThat(context).hasFailed());
    }

    @Test
    void httpExportsCannotStartWithoutAnApplicationAuthorizer() {
        runner.withPropertyValues("shenyu.ai.usage-ledger.enabled=true", "shenyu.ai.usage-ledger.exports-enabled=true")
                .run(context -> assertThat(context).hasFailed());
        runner.withPropertyValues("shenyu.ai.usage-ledger.enabled=true", "shenyu.ai.usage-ledger.exports-enabled=true")
                .withBean(LedgerExportAuthorizer.class, () -> principal -> Mono.empty())
                .run(context -> assertThat(context).hasNotFailed());
    }

    @Test
    void ordinaryAmbiguousObserversAndMultipleExplicitCompositesStillFail() {
        AiCallObserver first = event -> { };
        AiCallObserver second = event -> { };
        runner.withBean("first", AiCallObserver.class, () -> first).withBean("second", AiCallObserver.class, () -> second)
                .run(context -> assertThat(context).hasFailed());
        AiCallObserver.Composite composite = event -> { };
        runner.withPropertyValues("shenyu.ai.usage-ledger.enabled=true")
                .withBean("otherComposite", AiCallObserver.class, () -> composite)
                .run(context -> assertThat(context).hasFailed());
    }

    @Test
    void nativeAnthropicProfileIsExplicitlyEnabledAlongsideTheLedger() {
        runner.withPropertyValues("shenyu.ai.usage-ledger.enabled=true", "shenyu.ai.usage-ledger.namespace=namespace",
                "shenyu.plugins.ai.proxy.anthropic.enabled=true").run(context -> {
                    assertThat(context).hasNotFailed();
                    assertEquals("anthropic-messages", context.getBean(org.apache.shenyu.plugin.ai.proxy.enhanced.protocol.AiProxyProtocolFactory.class)
                            .getProtocol("anthropic-messages").name());
                    assertEquals("anthropic", context.getBean(org.apache.shenyu.plugin.ai.proxy.enhanced.provider.AiProxyProviderFactory.class)
                            .getProvider("Anthropic").name());
                });
    }
}
