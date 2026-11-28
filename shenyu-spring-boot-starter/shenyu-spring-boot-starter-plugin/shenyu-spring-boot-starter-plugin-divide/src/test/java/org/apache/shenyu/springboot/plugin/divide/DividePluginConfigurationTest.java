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

package org.apache.shenyu.springboot.plugin.divide;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.api.context.ShenyuContextDecorator;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;
import org.apache.shenyu.plugin.base.handler.MetaDataHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.divide.canary.CanaryDecisionService;
import org.apache.shenyu.plugin.divide.canary.DefaultCanaryDecisionService;
import org.apache.shenyu.plugin.divide.handler.DivideUpstreamDataHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Test case for {@link DividePluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class DividePluginConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(DividePluginConfiguration.class))
            .withBean(DividePluginConfigurationTest.class)
            .withPropertyValues("debug=true");
    }

    @Test
    public void testDividePlugin() {
        applicationContextRunner.run(context -> {
                ShenyuPlugin plugin = context.getBean("dividePlugin", ShenyuPlugin.class);
                assertNotNull(plugin);
                DivideCircuitBreakerProperties properties = context.getBean(DivideCircuitBreakerProperties.class);
                assertThat(properties.getFailureThreshold()).isEqualTo(3);
                assertThat(properties.getHalfOpenWait()).isEqualTo(Duration.ofSeconds(10));
            }
        );
    }

    @Test
    void testConfigureFailureThresholdIndependently() {
        applicationContextRunner.withPropertyValues("shenyu.upstream-circuit-breaker.failure-threshold=5").run(context -> {
            UpstreamCircuitBreaker breaker = context.getBean(UpstreamCircuitBreaker.class);
            assertThat(context.getBean(DivideCircuitBreakerProperties.class).getHalfOpenWait()).isEqualTo(Duration.ofSeconds(10));
            for (int attempt = 0; attempt < 4; attempt++) {
                breaker.recordFailure("upstream");
                assertThat(breaker.isRequestAllowed("upstream")).isTrue();
            }
            breaker.recordFailure("upstream");
            assertThat(breaker.isRequestAllowed("upstream")).isFalse();
            assertThat(breaker.tryAcquireHalfOpenProbe("upstream")).isFalse();
        });
    }

    @Test
    void testConfigureHalfOpenWaitIndependently() {
        applicationContextRunner.withPropertyValues("shenyu.upstream-circuit-breaker.half-open-wait=1ms").run(context -> {
            UpstreamCircuitBreaker breaker = context.getBean(UpstreamCircuitBreaker.class);
            breaker.recordFailure("upstream");
            breaker.recordFailure("upstream");
            assertThat(breaker.isRequestAllowed("upstream")).isTrue();
            breaker.recordFailure("upstream");
            long deadline = System.nanoTime() + Duration.ofSeconds(1).toNanos();
            boolean acquired = breaker.tryAcquireHalfOpenProbe("upstream");
            while (!acquired && System.nanoTime() < deadline) {
                acquired = breaker.tryAcquireHalfOpenProbe("upstream");
            }
            assertThat(acquired).isTrue();
            assertThat(breaker.tryAcquireHalfOpenProbe("upstream")).isFalse();
        });
    }

    @ParameterizedTest
    @ValueSource(strings = {"failure-threshold=", "failure-threshold=0", "failure-threshold=2147483648",
            "half-open-wait=", "half-open-wait=999us", "half-open-wait=invalid", "half-open-wait=PT9223372036854776S"})
    void testRejectInvalidBreakerProperties(final String property) {
        applicationContextRunner.withPropertyValues("shenyu.upstream-circuit-breaker." + property).run(context ->
                assertThat(context.getStartupFailure()).hasStackTraceContaining("shenyu.upstream-circuit-breaker." + property.split("=")[0]));
    }

    @Test
    public void testDefaultCanaryDecisionService() {
        applicationContextRunner.run(context -> assertTrue(context.getBean(CanaryDecisionService.class) instanceof DefaultCanaryDecisionService));
    }

    @Test
    public void testCustomCanaryDecisionService() {
        CanaryDecisionService custom = mock(CanaryDecisionService.class);
        applicationContextRunner.withBean(CanaryDecisionService.class, () -> custom).run(context -> {
            assertSame(custom, context.getBean(CanaryDecisionService.class));
            assertNotNull(context.getBean("dividePlugin", ShenyuPlugin.class));
        });
    }

    @Test
    public void testDividePluginDataHandler() {
        applicationContextRunner.run(context -> {
                PluginDataHandler handler = context.getBean("dividePluginDataHandler", PluginDataHandler.class);
                assertNotNull(handler);
            }
        );
    }
    
    @Test
    public void testDivideMetaDataHandler() {
        applicationContextRunner.run(context -> {
                MetaDataHandler handler = context.getBean("divideMetaDataHandler", MetaDataHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testDivideUpstreamDataHandler() {
        applicationContextRunner.run(context -> {
                DivideUpstreamDataHandler handler = context.getBean("divideUpstreamDataHandler", DivideUpstreamDataHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testDivideShenyuContextDecorator() {
        applicationContextRunner.run(context -> {
                ShenyuContextDecorator decorator = context.getBean("divideShenyuContextDecorator", ShenyuContextDecorator.class);
                assertNotNull(decorator);
            }
        );
    }
}
