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

package org.apache.shenyu.springboot.starter.plugin.sentinel;

import com.alibaba.csp.sentinel.adapter.spring.webflux.exception.SentinelBlockExceptionHandler;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.base.fallback.FallbackHandler;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.sentinel.SentinelPlugin;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.codec.support.DefaultServerCodecConfigurer;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link SentinelPluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class SentinelPluginConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(SentinelPluginConfiguration.class))
            .withBean(SentinelPluginConfigurationTest.class)
            .withBean(DefaultServerCodecConfigurer.class)
            .withPropertyValues("debug=true");
    }

    @Test
    public void testSentinelPlugin() {
        applicationContextRunner.run(context -> {
                SentinelPlugin plugin = context.getBean("sentinelPlugin", SentinelPlugin.class);
                assertNotNull(plugin);
                assertThat(plugin.named()).isEqualTo(PluginEnum.SENTINEL.getName());
            }
        );
    }

    @Test
    public void testSentinelFallbackHandler() {
        applicationContextRunner.run(context -> {
                FallbackHandler handler = context.getBean("fallbackHandler", FallbackHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testSentinelRuleHandle() {
        applicationContextRunner.run(context -> {
                PluginDataHandler handler = context.getBean("sentinelRuleHandle", PluginDataHandler.class);
                assertNotNull(handler);
            }
        );
    }

    @Test
    public void testSentinelBlockExceptionHandler() {
        applicationContextRunner.run(context -> {
                SentinelBlockExceptionHandler handler = context.getBean("sentinelBlockExceptionHandler", SentinelBlockExceptionHandler.class);
                assertNotNull(handler);
            }
        );
    }
}
