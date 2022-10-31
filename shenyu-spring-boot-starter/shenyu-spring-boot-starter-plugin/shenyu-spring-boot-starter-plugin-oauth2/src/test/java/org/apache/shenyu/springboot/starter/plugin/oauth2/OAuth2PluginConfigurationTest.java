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

package org.apache.shenyu.springboot.starter.plugin.oauth2;

import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.core.userdetails.MapReactiveUserDetailsService;
import org.springframework.security.oauth2.client.InMemoryReactiveOAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.registration.ReactiveClientRegistrationRepository;
import org.springframework.security.web.server.SecurityWebFilterChain;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test case for {@link OAuth2PluginConfiguration}.
 */
@Configuration
@EnableConfigurationProperties
public class OAuth2PluginConfigurationTest {

    private ApplicationContextRunner applicationContextRunner;

    @BeforeEach
    public void before() {
        applicationContextRunner = new ApplicationContextRunner()
            .withConfiguration(AutoConfigurations.of(OAuth2PluginConfiguration.class))
            .withBean(OAuth2PluginConfigurationTest.class)
            .withBean(InMemoryReactiveOAuth2AuthorizedClientService.class)
            .withPropertyValues("debug=true");
    }

    @Test
    public void testOAuth2Plugin() {
        applicationContextRunner.run(context -> {
                ShenyuPlugin plugin = context.getBean("oAuth2Plugin", ShenyuPlugin.class);
                assertNotNull(plugin);
                assertThat(plugin.named()).isEqualTo(PluginEnum.OAUTH2.getName());
            }
        );
    }

    @Test
    public void testMapReactiveUserDetailsService() {
        applicationContextRunner.run(context -> {
                MapReactiveUserDetailsService service = context.getBean("userDetailsService", MapReactiveUserDetailsService.class);
                assertNotNull(service);
            }
        );
    }

    @Test
    public void testSecurityWebFilterChain() {
        applicationContextRunner.run(context -> {
                SecurityWebFilterChain chain = context.getBean("getSecurityWebFilterChain", SecurityWebFilterChain.class);
                assertNotNull(chain);
            }
        );
    }

    @Test
    public void testInMemoryReactiveClientRegistrationRepository() {
        applicationContextRunner.run(context -> {
                ReactiveClientRegistrationRepository repository = context.getBean(
                    "org.apache.shenyu.springboot.starter.plugin.oauth2.defaultReactiveClientRegistrationRepository",
                    ReactiveClientRegistrationRepository.class
                );
                assertNotNull(repository);
            }
        );
    }
}
