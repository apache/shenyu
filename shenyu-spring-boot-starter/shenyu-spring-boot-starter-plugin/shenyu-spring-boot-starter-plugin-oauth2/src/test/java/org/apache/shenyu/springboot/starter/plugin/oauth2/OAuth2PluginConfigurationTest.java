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
import org.junit.Test;
import org.springframework.boot.autoconfigure.AutoConfigurations;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.oauth2.client.InMemoryReactiveOAuth2AuthorizedClientService;

import static org.assertj.core.api.Assertions.assertThat;

public class OAuth2PluginConfigurationTest {

    @Test
    public void testSpringCloudPlugin() {
        new ApplicationContextRunner()
            .withUserConfiguration(EnableWebFluxSecurity.class)
            .withConfiguration(
                AutoConfigurations.of(
                    InMemoryReactiveOAuth2AuthorizedClientService.class,
                    OAuth2PluginConfiguration.class
                ))
            .withPropertyValues(
                "debug=true")
            .run(
                context -> {
                    assertThat(context).hasSingleBean(InMemoryReactiveOAuth2AuthorizedClientService.class);
                    ShenyuPlugin plugin = context.getBean(ShenyuPlugin.class);
                    assertThat(plugin.named()).isEqualTo(PluginEnum.OAUTH2.getName());
                }
            );
    }
}
