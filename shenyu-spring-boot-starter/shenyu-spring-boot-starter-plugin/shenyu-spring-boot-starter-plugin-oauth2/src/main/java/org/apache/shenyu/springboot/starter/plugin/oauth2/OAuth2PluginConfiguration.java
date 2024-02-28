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

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.oauth2.OAuth2Plugin;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfigureAfter;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.autoconfigure.security.SecurityProperties;
import org.springframework.boot.autoconfigure.security.reactive.ReactiveSecurityAutoConfiguration;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.oauth2.client.OAuth2AuthorizedClient;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.web.server.SecurityWebFilterChain;

/**
 * The type oauth2 plugin configuration.
 */
@Configuration(proxyBeanMethods = false)
@ConditionalOnProperty(value = {"shenyu.plugins.oauth2.enabled"}, havingValue = "true", matchIfMissing = true)
@ConditionalOnClass({ OAuth2AuthorizedClient.class, SecurityWebFilterChain.class, SecurityProperties.class })
@AutoConfigureAfter(ReactiveSecurityAutoConfiguration.class)
public class OAuth2PluginConfiguration {
    
    /**
     * Oauth2 plugin for Apache ShenYu.
     *
     * @param authorizedClientServiceProvider the authorized client service provider
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin oAuth2Plugin(final ObjectProvider<ReactiveOAuth2AuthorizedClientService> authorizedClientServiceProvider) {
        return new OAuth2Plugin(authorizedClientServiceProvider);
    }
}
