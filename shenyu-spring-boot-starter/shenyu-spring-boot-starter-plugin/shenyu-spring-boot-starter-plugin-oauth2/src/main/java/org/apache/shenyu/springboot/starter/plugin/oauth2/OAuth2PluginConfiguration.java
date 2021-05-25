/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.apache.shenyu.springboot.starter.plugin.oauth2;

import org.apache.shenyu.plugin.api.ShenyuPlugin;
import org.apache.shenyu.plugin.oauth2.OAuth2Plugin;
import org.apache.shenyu.plugin.oauth2.filter.OAuth2Filter;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.method.configuration.EnableReactiveMethodSecurity;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.config.web.server.SecurityWebFiltersOrder;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.web.server.SecurityWebFilterChain;

import java.util.Objects;

import static org.springframework.security.config.Customizer.withDefaults;

/**
 * The type motan plugin configuration.
 */
@Configuration
@ConditionalOnClass(OAuth2Plugin.class)
@EnableWebFluxSecurity
@EnableReactiveMethodSecurity
public class OAuth2PluginConfiguration {

    /**
     * oauth2 plugin shenyu plugin.
     *
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin oAuth2Plugin() {
        return new OAuth2Plugin();
    }

    /**
     * Build SecurityWebFilterChain.
     *
     * @param http The ServerHttpSecurity Instance
     * @param oAuth2FilterProvider The OAuth2Filter Instance
     * @return The SecurityWebFilterChain
     */
    @Bean
    public SecurityWebFilterChain getSecurityWebFilterChain(final ServerHttpSecurity http, final ObjectProvider<OAuth2Filter> oAuth2FilterProvider) {
        http
                .authorizeExchange(exchanges ->
                        exchanges
                                .pathMatchers("/**").permitAll()
                                .anyExchange().authenticated()
                )
                .oauth2Login(withDefaults())
                .formLogin(withDefaults())
                .oauth2Client(withDefaults())
                .addFilterAt(Objects.requireNonNull(oAuth2FilterProvider.getIfAvailable()), SecurityWebFiltersOrder.LAST);
        return http.build();
    }

    /**
     * Build OAuth2Filter.
     *
     * @param authorizedClientService The authorizedClientService Instance
     * @return The OAuth2Filter.
     */
    @Bean
    public OAuth2Filter oAuth2Filter(final ObjectProvider<ReactiveOAuth2AuthorizedClientService> authorizedClientService) {
        return new OAuth2Filter(Objects.requireNonNull(authorizedClientService.getIfAvailable()));
    }
}
