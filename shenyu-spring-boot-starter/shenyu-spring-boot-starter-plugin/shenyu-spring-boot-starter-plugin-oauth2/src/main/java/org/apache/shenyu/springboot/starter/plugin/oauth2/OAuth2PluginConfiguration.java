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
import org.apache.shenyu.plugin.oauth2.filter.OAuth2PreFilter;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.security.reactive.PathRequest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.FilterType;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.config.web.server.SecurityWebFiltersOrder;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.InMemoryReactiveClientRegistrationRepository;
import org.springframework.security.oauth2.client.registration.ReactiveClientRegistrationRepository;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.web.server.SecurityWebFilterChain;
import org.springframework.security.web.server.util.matcher.OrServerWebExchangeMatcher;
import org.springframework.security.web.server.util.matcher.PathPatternParserServerWebExchangeMatcher;
import org.springframework.security.web.server.util.matcher.ServerWebExchangeMatcher;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * The type motan plugin configuration.
 */
@Configuration
@ConditionalOnClass(OAuth2Plugin.class)
@EnableWebFluxSecurity
@ComponentScan(excludeFilters = {@ComponentScan.Filter(type = FilterType.ASSIGNABLE_TYPE, value = org.springframework.boot.autoconfigure.security.servlet.SecurityAutoConfiguration.class)})
public class OAuth2PluginConfiguration {

    private static final List<ServerWebExchangeMatcher> MATCHERS = new CopyOnWriteArrayList<>();

    static {
        MATCHERS.add(new PathPatternParserServerWebExchangeMatcher("/"));
    }

    private static final OrServerWebExchangeMatcher OR_MATCHER = new OrServerWebExchangeMatcher(MATCHERS);

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
     * @param http                    The ServerHttpSecurity Instance
     * @param oAuth2FilterProvider    The OAuth2Filter Instance
     * @param oAuth2PreFilterProvider The OAuth2PreFilter Instance
     * @return The SecurityWebFilterChain
     */
    @Bean
    public SecurityWebFilterChain getSecurityWebFilterChain(final ServerHttpSecurity http, final ObjectProvider<OAuth2Filter> oAuth2FilterProvider,
                                                            final ObjectProvider<OAuth2PreFilter> oAuth2PreFilterProvider) {
        http
            .csrf()
            .disable()
            .oauth2Login()
            .and()
            .httpBasic(ServerHttpSecurity.HttpBasicSpec::disable)
            .oauth2Client()
            .and()
            .addFilterAt(Objects.requireNonNull(oAuth2FilterProvider.getIfAvailable()), SecurityWebFiltersOrder.LAST)
            .addFilterAfter(Objects.requireNonNull(oAuth2PreFilterProvider.getIfAvailable()), SecurityWebFiltersOrder.REACTOR_CONTEXT)
            .authorizeExchange(exchanges ->
                exchanges
                    .matchers(PathRequest.toStaticResources().atCommonLocations())
                    .permitAll()
                    .pathMatchers(HttpMethod.OPTIONS)
                    .permitAll()
                    .matchers(OR_MATCHER)
                    .authenticated()
                    .anyExchange()
                    .permitAll()
            );
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

    /**
     * Build OAuth2PreFilter.
     * @return The OAuth2PreFilter
     */
    @Bean
    public OAuth2PreFilter oAuth2PreFilter() {
        return new OAuth2PreFilter(MATCHERS);
    }

    /**
     * Build clientRegistration.
     *
     * @return The clientRegistration instance.
     */
    @Bean
    public ReactiveClientRegistrationRepository reactiveClientRegistrationRepository() {
//        ClientRegistration.Builder builder = ClientRegistrations.fromIssuerLocation("");
//        builder.registrationId()
        ClientRegistration.Builder github = ClientRegistration.withRegistrationId("123");
        github.authorizationGrantType(AuthorizationGrantType.AUTHORIZATION_CODE);
        github.tokenUri("/");
        github.authorizationUri("/");
        github.redirectUriTemplate("/");
        github.scope("");
        github.userInfoUri("/");
        github.clientId("123");
        github.clientSecret("123");
        github.redirectUriTemplate("{baseUrl}/login/oauth2/code/{registrationId}");
        return new InMemoryReactiveClientRegistrationRepository(github.build());
    }
}
