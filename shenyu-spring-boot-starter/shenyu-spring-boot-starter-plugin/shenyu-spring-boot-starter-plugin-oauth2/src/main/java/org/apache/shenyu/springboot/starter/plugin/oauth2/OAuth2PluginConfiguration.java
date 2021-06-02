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
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.security.reactive.PathRequest;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.DependsOn;
import org.springframework.http.HttpMethod;
import org.springframework.security.authentication.ReactiveAuthenticationManager;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.config.web.server.SecurityWebFiltersOrder;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.core.userdetails.MapReactiveUserDetailsService;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.oauth2.client.ReactiveOAuth2AuthorizedClientService;
import org.springframework.security.oauth2.client.registration.ClientRegistration;
import org.springframework.security.oauth2.client.registration.InMemoryReactiveClientRegistrationRepository;
import org.springframework.security.oauth2.client.registration.ReactiveClientRegistrationRepository;
import org.springframework.security.oauth2.core.AuthorizationGrantType;
import org.springframework.security.web.server.SecurityWebFilterChain;
import org.springframework.security.web.server.util.matcher.OrServerWebExchangeMatcher;
import org.springframework.security.web.server.util.matcher.PathPatternParserServerWebExchangeMatcher;
import org.springframework.security.web.server.util.matcher.ServerWebExchangeMatcher;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * The type oauth2 plugin configuration.
 */
@Configuration
@ConditionalOnClass(OAuth2Plugin.class)
@EnableWebFluxSecurity
public class OAuth2PluginConfiguration {

    private static final String DEFAULT_CLIENT_REGISTRATION_BEAN = "org.apache.shenyu.springboot.starter.plugin.oauth2.defaultReactiveClientRegistrationRepository";

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

    @Bean
    @ConditionalOnMissingBean(ReactiveAuthenticationManager.class)
    MapReactiveUserDetailsService userDetailsService() {
        UserDetails userDetails = User
            .withDefaultPasswordEncoder()
            .username("shenyu")
            .password("shenyu")
            .roles("USER")
            .disabled(true)
            .build();
        return new MapReactiveUserDetailsService(userDetails);
    }

    /**
     * Build SecurityWebFilterChain.
     *
     * @param http                    The ServerHttpSecurity Instance
     * @param oAuth2FilterProvider    The OAuth2Filter Instance
     * @param oAuth2PreFilterProvider The OAuth2PreFilter Instance
     * @param context                 The ApplicationContext Instance
     * @return The SecurityWebFilterChain
     */
    @Bean
    public SecurityWebFilterChain getSecurityWebFilterChain(final ServerHttpSecurity http, final ObjectProvider<OAuth2Filter> oAuth2FilterProvider,
                                                            final ObjectProvider<OAuth2PreFilter> oAuth2PreFilterProvider, final ApplicationContext context) {
        String[] names = context.getBeanNamesForType(ReactiveClientRegistrationRepository.class);
        boolean exists = Arrays.asList(names).contains(DEFAULT_CLIENT_REGISTRATION_BEAN);
        if (exists) {
            return http.csrf()
                .disable()
                .httpBasic()
                .disable()
                .formLogin()
                .disable()
                .authorizeExchange()
                .anyExchange()
                .permitAll()
                .and()
                .build();
        }
        return http
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
            )
            .build();
    }

    /**
     * Build OAuth2PreFilter.
     *
     * @return The OAuth2PreFilter
     */
    @Bean
    @ConditionalOnMissingBean(value = ReactiveClientRegistrationRepository.class, name = DEFAULT_CLIENT_REGISTRATION_BEAN)
    public OAuth2PreFilter oAuth2PreFilter() {
        return new OAuth2PreFilter(MATCHERS);
    }

    /**
     * Build OAuth2Filter.
     *
     * @param authorizedClientService The authorizedClientService Instance
     * @return The OAuth2Filter.
     */
    @Bean
    @DependsOn("oAuth2PreFilter")
    @ConditionalOnMissingBean(value = ReactiveClientRegistrationRepository.class, name = DEFAULT_CLIENT_REGISTRATION_BEAN)
    public OAuth2Filter oAuth2Filter(final ObjectProvider<ReactiveOAuth2AuthorizedClientService> authorizedClientService) {
        return new OAuth2Filter(authorizedClientService.getIfAvailable());
    }

    /**
     * Build clientRegistration.
     *
     * @return The clientRegistration instance.
     */
    @Bean(DEFAULT_CLIENT_REGISTRATION_BEAN)
    @ConditionalOnMissingBean(ReactiveClientRegistrationRepository.class)
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
