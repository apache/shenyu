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
import org.apache.shenyu.plugin.oauth2.filter.OAuth2PreFilter;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.security.reactive.PathRequest;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Conditional;
import org.springframework.context.annotation.Configuration;
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
        MATCHERS.add(new PathPatternParserServerWebExchangeMatcher("-"));
    }

    private static final OrServerWebExchangeMatcher OR_MATCHER = new OrServerWebExchangeMatcher(MATCHERS);

    /**
     * oauth2 plugin shenyu plugin.
     * @param authorizedClientServiceProvider the ReactiveOAuth2AuthorizedClientServiceProvider
     * @return the shenyu plugin
     */
    @Bean
    public ShenyuPlugin oAuth2Plugin(final ObjectProvider<ReactiveOAuth2AuthorizedClientService> authorizedClientServiceProvider) {
        return new OAuth2Plugin(authorizedClientServiceProvider.getObject());
    }

    @Bean
    @ConditionalOnMissingBean(ReactiveAuthenticationManager.class)
    MapReactiveUserDetailsService userDetailsService() {
        UserDetails userDetails = User
            .builder()
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
     * @param context                 The ApplicationContext Instance
     * @return The SecurityWebFilterChain
     */
    @Bean
    public SecurityWebFilterChain getSecurityWebFilterChain(final ServerHttpSecurity http,
                                                            final ApplicationContext context) {
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
            .addFilterAfter(new OAuth2PreFilter(MATCHERS), SecurityWebFiltersOrder.REACTOR_CONTEXT)
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
     * Build default clientRegistration.
     * if this bean load, the oauth2 plugin don't take effect
     *
     * @return The clientRegistration instance.
     */
    @Bean(DEFAULT_CLIENT_REGISTRATION_BEAN)
    @Conditional(DefaultClientsConfiguredCondition.class)
    public ReactiveClientRegistrationRepository reactiveClientRegistrationRepository() {
        ClientRegistration.Builder shenyuClientRegistration = ClientRegistration.withRegistrationId("shenyu");
        shenyuClientRegistration.authorizationGrantType(AuthorizationGrantType.AUTHORIZATION_CODE);
        shenyuClientRegistration.tokenUri("/");
        shenyuClientRegistration.authorizationUri("/");
        shenyuClientRegistration.redirectUriTemplate("/");
        shenyuClientRegistration.scope("read:user");
        shenyuClientRegistration.userInfoUri("/");
        shenyuClientRegistration.clientId("shenyu");
        shenyuClientRegistration.clientSecret("shenyu");
        shenyuClientRegistration.redirectUriTemplate("{baseUrl}/login/oauth2/code/{registrationId}");
        return new InMemoryReactiveClientRegistrationRepository(shenyuClientRegistration.build());
    }
}
