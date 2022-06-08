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

package org.apache.shenyu.springboot.starter.sync.data.http;

import org.apache.shenyu.common.constant.HttpConstants;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.http.AccessTokenManager;
import org.apache.shenyu.sync.data.http.HttpSyncDataService;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.client.OkHttp3ClientHttpRequestFactory;
import org.springframework.web.client.RestTemplate;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

/**
 * Http sync data configuration for spring boot.
 */
@Configuration
@ConditionalOnClass(HttpSyncDataService.class)
@ConditionalOnProperty(prefix = "shenyu.sync.http", name = "url")
@EnableConfigurationProperties(value = HttpConfig.class)
public class HttpSyncDataConfiguration {

    private static final Logger LOGGER = LoggerFactory.getLogger(HttpSyncDataConfiguration.class);

    /**
     * Rest template.
     *
     * @param httpConfig the http config
     * @return the rest template
     */
    @Bean
    public RestTemplate restTemplate(final HttpConfig httpConfig) {
        OkHttp3ClientHttpRequestFactory factory = new OkHttp3ClientHttpRequestFactory();
        factory.setConnectTimeout(Objects.isNull(httpConfig.getConnectionTimeout()) ? (int) HttpConstants.CLIENT_POLLING_CONNECT_TIMEOUT : httpConfig.getConnectionTimeout());
        factory.setReadTimeout(Objects.isNull(httpConfig.getReadTimeout()) ? (int) HttpConstants.CLIENT_POLLING_READ_TIMEOUT : httpConfig.getReadTimeout());
        factory.setWriteTimeout(Objects.isNull(httpConfig.getWriteTimeout()) ? (int) HttpConstants.CLIENT_POLLING_WRITE_TIMEOUT : httpConfig.getWriteTimeout());
        return new RestTemplate(factory);
    }

    /**
     * AccessTokenManager.
     *
     * @param httpConfig   the http config.
     * @param restTemplate the rest template.
     * @return the access token manager.
     */
    @Bean
    public AccessTokenManager accessTokenManager(final HttpConfig httpConfig, final RestTemplate restTemplate) {
        return new AccessTokenManager(restTemplate, httpConfig);
    }

    /**
     * Http sync data service.
     *
     * @param httpConfig         the http config
     * @param pluginSubscriber   the plugin subscriber
     * @param restTemplate       the rest template
     * @param metaSubscribers    the meta subscribers
     * @param authSubscribers    the auth subscribers
     * @param accessTokenManager the access token manager
     * @return the sync data service
     */
    @Bean
    public SyncDataService httpSyncDataService(final ObjectProvider<HttpConfig> httpConfig,
                                               final ObjectProvider<PluginDataSubscriber> pluginSubscriber,
                                               final ObjectProvider<RestTemplate> restTemplate,
                                               final ObjectProvider<List<MetaDataSubscriber>> metaSubscribers,
                                               final ObjectProvider<List<AuthDataSubscriber>> authSubscribers,
                                               final ObjectProvider<AccessTokenManager> accessTokenManager) {
        LOGGER.info("you use http long pull sync shenyu data");
        return new HttpSyncDataService(
                Objects.requireNonNull(httpConfig.getIfAvailable()),
                Objects.requireNonNull(pluginSubscriber.getIfAvailable()),
                Objects.requireNonNull(restTemplate.getIfAvailable()),
                metaSubscribers.getIfAvailable(Collections::emptyList),
                authSubscribers.getIfAvailable(Collections::emptyList),
                Objects.requireNonNull(accessTokenManager.getIfAvailable())
        );
    }
}
