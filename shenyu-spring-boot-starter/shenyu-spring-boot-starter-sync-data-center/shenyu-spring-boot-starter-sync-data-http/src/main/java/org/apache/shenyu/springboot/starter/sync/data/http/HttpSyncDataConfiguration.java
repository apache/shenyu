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

import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.constant.HttpConstants;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.ProxySelectorDataSubscriber;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.http.AccessTokenManager;
import org.apache.shenyu.sync.data.http.HttpSyncDataService;
import org.apache.shenyu.sync.data.http.config.HttpConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;
import java.util.Collections;
import java.util.List;
import java.util.Objects;
import okhttp3.OkHttpClient;

/**
 * Http sync data configuration for spring boot.
 */
@Configuration
@ConditionalOnClass(HttpSyncDataService.class)
@ConditionalOnProperty(prefix = "shenyu.sync.http", name = "url")
public class HttpSyncDataConfiguration {

    private static final Logger LOGGER = LoggerFactory.getLogger(HttpSyncDataConfiguration.class);

    /**
     * Http config http config.
     *
     * @return the http config
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.sync.http")
    public HttpConfig httpConfig() {
        return new HttpConfig();
    }

    /**
     * Rest template.
     *
     * @param httpConfig the http config
     * @return the rest template
     */
    @Bean
    @Qualifier("httpSyncClient")
    public OkHttpClient okHttpClient(final HttpConfig httpConfig) {
        return new OkHttpClient.Builder()
                .readTimeout(Duration.ofMillis(Objects.isNull(httpConfig.getReadTimeout()) ? (int) HttpConstants.CLIENT_POLLING_READ_TIMEOUT : httpConfig.getReadTimeout()))
                .connectTimeout(Duration.ofMillis(Objects.isNull(httpConfig.getConnectionTimeout()) ? HttpConstants.CLIENT_POLLING_CONNECT_TIMEOUT : httpConfig.getConnectionTimeout()))
                .writeTimeout(Duration.ofMillis(Objects.isNull(httpConfig.getWriteTimeout()) ? (int) HttpConstants.CLIENT_POLLING_WRITE_TIMEOUT : httpConfig.getWriteTimeout()))
                .build();
    }

    /**
     * AccessTokenManager.
     *
     * @param httpConfig   the http config.
     * @param okHttpClient the rest okHttpClient.
     * @return the access token manager.
     */
    @Bean
    public AccessTokenManager accessTokenManager(final HttpConfig httpConfig, @Qualifier("httpSyncClient") final OkHttpClient okHttpClient) {
        return new AccessTokenManager(okHttpClient, httpConfig);
    }

    /**
     * Http sync data service.
     *
     * @param httpConfig         the http config
     * @param pluginSubscriber   the plugin subscriber
     * @param okHttpClient       the ok http client
     * @param metaSubscribers    the meta subscribers
     * @param authSubscribers    the auth subscribers
     * @param accessTokenManager the access token manager
     * @param proxySelectorDataSubscribers the proxySelectorData subscribers
     * @param discoveryUpstreamDataSubscribers the discoveryUpstreamData subscribers
     * @param shenyuConfig       the shenyuConfig
     * @return the sync data service
     */
    @Bean
    public SyncDataService httpSyncDataService(final ObjectProvider<HttpConfig> httpConfig,
                                               final ObjectProvider<PluginDataSubscriber> pluginSubscriber,
                                               @Qualifier("httpSyncClient") final ObjectProvider<OkHttpClient> okHttpClient,
                                               final ObjectProvider<List<MetaDataSubscriber>> metaSubscribers,
                                               final ObjectProvider<List<AuthDataSubscriber>> authSubscribers,
                                               final ObjectProvider<AccessTokenManager> accessTokenManager,
                                               final ObjectProvider<List<ProxySelectorDataSubscriber>> proxySelectorDataSubscribers,
                                               final ObjectProvider<List<DiscoveryUpstreamDataSubscriber>> discoveryUpstreamDataSubscribers,
                                               final ObjectProvider<ShenyuConfig> shenyuConfig) {
        LOGGER.info("you use http long pull sync shenyu data");
        return new HttpSyncDataService(
                Objects.requireNonNull(httpConfig.getIfAvailable()),
                Objects.requireNonNull(pluginSubscriber.getIfAvailable()),
                Objects.requireNonNull(okHttpClient.getIfAvailable()),
                metaSubscribers.getIfAvailable(Collections::emptyList),
                authSubscribers.getIfAvailable(Collections::emptyList),
                proxySelectorDataSubscribers.getIfAvailable(Collections::emptyList),
                discoveryUpstreamDataSubscribers.getIfAvailable(Collections::emptyList),
                Objects.requireNonNull(accessTokenManager.getIfAvailable()),
                Objects.requireNonNull(shenyuConfig.getIfAvailable())
        );
    }
}
