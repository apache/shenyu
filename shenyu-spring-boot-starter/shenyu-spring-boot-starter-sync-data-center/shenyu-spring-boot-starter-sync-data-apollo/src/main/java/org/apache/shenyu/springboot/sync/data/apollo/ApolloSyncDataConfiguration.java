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

package org.apache.shenyu.springboot.sync.data.apollo;

import com.ctrip.framework.apollo.Config;
import com.ctrip.framework.apollo.ConfigService;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.apollo.ApolloDataService;
import org.apache.shenyu.sync.data.apollo.config.ApolloConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * Apollo sync data configuration for spring boot.
 */
@Configuration
@ConditionalOnClass(ApolloSyncDataConfiguration.class)
@ConditionalOnProperty(prefix = "shenyu.sync.apollo", name = "meta")
public class ApolloSyncDataConfiguration {

    private static final Logger LOGGER = LoggerFactory.getLogger(ApolloSyncDataConfiguration.class);

    private static final String APOLLO_CLUSTER = "apollo.cluster";

    private static final String PROP_APP_ID = "app.id";

    private static final String PROP_APOLLO_META = "apollo.meta";

    private static final String PROP_APOLLO_SECRET = "apollo.access-key";

    private static final String APOLLO_NAMESPACE = "apollo.bootstrap.namespace";

    /**
     * Apollo config apollo config.
     * @param configService the config service
     * @param pluginSubscriber the plugin subscriber
     * @param metaSubscribers the meta subscribers
     * @param authSubscribers the auth subscribers
     * @return the apollo config
     */
    @Bean
    public ApolloDataService apolloSyncDataService(final ObjectProvider<Config> configService, final ObjectProvider<PluginDataSubscriber> pluginSubscriber,
                                                   final ObjectProvider<List<MetaDataSubscriber>> metaSubscribers, final ObjectProvider<List<AuthDataSubscriber>> authSubscribers) {
        LOGGER.info("you use apollo sync shenyu data.......");
        return new ApolloDataService(configService.getIfAvailable(), pluginSubscriber.getIfAvailable(),
                metaSubscribers.getIfAvailable(Collections::emptyList), authSubscribers.getIfAvailable(Collections::emptyList));
    }

    /**
     * Apollo config config.
     *
     * @param apolloConfig the apollo config
     * @return the config
     */
    @Bean
    public Config apolloConfigService(final ApolloConfig apolloConfig) {
        Optional.ofNullable(apolloConfig.getAppId()).ifPresent(appId -> System.setProperty(PROP_APP_ID, appId));
        Optional.ofNullable(apolloConfig.getMeta()).ifPresent(meta -> System.setProperty(PROP_APOLLO_META, meta));
        Optional.ofNullable(apolloConfig.getClusterName()).ifPresent(cluster -> System.setProperty(APOLLO_CLUSTER, cluster));
        Optional.ofNullable(apolloConfig.getNamespace()).ifPresent(namespace -> System.setProperty(APOLLO_NAMESPACE, namespace));
        Optional.ofNullable(apolloConfig.getAccessKey()).ifPresent(accessKey -> System.setProperty(PROP_APOLLO_SECRET, accessKey));
        return ConfigService.getAppConfig();
    }

    /**
     * apollo config.
     *
     * @return the apollo config
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.sync.apollo")
    public ApolloConfig apolloConfig() {
        return new ApolloConfig();
    }
}

