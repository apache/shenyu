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

package org.apache.shenyu.admin.config;

import org.apache.shenyu.admin.config.properties.ApolloProperties;
import org.apache.shenyu.admin.listener.DataChangedInit;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.listener.apollo.ApolloClient;
import org.apache.shenyu.admin.listener.apollo.ApolloDataChangedInit;
import org.apache.shenyu.admin.listener.apollo.ApolloDataChangedListener;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * the type apollo listener.
 */
@Configuration
@ConditionalOnProperty(prefix = "shenyu.sync.apollo", name = "meta")
@EnableConfigurationProperties(ApolloProperties.class)
public class ApolloSyncConfiguration {


    /**
     * init Consul client.
     *
     * @param apolloProperties the apollo properties
     * @return apollo client
     */
    @Bean
    public ApolloClient apolloClient(final ApolloProperties apolloProperties) {
        return new ApolloClient(apolloProperties);
    }

    /**
     * Config event listener data changed listener.
     *
     * @param apolloClient the apollo client
     * @return the data changed listener
     */
    @Bean
    @ConditionalOnMissingBean(ApolloDataChangedListener.class)
    public DataChangedListener apolloDataChangeListener(final ApolloClient apolloClient) {
        return new ApolloDataChangedListener(apolloClient);
    }

    /**
     * apollo data init.
     *
     * @param apolloClient the apollo client
     * @return the apollo data init
     */
    @Bean
    @ConditionalOnMissingBean(ApolloDataChangedInit.class)
    public DataChangedInit apolloDataChangeInit(final ApolloClient apolloClient) {
        return new ApolloDataChangedInit(apolloClient);
    }

}
