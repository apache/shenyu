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

import org.apache.shenyu.admin.config.properties.HttpSyncProperties;
import org.apache.shenyu.admin.controller.ConfigController;
import org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * http long polling.
 */
@Configuration
@ConditionalOnProperty(name = "shenyu.sync.http.enabled", havingValue = "true")
@EnableConfigurationProperties(HttpSyncProperties.class)
public class HttpLongPollingSyncConfiguration {

    /**
     * httpLongPollingDataChangedListener.
     *
     * @param httpSyncProperties httpSyncProperties
     * @return {@link HttpLongPollingDataChangedListener}
     */
    @Bean
    @ConditionalOnMissingBean(HttpLongPollingDataChangedListener.class)
    public HttpLongPollingDataChangedListener httpLongPollingDataChangedListener(final HttpSyncProperties httpSyncProperties) {
        return new HttpLongPollingDataChangedListener(httpSyncProperties);
    }

    /**
     * configController.
     *
     * @param httpLongPollingDataChangedListener httpLongPollingDataChangedListener
     * @return {@link ConfigController}
     */
    @Bean
    @ConditionalOnMissingBean(ConfigController.class)
    public ConfigController configController(final HttpLongPollingDataChangedListener httpLongPollingDataChangedListener) {
        return new ConfigController(httpLongPollingDataChangedListener);
    }
}
