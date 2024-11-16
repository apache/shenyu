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
import org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * http long polling.
 */
@Configuration
@EnableConfigurationProperties(HttpSyncProperties.class)
@ConditionalOnProperty(prefix = "shenyu.sync.http", name = "enabled", havingValue = "true")
public class HttpLongPollingSyncConfiguration {
    
    private static final Logger LOG = LoggerFactory.getLogger(HttpLongPollingSyncConfiguration.class);

    /**
     * httpLongPollingDataChangedListener.
     *
     * @param httpSyncProperties httpSyncProperties
     * @return {@link HttpLongPollingDataChangedListener}
     */
    @Bean
    @ConditionalOnMissingBean(HttpLongPollingDataChangedListener.class)
    public HttpLongPollingDataChangedListener httpLongPollingDataChangedListener(final HttpSyncProperties httpSyncProperties) {
        LOG.info("you use http long polling sync");
        return new HttpLongPollingDataChangedListener(httpSyncProperties);
    }
    
}
