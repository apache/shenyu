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

package org.apache.shenyu.springboot.plugin.http.record;

import jakarta.annotation.PreDestroy;
import org.apache.shenyu.plugin.record.HttpRecordPlugin;
import org.apache.shenyu.plugin.record.collector.HttpRecordCollector;
import org.apache.shenyu.plugin.record.config.ShenyuHttpRecordCenterConfig;
import org.apache.shenyu.plugin.record.handler.HttpRecordPluginDataHandler;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * HttpRecordPluginConfiguration.
 *
 * <p>Spring Boot auto-configuration for the http record plugin.
 * Enabled by default; can be disabled via {@code shenyu.plugins.http-record.enabled=false}.</p>
 */
@Configuration
@ConditionalOnProperty(value = "shenyu.plugins.http-record.enabled", havingValue = "true", matchIfMissing = true)
public class HttpRecordPluginConfiguration {

    /**
     * Http record plugin bean.
     *
     * @return the http record plugin
     */
    @Bean
    @ConditionalOnMissingBean
    public HttpRecordPlugin httpRecordPlugin() {
        return new HttpRecordPlugin();
    }

    /**
     * Http record plugin data handler bean.
     *
     * @return the data handler
     */
    @Bean
    public HttpRecordPluginDataHandler httpRecordPluginDataHandler() {
        return new HttpRecordPluginDataHandler();
    }

    /**
     * Shenyu http record center config bean.
     *
     * @return the config bean
     */
    @Bean
    @ConfigurationProperties(prefix = "shenyu.http-record")
    ShenyuHttpRecordCenterConfig shenyuRecordCenterConfig() {
        return new ShenyuHttpRecordCenterConfig();
    }

    /**
     * Cleanup on application shutdown.
     */
    @PreDestroy
    public void destroy() {
        HttpRecordCollector.INSTANCE.stop();
    }
}
