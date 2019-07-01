/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package org.dromara.soul.web.config;

import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.common.dto.AppAuthData;
import org.dromara.soul.common.dto.PluginData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.configuration.zookeeper.ZookeeperConfig;
import org.dromara.soul.configuration.zookeeper.ZookeeperConfiguration;
import org.dromara.soul.web.cache.HttpLongPollLookupCacheManager;
import org.dromara.soul.web.cache.LookupCacheManager;
import org.dromara.soul.web.cache.ZookeeperCacheManager;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;

/**
 * Automatic data cache configuration for caching {@link AppAuthData}、{@link PluginData}、{@link RuleData}、{@link SelectorData}
 *
 * @author huangxiaofeng
 * @date 2019/6/26 14:40
 */
@Configuration
public class LookupCacheManagerAutoConfiguration {

    /**
     * zookeeper
     */
    @Configuration
    @ConditionalOnMissingBean(LookupCacheManager.class)
    @ConditionalOnProperty(name = "soul.cache.type", havingValue = "zookeeper")
    @Import(ZookeeperConfiguration.class)
    static class Zookeeper {
        @Bean
        @ConfigurationProperties(prefix = "soul.cache.zookeeper")
        public ZookeeperConfig zookeeperConfig() {
            return new ZookeeperConfig();
        }

        @Bean
        public ZookeeperCacheManager zookeeperCacheManager(ZkClient zkClient) {
            return new ZookeeperCacheManager(zkClient);
        }
    }

    /**
     * http long polling(default strategy)
     */
    @Configuration
    @ConditionalOnMissingBean(LookupCacheManager.class)
    @ConditionalOnProperty(name = "soul.cache.type", havingValue = "long-poll", matchIfMissing = true)
    static class HttpLongPolling {

        @Bean
        @ConfigurationProperties(prefix = "soul.cache.long-poll")
        public HttpLongPollLookupCacheManager configEventListener() {
            return new HttpLongPollLookupCacheManager();
        }

    }

}
