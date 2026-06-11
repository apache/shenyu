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

package org.apache.shenyu.infra.zookeeper.autoconfig;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.infra.zookeeper.client.ZookeeperClient;
import org.apache.shenyu.infra.zookeeper.config.ZookeeperConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.util.StringUtils;

import java.util.Objects;

/**
 * The type Zookeeper configuration.
 */

@Configuration
@ConditionOnSyncZookeeper
@ConditionalOnClass(ZookeeperClient.class)
@EnableConfigurationProperties(ZookeeperProperties.class)
public class ZookeeperConfiguration {

    private static final Logger LOG = LoggerFactory.getLogger(ZookeeperConfiguration.class);

    private static final Integer DEFAULT_SESSION_TIMEOUT = 60 * 1000;

    private static final Integer DEFAULT_CONNECT_TIMEOUT = 15 * 1000;

    private static final Integer DEFAULT_BASE_SLEEP_TIME = 1000;

    private static final Integer DEFAULT_MAX_SLEEP_TIME = Integer.MAX_VALUE;

    private static final Integer DEFAULT_MAX_RETRIES = 3;

    /**
     * Zookeeper client bean.
     *
     * @param zookeeperProperties zookeeper properties
     * @return ZookeeperClient
     */
    @Bean
    @ConditionalOnMissingBean(ZookeeperClient.class)
    public ZookeeperClient zookeeperClient(final ZookeeperProperties zookeeperProperties) {

        ZookeeperConfig config = zookeeperProperties.getZookeeper();
        if (!StringUtils.hasText(config.getUrl())) {
            throw new ShenyuException("zookeeper url is empty");
        }
        if (Objects.isNull(config.getBaseSleepTimeMilliseconds())) {
            config.setBaseSleepTimeMilliseconds(DEFAULT_BASE_SLEEP_TIME);
        }
        if (Objects.isNull(config.getMaxSleepTimeMilliseconds())) {
            config.setMaxSleepTimeMilliseconds(DEFAULT_MAX_SLEEP_TIME);
        }
        if (Objects.isNull(config.getMaxRetries())) {
            config.setMaxRetries(DEFAULT_MAX_RETRIES);
        }
        if (Objects.isNull(config.getSessionTimeoutMilliseconds())) {
            config.setSessionTimeoutMilliseconds(DEFAULT_SESSION_TIMEOUT);
        }
        if (Objects.isNull(config.getConnectionTimeoutMilliseconds())) {
            config.setConnectionTimeoutMilliseconds(DEFAULT_CONNECT_TIMEOUT);
        }

        LOG.info("init zookeeper client, config: {}", config);

        ZookeeperClient client = ZookeeperClient.builder()
                .config(config)
                .build();
        client.start();

        return client;
    }
}
