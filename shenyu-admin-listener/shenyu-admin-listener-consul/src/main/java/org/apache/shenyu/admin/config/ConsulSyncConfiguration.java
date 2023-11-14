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

import com.ecwid.consul.v1.ConsulClient;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ConsulProperties;
import org.apache.shenyu.admin.listener.DataChangedInit;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.listener.consul.ConsulDataChangedInit;
import org.apache.shenyu.admin.listener.consul.ConsulDataChangedListener;
import org.apache.shenyu.common.exception.ShenyuException;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * The type Consul listener.
 */
@Configuration
@ConditionalOnProperty(prefix = "shenyu.sync.consul", name = "url")
@EnableConfigurationProperties(ConsulProperties.class)
public class ConsulSyncConfiguration {

    /**
     * init Consul client.
     *
     * @param consulProperties the consul properties
     * @return Consul client
     */
    @Bean
    public ConsulClient consulClient(final ConsulProperties consulProperties) {
        String url = consulProperties.getUrl();
        if (StringUtils.isBlank(url)) {
            throw new ShenyuException("sync.consul.url can not be null.");
        }
        try {
            URL consulUrl = new URL(url);
            return consulUrl.getPort() < 0 ? new ConsulClient(consulUrl.getHost()) : new ConsulClient(consulUrl.getHost(), consulUrl.getPort());
        } catch (MalformedURLException e) {
            throw new ShenyuException("sync.consul.url formatter is not incorrect.");
        }
    }

    /**
     * Config event listener data changed listener.
     *
     * @param consulClient the consul client
     * @return the data changed listener
     */
    @Bean
    @ConditionalOnMissingBean(ConsulDataChangedListener.class)
    public DataChangedListener consulDataChangedListener(final ConsulClient consulClient) {
        return new ConsulDataChangedListener(consulClient);
    }

    /**
     * Consul data init.
     *
     * @param consulClient the consul client
     * @return the consul data init
     */
    @Bean
    @ConditionalOnMissingBean(ConsulDataChangedInit.class)
    public DataChangedInit consulDataChangedInit(final ConsulClient consulClient) {
        return new ConsulDataChangedInit(consulClient);
    }
}
