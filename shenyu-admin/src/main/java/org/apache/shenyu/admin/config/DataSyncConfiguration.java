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

import com.alibaba.nacos.api.NacosFactory;
import com.alibaba.nacos.api.PropertyKeyConst;
import com.alibaba.nacos.api.config.ConfigService;
import com.ecwid.consul.v1.ConsulClient;
import io.etcd.jetcd.Client;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ConsulProperties;
import org.apache.shenyu.admin.config.properties.EtcdProperties;
import org.apache.shenyu.admin.config.properties.HttpSyncProperties;
import org.apache.shenyu.admin.config.properties.NacosProperties;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.config.properties.ZookeeperProperties;
import org.apache.shenyu.admin.listener.DataChangedInit;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.listener.consul.ConsulDataChangedInit;
import org.apache.shenyu.admin.listener.consul.ConsulDataChangedListener;
import org.apache.shenyu.admin.listener.etcd.EtcdClient;
import org.apache.shenyu.admin.listener.etcd.EtcdDataChangedInit;
import org.apache.shenyu.admin.listener.etcd.EtcdDataDataChangedListener;
import org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener;
import org.apache.shenyu.admin.listener.nacos.NacosDataChangedInit;
import org.apache.shenyu.admin.listener.nacos.NacosDataChangedListener;
import org.apache.shenyu.admin.listener.websocket.WebsocketCollector;
import org.apache.shenyu.admin.listener.websocket.WebsocketDataChangedListener;
import org.apache.shenyu.admin.listener.zookeeper.ZookeeperDataChangedInit;
import org.apache.shenyu.admin.listener.zookeeper.ZookeeperDataChangedListener;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperClient;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperConfig;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.server.standard.ServerEndpointExporter;

import java.util.Objects;
import java.util.Properties;

/**
 * The type Data sync configuration.
 */
@Configuration
public class DataSyncConfiguration {

    /**
     * http long polling.
     */
    @Configuration
    @ConditionalOnProperty(name = "shenyu.sync.http.enabled", havingValue = "true")
    @EnableConfigurationProperties(HttpSyncProperties.class)
    static class HttpLongPollingListener {

        @Bean
        @ConditionalOnMissingBean(HttpLongPollingDataChangedListener.class)
        public HttpLongPollingDataChangedListener httpLongPollingDataChangedListener(final HttpSyncProperties httpSyncProperties) {
            return new HttpLongPollingDataChangedListener(httpSyncProperties);
        }
    }

    /**
     * The type Zookeeper listener.
     */
    @Configuration
    @ConditionalOnProperty(prefix = "shenyu.sync.zookeeper", name = "url")
    @EnableConfigurationProperties(ZookeeperProperties.class)
    static class ZookeeperListener {

        /**
         * register ZookeeperClient in spring ioc.
         *
         * @param zookeeperProp the zookeeper configuration
         * @return ZookeeperClient {@linkplain ZookeeperClient}
         */
        @Bean
        @ConditionalOnMissingBean(ZookeeperClient.class)
        public ZookeeperClient zookeeperClient(final ZookeeperProperties zookeeperProp) {
            int sessionTimeout = Objects.isNull(zookeeperProp.getSessionTimeout()) ? 3000 : zookeeperProp.getSessionTimeout();
            int connectionTimeout = Objects.isNull(zookeeperProp.getConnectionTimeout()) ? 3000 : zookeeperProp.getConnectionTimeout();
            ZookeeperConfig zkConfig = new ZookeeperConfig(zookeeperProp.getUrl());
            zkConfig.setSessionTimeoutMilliseconds(sessionTimeout)
                    .setConnectionTimeoutMilliseconds(connectionTimeout);
            ZookeeperClient client = new ZookeeperClient(zkConfig);
            client.start();
            return client;
        }

        /**
         * Config event listener data changed listener.
         *
         * @param zkClient the zk client
         * @return the data changed listener
         */
        @Bean
        @ConditionalOnMissingBean(ZookeeperDataChangedListener.class)
        public DataChangedListener zookeeperDataChangedListener(final ZookeeperClient zkClient) {
            return new ZookeeperDataChangedListener(zkClient);
        }

        /**
         * Zookeeper data init zookeeper data init.
         *
         * @param zkClient        the zk client
         * @return the zookeeper data init
         */
        @Bean
        @ConditionalOnMissingBean(ZookeeperDataChangedInit.class)
        public DataChangedInit zookeeperDataChangedInit(final ZookeeperClient zkClient) {
            return new ZookeeperDataChangedInit(zkClient);
        }
    }

    /**
     * The type Nacos listener.
     */
    @Configuration
    @ConditionalOnProperty(prefix = "shenyu.sync.nacos", name = "url")
    @EnableConfigurationProperties(NacosProperties.class)
    static class NacosListener {

        /**
         * register configService in spring ioc.
         *
         * @param nacosProp the nacos configuration
         * @return ConfigService {@linkplain ConfigService}
         * @throws Exception the exception
         */
        @Bean
        @ConditionalOnMissingBean(ConfigService.class)
        public ConfigService nacosConfigService(final NacosProperties nacosProp) throws Exception {
            Properties properties = new Properties();
            if (Objects.nonNull(nacosProp.getAcm()) && nacosProp.getAcm().isEnabled()) {
                // Use aliyun ACM service
                properties.put(PropertyKeyConst.ENDPOINT, nacosProp.getAcm().getEndpoint());
                properties.put(PropertyKeyConst.NAMESPACE, nacosProp.getAcm().getNamespace());
                // Use subaccount ACM administrative authority
                properties.put(PropertyKeyConst.ACCESS_KEY, nacosProp.getAcm().getAccessKey());
                properties.put(PropertyKeyConst.SECRET_KEY, nacosProp.getAcm().getSecretKey());
            } else {
                properties.put(PropertyKeyConst.SERVER_ADDR, nacosProp.getUrl());
                if (StringUtils.isNotBlank(nacosProp.getNamespace())) {
                    properties.put(PropertyKeyConst.NAMESPACE, nacosProp.getNamespace());
                }
                if (StringUtils.isNotBlank(nacosProp.getUsername())) {
                    properties.put(PropertyKeyConst.USERNAME, nacosProp.getUsername());
                }
                if (StringUtils.isNotBlank(nacosProp.getPassword())) {
                    properties.put(PropertyKeyConst.PASSWORD, nacosProp.getPassword());
                }
            }
            return NacosFactory.createConfigService(properties);
        }

        /**
         * Data changed listener data changed listener.
         *
         * @param configService the config service
         * @return the data changed listener
         */
        @Bean
        @ConditionalOnMissingBean(NacosDataChangedListener.class)
        public DataChangedListener nacosDataChangedListener(final ConfigService configService) {
            return new NacosDataChangedListener(configService);
        }

        /**
         * Nacos data init nacos data init.
         *
         * @param configService the config service
         * @return the nacos data init
         */
        @Bean
        @ConditionalOnMissingBean(NacosDataChangedInit.class)
        public DataChangedInit nacosDataChangedInit(final ConfigService configService) {
            return new NacosDataChangedInit(configService);
        }
    }

    /**
     * The WebsocketListener(default strategy).
     */
    @Configuration
    @ConditionalOnProperty(name = "shenyu.sync.websocket.enabled", havingValue = "true", matchIfMissing = true)
    @EnableConfigurationProperties(WebsocketSyncProperties.class)
    static class WebsocketListener {

        /**
         * Config event listener data changed listener.
         *
         * @return the data changed listener
         */
        @Bean
        @ConditionalOnMissingBean(WebsocketDataChangedListener.class)
        public DataChangedListener websocketDataChangedListener() {
            return new WebsocketDataChangedListener();
        }

        /**
         * Websocket collector.
         *
         * @return the websocket collector
         */
        @Bean
        @ConditionalOnMissingBean(WebsocketCollector.class)
        public WebsocketCollector websocketCollector() {
            return new WebsocketCollector();
        }

        /**
         * Server endpoint exporter server endpoint exporter.
         *
         * @return the server endpoint exporter
         */
        @Bean
        @ConditionalOnMissingBean(ServerEndpointExporter.class)
        public ServerEndpointExporter serverEndpointExporter() {
            return new ServerEndpointExporter();
        }
    }

    /**
     * The type Etcd listener.
     */
    @Configuration
    @ConditionalOnProperty(prefix = "shenyu.sync.etcd", name = "url")
    @EnableConfigurationProperties(EtcdProperties.class)
    static class EtcdListener {

        /**
         * Init etcd client.
         *
         * @param etcdProperties etcd properties
         * @return Etcd Client
         */
        @Bean
        public EtcdClient etcdClient(final EtcdProperties etcdProperties) {
            Client client = Client.builder()
                    .endpoints(etcdProperties.getUrl().split(","))
                    .build();
            return new EtcdClient(client);
        }

        /**
         * Config event listener data changed listener.
         *
         * @param etcdClient the etcd client
         * @return the data changed listener
         */
        @Bean
        @ConditionalOnMissingBean(EtcdDataDataChangedListener.class)
        public DataChangedListener etcdDataChangedListener(final EtcdClient etcdClient) {
            return new EtcdDataDataChangedListener(etcdClient);
        }

        /**
         * data init.
         *
         * @param etcdClient        the etcd client
         * @return the etcd data init
         */
        @Bean
        @ConditionalOnMissingBean(EtcdDataChangedInit.class)
        public DataChangedInit etcdDataChangedInit(final EtcdClient etcdClient) {
            return new EtcdDataChangedInit(etcdClient);
        }
    }

    /**
     * The type Consul listener.
     */
    @Configuration
    @ConditionalOnProperty(prefix = "shenyu.sync.consul", name = "url")
    @EnableConfigurationProperties(ConsulProperties.class)
    static class ConsulListener {

        /**
         * init Consul client.
         * @param consulProperties the consul properties
         * @return Consul client
         */
        @Bean
        public ConsulClient consulClient(final ConsulProperties consulProperties) {
            return new ConsulClient(consulProperties.getUrl());
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
}

