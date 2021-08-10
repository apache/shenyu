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

import com.alibaba.nacos.api.config.ConfigService;
import com.ecwid.consul.v1.ConsulClient;
import io.etcd.jetcd.Client;
import org.I0Itec.zkclient.ZkClient;
import org.apache.shenyu.admin.config.properties.ConsulProperties;
import org.apache.shenyu.admin.config.properties.EtcdProperties;
import org.apache.shenyu.admin.config.properties.HttpSyncProperties;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.listener.consul.ConsulDataChangedListener;
import org.apache.shenyu.admin.listener.consul.ConsulDataInit;
import org.apache.shenyu.admin.listener.etcd.EtcdClient;
import org.apache.shenyu.admin.listener.etcd.EtcdDataDataChangedListener;
import org.apache.shenyu.admin.listener.etcd.EtcdDataInit;
import org.apache.shenyu.admin.listener.http.HttpLongPollingDataChangedListener;
import org.apache.shenyu.admin.listener.nacos.NacosDataChangedListener;
import org.apache.shenyu.admin.listener.nacos.NacosDataInit;
import org.apache.shenyu.admin.listener.websocket.WebsocketCollector;
import org.apache.shenyu.admin.listener.websocket.WebsocketDataChangedListener;
import org.apache.shenyu.admin.listener.zookeeper.ZookeeperDataChangedListener;
import org.apache.shenyu.admin.listener.zookeeper.ZookeeperDataInit;
import org.apache.shenyu.admin.service.SyncDataService;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.web.socket.server.standard.ServerEndpointExporter;

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
    @Import(ZookeeperConfiguration.class)
    static class ZookeeperListener {

        /**
         * Config event listener data changed listener.
         *
         * @param zkClient the zk client
         * @return the data changed listener
         */
        @Bean
        @ConditionalOnMissingBean(ZookeeperDataChangedListener.class)
        public DataChangedListener zookeeperDataChangedListener(final ZkClient zkClient) {
            return new ZookeeperDataChangedListener(zkClient);
        }

        /**
         * Zookeeper data init zookeeper data init.
         *
         * @param zkClient        the zk client
         * @param syncDataService the sync data service
         * @return the zookeeper data init
         */
        @Bean
        @ConditionalOnMissingBean(ZookeeperDataInit.class)
        public ZookeeperDataInit zookeeperDataInit(final ZkClient zkClient, final SyncDataService syncDataService) {
            return new ZookeeperDataInit(zkClient, syncDataService);
        }
    }

    /**
     * The type Nacos listener.
     */
    @Configuration
    @ConditionalOnProperty(prefix = "shenyu.sync.nacos", name = "url")
    @Import(NacosConfiguration.class)
    static class NacosListener {

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
         * Nacos data init zookeeper data init.
         *
         * @param configService the config service
         * @param syncDataService the sync data service
         * @return the nacos data init
         */
        @Bean
        @ConditionalOnMissingBean(NacosDataInit.class)
        public NacosDataInit nacosDataInit(final ConfigService configService, final SyncDataService syncDataService) {
            return new NacosDataInit(configService, syncDataService);
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
         * Websocket collector websocket collector.
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

        @Bean
        public EtcdClient etcdClient(final EtcdProperties etcdProperties) {
            Client client = Client.builder()
                    .endpoints(etcdProperties.getUrl())
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
         * @param syncDataService the sync data service
         * @return the etcd data init
         */
        @Bean
        @ConditionalOnMissingBean(EtcdDataInit.class)
        public EtcdDataInit etcdDataInit(final EtcdClient etcdClient, final SyncDataService syncDataService) {
            return new EtcdDataInit(etcdClient, syncDataService);
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
         * @param syncDataService the sync data service
         * @return the consul data init
         */
        @Bean
        @ConditionalOnMissingBean(ConsulDataInit.class)
        public ConsulDataInit consulDataInit(final ConsulClient consulClient, final SyncDataService syncDataService) {
            return new ConsulDataInit(consulClient, syncDataService);
        }
    }
}

