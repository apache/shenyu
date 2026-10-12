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

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.config.properties.WebsocketSyncProperties;
import org.apache.shenyu.admin.listener.DataChangedListener;
import org.apache.shenyu.admin.listener.websocket.WebsocketCollector;
import org.apache.shenyu.admin.listener.websocket.WebsocketDataChangedListener;
import org.apache.shenyu.admin.listener.websocket.WebsocketDataReconciler;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.service.AiProxyApiKeyService;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.NamespacePluginService;
import org.apache.shenyu.admin.service.ProxySelectorService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.server.standard.ServerEndpointExporter;

/**
 * The WebsocketListener(default strategy).
 */
@Configuration
@ConditionalOnProperty(name = "shenyu.sync.websocket.enabled", havingValue = "true", matchIfMissing = true)
@EnableConfigurationProperties(WebsocketSyncProperties.class)
public class WebSocketSyncConfiguration {

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
     * Websocket data reconciler, converges gateways connected to this admin node
     * with configuration written through other admin nodes sharing the same database.
     *
     * @param websocketSyncProperties the websocket sync properties
     * @param clusterProperties the cluster properties
     * @param masterServiceProvider the cluster master service provider
     * @param appAuthService the app auth service
     * @param namespacePluginService the namespace plugin service
     * @param selectorService the selector service
     * @param ruleService the rule service
     * @param metaDataService the meta data service
     * @param proxySelectorService the proxy selector service
     * @param discoveryUpstreamService the discovery upstream service
     * @param aiProxyApiKeyService the ai proxy api key service
     * @return the websocket data reconciler
     */
    @Bean
    @ConditionalOnMissingBean(WebsocketDataReconciler.class)
    public WebsocketDataReconciler websocketDataReconciler(final WebsocketSyncProperties websocketSyncProperties,
                                                           final ClusterProperties clusterProperties,
                                                           final ObjectProvider<ClusterSelectMasterService> masterServiceProvider,
                                                           final AppAuthService appAuthService,
                                                           final NamespacePluginService namespacePluginService,
                                                           final SelectorService selectorService,
                                                           final RuleService ruleService,
                                                           final MetaDataService metaDataService,
                                                           final ProxySelectorService proxySelectorService,
                                                           final DiscoveryUpstreamService discoveryUpstreamService,
                                                           final AiProxyApiKeyService aiProxyApiKeyService) {
        return new WebsocketDataReconciler(websocketSyncProperties, clusterProperties, masterServiceProvider,
                appAuthService, namespacePluginService, selectorService, ruleService, metaDataService,
                proxySelectorService, discoveryUpstreamService, aiProxyApiKeyService);
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
