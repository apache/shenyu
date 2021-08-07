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

package org.apache.shenyu.sync.data.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.QueryParams;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.kv.model.GetValue;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.ConsulConstants;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.apache.shenyu.sync.data.api.SyncDataService;
import org.apache.shenyu.sync.data.consul.config.ConsulConfig;
import org.apache.shenyu.sync.data.consul.handler.ConsulCacheHandler;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Consul sync data service.
 */
public class ConsulSyncDataService extends ConsulCacheHandler implements AutoCloseable, SyncDataService {
    /**
     * logger. 
     */
    private static final Logger LOG = LoggerFactory.getLogger(ConsulSyncDataService.class);

    private final Map<String, OnChange> groupMap = new HashMap<>();

    private final Map<String, Long> consulIndexes = new HashMap<>();

    private final ScheduledThreadPoolExecutor executor;

    private ScheduledFuture<?> watchFuture;

    private ConsulConfig consulConfig;

    private ConsulClient consulClient;

    private final AtomicBoolean running = new AtomicBoolean(false);

    /**
     * Instantiates a new Consul sync data service.
     *
     * @param pluginDataSubscriber the plugin data subscriber
     * @param metaDataSubscribers  the meta data subscribers
     * @param authDataSubscribers  the auth data subscribers
     */
    public ConsulSyncDataService(final ConsulClient consulClient, final ConsulConfig consulConfig, final PluginDataSubscriber pluginDataSubscriber,
                                 final List<MetaDataSubscriber> metaDataSubscribers, final List<AuthDataSubscriber> authDataSubscribers) {
        super(pluginDataSubscriber, metaDataSubscribers, authDataSubscribers);
        this.consulClient = consulClient;
        this.consulConfig = consulConfig;
        this.executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("consul-config-watch", true));
        consulIndexes.put(ConsulConstants.SYNC_PRE_FIX, 0L);
        initUpdateMap();
        start();
    }

    /**
     * init config key and update method mapping.
     */
    private void initUpdateMap() {
        groupMap.put(ConsulConstants.PLUGIN_DATA, this::updatePluginData);
        groupMap.put(ConsulConstants.SELECTOR_DATA, this::updateSelectorMap);
        groupMap.put(ConsulConstants.RULE_DATA, this::updateRuleMap);
        groupMap.put(ConsulConstants.META_DATA, this::updateMetaDataMap);
        groupMap.put(ConsulConstants.AUTH_DATA, this::updateAuthMap);
    }

    private void watchConfigKeyValues() {
        if (this.running.get()) {
            for (String context : this.consulIndexes.keySet()) {
                try {
                    Long currentIndex = this.consulIndexes.get(context);
                    if (currentIndex == null) {
                        currentIndex = ConsulConstants.INIT_CONFIG_VERSION_INDEX;
                    }
                    Response<List<GetValue>> response = this.consulClient.getKVValues(context, null,
                            new QueryParams(consulConfig.getWaitTime(), currentIndex));
                    if (response.getValue() == null || response.getValue().isEmpty()) {
                        if (LOG.isTraceEnabled()) {
                            LOG.trace("No value for context " + context);
                        }
                        continue;
                    }
                    Long newIndex = response.getConsulIndex();
                    if (newIndex == null || newIndex.equals(currentIndex)) {
                        if (LOG.isTraceEnabled()) {
                            LOG.trace("Same index for context " + context);
                        }
                        continue;
                    }
                    if (!this.consulIndexes.containsValue(newIndex)
                            && !currentIndex.equals(ConsulConstants.INIT_CONFIG_VERSION_INDEX)) {
                        if (LOG.isTraceEnabled()) {
                            LOG.trace("Context " + context + " has new index " + newIndex);
                        }
                        final Long lastIndex = currentIndex;
                        response.getValue().forEach(data -> {
                            if (data.getModifyIndex() == lastIndex) {
                                //data has not changed
                                return;
                            }
                            groupMap.get(data.getKey()).change(data.getDecodedValue());
                        });

                    } else if (LOG.isTraceEnabled()) {
                        LOG.info("Event for index already published for context " + context);
                    }
                    this.consulIndexes.put(context, newIndex);
                } catch (Exception e) {
                    LOG.warn("Error querying consul Key/Values for context '" + context + "'. Message: " + e.getMessage());
                }
            }
        }
    }

    /**
     * Start.
     */
    public void start() {
        if (this.running.compareAndSet(false, true)) {
            this.watchFuture = this.executor.scheduleWithFixedDelay(this::watchConfigKeyValues,
                    5, consulConfig.getWatchDelay(), TimeUnit.MILLISECONDS);
        }
    }

    @Override
    public void close() {
        if (this.running.compareAndSet(true, false) && this.watchFuture != null) {
            this.watchFuture.cancel(true);
        }
    }
}
