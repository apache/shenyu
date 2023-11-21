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

package org.apache.shenyu.register.client.server.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.QueryParams;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.agent.model.Service;
import com.ecwid.consul.v1.kv.model.GetValue;
import com.google.common.collect.Lists;
import com.google.common.collect.MapDifference;
import com.google.common.collect.Maps;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Consul client server register repository.
 */
@Join
public class ConsulClientServerRegisterRepository implements ShenyuClientServerRegisterRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(ConsulClientServerRegisterRepository.class);

    private ConsulClient consulClient;

    private Map<String, List<URIRegisterDTO>> uriRegisterDTOMap = new HashMap<>();

    private ShenyuClientServerRegisterPublisher publisher;

    private final Map<String, Long> indexMap = new HashMap<>();

    private final Map<String, String> uriRpcTypeMap = new HashMap<>();

    private int waitTime;

    private int watchDelay;

    private final AtomicBoolean running = new AtomicBoolean(false);

    private final Map<String, Long> consulIndexes = new HashMap<>();

    private ScheduledThreadPoolExecutor executor;

    @Override
    public void init(final ShenyuClientServerRegisterPublisher publisher,
                     final ShenyuRegisterCenterConfig config) {
        this.publisher = publisher;

        final String serverList = config.getServerLists();
        if (StringUtils.isBlank(serverList)) {
            throw new ShenyuException("serverList can not be null.");
        }
        final String[] addresses = splitAndCheckAddress(serverList);
        this.consulClient = new ConsulClient(addresses[0], Integer.parseInt(addresses[1]));
        this.watchDelay = Integer.parseInt(config.getProps().getProperty("delay", "10000"));
        this.waitTime = Integer.parseInt(config.getProps().getProperty("wait-time", "10000"));
        this.executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("consul-config-watch", true));
        String metadataPath = config.getProps().getProperty("metadata-path", "shenyu/register");
        consulIndexes.put(metadataPath, 0L);
        if (this.running.compareAndSet(false, true)) {
            watchConfigKeyValues();
        }
    }

    private void watchConfigKeyValues() {
        if (!this.running.get()) {
            return;
        }
        consulIndexes.keySet().forEach(watchPathRoot -> {
            try {
                Long currentIndex = this.consulIndexes.computeIfAbsent(watchPathRoot, k -> -1L);
                Response<List<GetValue>> response = this.consulClient
                        .getKVValues(watchPathRoot, null, new QueryParams(TimeUnit.MILLISECONDS.toSeconds(waitTime), currentIndex));
                if (CollectionUtils.isEmpty(response.getValue())) {
                    if (LOGGER.isTraceEnabled()) {
                        LOGGER.warn("No value for watchPathRoot {}", watchPathRoot);
                    }
                    this.executor.schedule(this::watchConfigKeyValues, watchDelay, TimeUnit.MILLISECONDS);
                    return;
                }
                Long newIndex = response.getConsulIndex();
                if (Objects.isNull(newIndex)) {
                    if (LOGGER.isTraceEnabled()) {
                        LOGGER.trace("Same index for watchPathRoot {}", watchPathRoot);
                    }
                    this.executor.schedule(this::watchConfigKeyValues, watchDelay, TimeUnit.MILLISECONDS);
                    return;
                }
                if (Objects.equals(newIndex, currentIndex)) {
                    this.executor.schedule(this::watchConfigKeyValues, -1, TimeUnit.MILLISECONDS);
                    return;
                }
                if (!this.consulIndexes.containsValue(newIndex)
                        && !currentIndex.equals(-1L)) {
                    LOGGER.trace("Context {} has new index {}", watchPathRoot, newIndex);
                    Map<String, GetValue> valueMap = extractGetValue(response);
                    onInstanceChange(new ConsulConfigChangedEvent(this, newIndex, valueMap));
                    onMetadataChange(new ConsulConfigChangedEvent(this, newIndex, valueMap));
                } else if (LOGGER.isTraceEnabled()) {
                    LOGGER.info("Event for index already published for watchPathRoot {}", watchPathRoot);
                }
                this.consulIndexes.put(watchPathRoot, newIndex);
                this.executor.schedule(this::watchConfigKeyValues, -1, TimeUnit.MILLISECONDS);
            } catch (Exception e) {
                LOGGER.error("Error querying consul Key/Values for watchPathRoot '{}'. Message: {}", watchPathRoot, e.getMessage());
                this.executor.schedule(this::watchConfigKeyValues, watchDelay, TimeUnit.MILLISECONDS);
            }
        });
    }

    private Map<String, GetValue> extractGetValue(final Response<List<GetValue>> response) {
        Map<String, GetValue> valueMap = new HashMap<>();
        List<GetValue> values = response.getValue();
        values.forEach(getValue -> valueMap.put(getValue.getKey(), getValue));
        return valueMap;
    }
    
    /**
     * Listen service instance change.
     *
     * @param event the service instances change event
     */
    private void onInstanceChange(final ConsulConfigChangedEvent event) {
        Map<String, List<URIRegisterDTO>> uriMap = fetchInstancesMap();
        MapDifference<String, List<URIRegisterDTO>> difference = Maps.difference(uriMap, uriRegisterDTOMap);
        difference.entriesOnlyOnLeft().forEach(this::publishRegisterURI);
        difference.entriesOnlyOnRight().keySet().forEach(contextPath -> publishRegisterURI(contextPath, new ArrayList<>()));
        difference.entriesDiffering().forEach((contextPath, listValueDifference) -> publishRegisterURI(contextPath, listValueDifference.leftValue()));
        uriRegisterDTOMap = uriMap;
    }
    
    /**
     * Listen metadata change.
     *
     * @param event the metadata change event
     */
    private void onMetadataChange(final ConsulConfigChangedEvent event) {
        Map<String, GetValue> metadataMap = event.getMetadataMap();
        metadataMap.forEach((path, getValue) -> {
            long modifyIndex = getValue.getModifyIndex();
            if (metadataChanged(path, modifyIndex)) {
                publishMetadata(getValue.getDecodedValue());
            }
        });
    }

    private String[] splitAndCheckAddress(final String serverList) {
        final String[] addresses = serverList.split(":");
        if (addresses.length != 2) {
            throw new ShenyuException("serverList formatter is not incorrect.");
        }
        return addresses;
    }

    private void publishMetadata(final String data) {
        publisher.publish(Lists.newArrayList(GsonUtils.getInstance().fromJson(data, MetaDataRegisterDTO.class)));
    }

    private void publishRegisterURI(final String contextPath, final List<URIRegisterDTO> registerDTOList) {
        if (registerDTOList.isEmpty()) {
            URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder().contextPath(contextPath).rpcType(uriRpcTypeMap.get(contextPath)).build();
            registerDTOList.add(uriRegisterDTO);
        } else {
            uriRpcTypeMap.put(contextPath, registerDTOList.get(0).getRpcType());
        }
        publisher.publish(registerDTOList);
    }

    private Map<String, List<URIRegisterDTO>> fetchInstancesMap() {
        Map<String, List<URIRegisterDTO>> map = new HashMap<>();
        Response<Map<String, Service>> agentServices = consulClient.getAgentServices();
        Map<String, Service> agentServicesValue = agentServices.getValue();
        agentServicesValue.forEach((k, v) -> {
            String data = v.getMeta().get(Constants.URI);
            if (Objects.nonNull(data)) {
                URIRegisterDTO uriRegisterDTO = GsonUtils.getInstance().fromJson(data, URIRegisterDTO.class);
                String contextPath = uriRegisterDTO.getContextPath();
                map.putIfAbsent(contextPath, new ArrayList<>());
                map.get(contextPath).add(uriRegisterDTO);
            } else {
                LOGGER.debug("maybe not shenyu client, ignore service instance: {}", v);
            }
        });
        return map;
    }

    private boolean metadataChanged(final String path, final long index) {
        boolean hasResult = !indexMap.containsKey(path) || indexMap.get(path) < index;
        if (hasResult) {
            indexMap.put(path, index);
        }
        return hasResult;
    }

    @Override
    public void close() {
        if (this.running.compareAndSet(true, false)) {
            executor.shutdown();
        }
    }

}
