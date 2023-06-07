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
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.agent.model.Service;
import com.ecwid.consul.v1.kv.model.GetValue;
import com.google.common.collect.Lists;
import com.google.common.collect.MapDifference;
import com.google.common.collect.Maps;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterRepository;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

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

    @Override
    public void init(final ShenyuClientServerRegisterPublisher publisher,
                     final ShenyuRegisterCenterConfig config) {
        this.publisher = publisher;
        consulClient = new ConsulClient(config.getServerLists());
    }
    
    /**
     * Listen service instance change.
     *
     * @param event the service instances change event
     */
    @EventListener
    public void onInstanceChange(final ConsulConfigChangedEvent event) {
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
    @EventListener
    public void onMetadataChange(final ConsulConfigChangedEvent event) {
        Map<String, GetValue> metadataMap = event.getMetadataMap();
        metadataMap.forEach((path, getValue) -> {
            long modifyIndex = getValue.getModifyIndex();
            if (metadataChanged(path, modifyIndex)) {
                publishMetadata(getValue.getDecodedValue());
            }
        });
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

}
