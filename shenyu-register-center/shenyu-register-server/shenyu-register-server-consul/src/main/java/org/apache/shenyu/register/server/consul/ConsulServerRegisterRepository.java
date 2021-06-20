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

package org.apache.shenyu.register.server.consul;

import com.ecwid.consul.v1.kv.model.GetValue;
import com.google.common.collect.Lists;
import com.google.common.collect.MapDifference;
import com.google.common.collect.Maps;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.server.api.ShenyuServerRegisterPublisher;
import org.apache.shenyu.register.server.api.ShenyuServerRegisterRepository;
import org.apache.shenyu.spi.Join;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cloud.client.ServiceInstance;
import org.springframework.cloud.client.discovery.event.HeartbeatEvent;
import org.springframework.cloud.consul.discovery.ConsulDiscoveryClient;
import org.springframework.context.event.EventListener;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Join
@Slf4j
public class ConsulServerRegisterRepository implements ShenyuServerRegisterRepository {

    @Autowired
    private ConsulDiscoveryClient discoveryClient;

    private long index;

    private Map<String, List<URIRegisterDTO>> uriRegisterDTOMap = new HashMap<>();

    private ShenyuServerRegisterPublisher publisher;

    private final Map<String, Long> indexMap = new HashMap<>();

    @Override
    public void init(final ShenyuServerRegisterPublisher publisher, final ShenyuRegisterCenterConfig config) {
        this.publisher = publisher;
    }

    /**
     * Listen service instance change.
     *
     * @param event the service instances change event
     */
    @EventListener
    public void onInstanceChange(final HeartbeatEvent event) {
        long i = Long.parseLong(event.getValue().toString());
        if (i > index) {
            index = i;
            Map<String, List<URIRegisterDTO>> uriMap = fetchInstancesMap();
            MapDifference<String, List<URIRegisterDTO>> difference = Maps.difference(uriMap, uriRegisterDTOMap);
            difference.entriesOnlyOnLeft().forEach(this::publishRegisterURI);
            difference.entriesOnlyOnRight().keySet().forEach(contextPath -> publishRegisterURI(contextPath, new ArrayList<>()));
            difference.entriesDiffering().forEach((contextPath, listValueDifference) -> publishRegisterURI(contextPath, listValueDifference.leftValue()));
            uriRegisterDTOMap = uriMap;
        }
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
            URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder().contextPath(contextPath).build();
            registerDTOList.add(uriRegisterDTO);
        }
        publisher.publish(registerDTOList);
    }

    private Map<String, List<URIRegisterDTO>> fetchInstancesMap() {
        Map<String, List<URIRegisterDTO>> map = new HashMap<>();
        List<ServiceInstance> instances = discoveryClient.getAllInstances();
        instances.forEach(serviceInstance -> {
            String data = serviceInstance.getMetadata().get("uri");
            if (null != data) {
                URIRegisterDTO uriRegisterDTO = GsonUtils.getInstance().fromJson(data, URIRegisterDTO.class);
                String contextPath = uriRegisterDTO.getContextPath();
                map.putIfAbsent(contextPath, new ArrayList<>());
                map.get(contextPath).add(uriRegisterDTO);
            } else {
                log.debug("maybe not shenyu client, ignore service instance: {}", serviceInstance);
            }
        });
        return map;
    }

    private boolean metadataChanged(final String path, final long index) {
        boolean result = !indexMap.containsKey(path) || indexMap.get(path) < index;
        if (result) {
            indexMap.put(path, index);
        }
        return result;
    }

}
