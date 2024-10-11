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

package org.apache.shenyu.plugin.springcloud.listener;


import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.utils.LogUtils;
import org.apache.shenyu.plugin.springcloud.cache.ServiceInstanceCache;
import org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler;
import org.apache.shenyu.registry.api.entity.InstanceEntity;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.cloud.client.discovery.event.HeartbeatEvent;
import org.springframework.context.ApplicationListener;

import java.util.List;
import java.util.Map;

import static org.apache.shenyu.plugin.springcloud.handler.SpringCloudPluginDataHandler.SELECTOR_CACHED;

/**
 * SpringCloud HeartBeat Listener.
 */
public class SpringCloudHeartBeatListener implements ApplicationListener<HeartbeatEvent> {
    
    private static final Logger LOG = LoggerFactory.getLogger(SpringCloudHeartBeatListener.class);
    
    private final ShenyuConfig.SpringCloudCacheConfig cacheConfig;
    
    public SpringCloudHeartBeatListener(final ShenyuConfig.SpringCloudCacheConfig cacheConfig) {
        this.cacheConfig = cacheConfig;
    }
    
    @Override
    public void onApplicationEvent(final HeartbeatEvent event) {
        if (!cacheConfig.getEnabled()) {
            return;
        }
        LogUtils.debug(LOG, "shenyu receive spring cloud heartbeat event");
        Map<String, SpringCloudSelectorHandle> map = SELECTOR_CACHED.get().getAllCache();
        if (MapUtils.isEmpty(map)) {
            return;
        }
        map.forEach((key, value) -> {
            String serviceId = value.getServiceId();
            List<InstanceEntity> serviceInstanceList = SpringCloudPluginDataHandler.getRepository().selectInstances(serviceId);
            ServiceInstanceCache.cacheServiceInstance(serviceId, serviceInstanceList);
        });
    }
}
