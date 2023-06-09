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

package org.apache.shenyu.plugin.springcloud.cache;

import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.springframework.cloud.client.ServiceInstance;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class ServiceInstanceCache {
    
    private static final Map<String, List<ServiceInstance>> SERVICE_INSTANCE_MAP = Maps.newConcurrentMap();
    
    /**
     * Cache service instance.
     *
     * @param serviceId      service id
     * @param serviceInstances service instance list
     */
    public static void cacheServiceInstance(final String serviceId, final List<ServiceInstance> serviceInstances) {
        if (StringUtils.isNotBlank(serviceId)) {
            SERVICE_INSTANCE_MAP.put(serviceId, Optional.ofNullable(serviceInstances).orElse(Collections.emptyList()));
        }
    }
    
    /**
     * Remove service instance.
     * @param serviceId service id
     * @return the list of {@linkplain ServiceInstance}
     */
    public static List<ServiceInstance> getServiceInstance(final String serviceId) {
        if (StringUtils.isBlank(serviceId)) {
            return Collections.emptyList();
        }
        return SERVICE_INSTANCE_MAP.get(serviceId);
    }
    
    /**
     * Remove service instance.
     *
     * @param serviceId service id
     */
    public static void removeServiceInstance(final String serviceId) {
        if (StringUtils.isBlank(serviceId)) {
            return;
        }
        SERVICE_INSTANCE_MAP.remove(serviceId);
    }
}
