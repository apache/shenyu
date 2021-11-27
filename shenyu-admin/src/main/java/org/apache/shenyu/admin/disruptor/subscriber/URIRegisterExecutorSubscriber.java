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

package org.apache.shenyu.admin.disruptor.subscriber;

import com.google.common.collect.Lists;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.service.register.ShenyuClientRegisterService;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;

import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * The type Uri register executor subscriber.
 */
public class URIRegisterExecutorSubscriber implements ExecutorTypeSubscriber<URIRegisterDTO> {
    
    private final Map<String, ShenyuClientRegisterService> shenyuClientRegisterService;
    
    /**
     * Instantiates a new Uri register executor subscriber.
     *
     * @param shenyuClientRegisterService the shenyu client register service
     */
    public URIRegisterExecutorSubscriber(final Map<String, ShenyuClientRegisterService> shenyuClientRegisterService) {
        this.shenyuClientRegisterService = shenyuClientRegisterService;
    }
    
    @Override
    public DataType getType() {
        return DataType.URI;
    }
    
    @Override
    public void executor(final Collection<URIRegisterDTO> dataList) {
        if (CollectionUtils.isEmpty(dataList)) {
            return;
        }
        findService(dataList).ifPresent(service -> {
            Map<String, List<URIRegisterDTO>> listMap = buildData(dataList);
            listMap.forEach(service::registerURI);
        });
    }
    
    private Map<String, List<URIRegisterDTO>> buildData(final Collection<URIRegisterDTO> dataList) {
        Map<String, List<URIRegisterDTO>> resultMap = new HashMap<>();
        for (URIRegisterDTO dto : dataList) {
            String contextPath = dto.getContextPath();
            String key = StringUtils.isNotEmpty(contextPath) ? contextPath : dto.getAppName();
            if (StringUtils.isNotEmpty(key)) {
                if (resultMap.containsKey(key)) {
                    List<URIRegisterDTO> existList = resultMap.get(key);
                    existList.add(dto);
                    resultMap.put(key, existList);
                } else {
                    resultMap.put(key, Lists.newArrayList(dto));
                }
            }
        }
        return resultMap;
    }
    
    private Optional<ShenyuClientRegisterService> findService(final Collection<URIRegisterDTO> dataList) {
        return dataList.stream().map(dto -> shenyuClientRegisterService.get(dto.getRpcType())).findFirst();
    }
}
