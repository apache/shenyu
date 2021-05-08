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

import org.apache.shenyu.admin.service.SoulClientRegisterService;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.subsriber.ExecutorTypeSubscriber;
import org.apache.shenyu.register.common.type.DataType;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The type Uri register executor subscriber.
 *
 * @author xiaoyu
 */
public class URIRegisterExecutorSubscriber implements ExecutorTypeSubscriber<URIRegisterDTO> {
    
    private final SoulClientRegisterService soulClientRegisterService;
    
    /**
     * Instantiates a new Uri register executor subscriber.
     *
     * @param soulClientRegisterService the soul client register service
     */
    public URIRegisterExecutorSubscriber(final SoulClientRegisterService soulClientRegisterService) {
        this.soulClientRegisterService = soulClientRegisterService;
    }
    
    @Override
    public DataType getType() {
        return DataType.URI;
    }
    
    @Override
    public void executor(final Collection<URIRegisterDTO> dataList) {
        Map<String, List<URIRegisterDTO>> listMap = dataList.stream().collect(Collectors.groupingBy(URIRegisterDTO::getContextPath));
        listMap.forEach((contextPath, dtoList) -> {
            List<String> uriList = new ArrayList<>();
            dataList.forEach(uriRegisterDTO -> {
                if (uriRegisterDTO.getHost() != null && uriRegisterDTO.getPort() != null) {
                    uriList.add(String.join(":", uriRegisterDTO.getHost(), uriRegisterDTO.getPort().toString()));
                }
            });
            soulClientRegisterService.registerURI(contextPath, uriList);
        });
    }
}
