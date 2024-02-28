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

package org.apache.shenyu.client.core.register.registrar;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.ClientRegisterConfig;
import org.apache.shenyu.client.core.utils.ApiBeanUtil;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;

/**
 * UriApiRegistrarImplImpl.
 *
 * @see URIRegisterDTO
 */
public class UriApiRegistrarImplImpl extends BaseApiRegistrarImpl {
    
    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
    
    private final ClientRegisterConfig clientRegisterConfig;
    
    public UriApiRegistrarImplImpl(final ClientRegisterConfig clientRegisterConfig) {
        this.clientRegisterConfig = clientRegisterConfig;
    }
    
    @Override
    protected void doRegisterApi(final ApiBean.ApiDefinition api) {
        final URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder()
                .contextPath(clientRegisterConfig.getContextPath())
                .appName(clientRegisterConfig.getAppName())
                .eventType(EventType.REGISTER)
                .host(clientRegisterConfig.getHost())
                .port(clientRegisterConfig.getPort())
                .rpcType(getRpcType(api))
                .build();
        publisher.publishEvent(uriRegisterDTO);
    }
    
    @Override
    protected void doRegisterBean(final ApiBean apiBean) {
        final URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder()
                .contextPath(clientRegisterConfig.getContextPath())
                .appName(clientRegisterConfig.getAppName())
                .eventType(EventType.REGISTER)
                .host(clientRegisterConfig.getHost())
                .port(clientRegisterConfig.getPort())
                .rpcType(getRpcType(apiBean))
                .build();
        publisher.publishEvent(uriRegisterDTO);
    }
    
    private static String getRpcType(final ApiBean.ApiDefinition api) {
        return ApiBeanUtil.getRpcType(api);
    }
    
    private static String getRpcType(final ApiBean api) {
        return ApiBeanUtil.getRpcType(api);
    }
    
}
