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

package org.apache.shenyu.client.core.register;

import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

public final class ClientInfoRefreshedEventListener implements ApplicationListener<ContextRefreshedEvent> {

    private final String contextPath;

    private final String appName;

    private final RpcTypeEnum rpcTypeEnum;

    private final String host;

    private final Integer port;

    private final ShenyuClientRegisterEventPublisher publisher;

    public ClientInfoRefreshedEventListener(final ClientRegisterConfig clientRegisterConfig,
                                            final ShenyuClientRegisterEventPublisher publisher) {

        this.contextPath = clientRegisterConfig.getContextPath();

        this.appName = clientRegisterConfig.getAppName();

        this.rpcTypeEnum = clientRegisterConfig.getRpcTypeEnum();

        this.host = clientRegisterConfig.getHost();

        this.port = clientRegisterConfig.getPort();

        this.publisher = publisher;
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {

        URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder()
                .contextPath(contextPath)
                .appName(appName)
                .rpcType(rpcTypeEnum.getName())
                .host(host)
                .port(port)
                .eventType(EventType.REGISTER)
                .build();

        publisher.publishEvent(uriRegisterDTO);
    }
}
