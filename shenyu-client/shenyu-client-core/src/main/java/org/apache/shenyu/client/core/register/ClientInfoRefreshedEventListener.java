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

import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.utils.PortUtils;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

import java.util.Optional;
import java.util.Properties;

public final class ClientInfoRefreshedEventListener implements ApplicationListener<ContextRefreshedEvent> {

    private final boolean addPrefixed;

    private final boolean isFull;

    private final String contextPath;

    private final String appName;

    private final RpcTypeEnum rpcTypeEnum;

    private final String ipAndPort;

    private final String host;

    private final String port;

    private final ShenyuClientRegisterEventPublisher publisher;

    public ClientInfoRefreshedEventListener(final PropertiesConfig clientConfig,
                                            final ShenyuClientRegisterEventPublisher publisher,
                                            final RpcTypeEnum rpcTypeEnum) {

        Properties props = clientConfig.getProps();

        this.addPrefixed = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.ADD_PREFIXED,
                Boolean.FALSE.toString()));

        this.isFull = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString()));

        this.contextPath = Optional.ofNullable(props
                .getProperty(ShenyuClientConstants.CONTEXT_PATH))
                .map(UriUtils::repairData).orElse("");

        this.appName = props.getProperty(ShenyuClientConstants.APP_NAME);

        this.rpcTypeEnum = rpcTypeEnum;

        this.ipAndPort = props.getProperty(ShenyuClientConstants.IP_PORT);

        this.host = props.getProperty(ShenyuClientConstants.HOST);

        this.port = props.getProperty(ShenyuClientConstants.PORT);

        this.publisher = publisher;
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {

        final int port = Integer.parseInt(Optional.ofNullable(host).orElseGet(() -> "-1"));
        final int mergedPort = port <= 0 ? PortUtils.findPort(event.getApplicationContext().getAutowireCapableBeanFactory()) : port;

        URIRegisterDTO uriRegisterDTO = URIRegisterDTO.builder()
                .contextPath(contextPath)
                .appName(appName)
                .rpcType(rpcTypeEnum.getName())
                .host(IpUtils.isCompleteHost(host) ? host : IpUtils.getHost(host))
                .port(mergedPort)
                .build();

        publisher.publishEvent(uriRegisterDTO);
    }
}
