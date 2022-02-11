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

package org.apache.shenyu.client.tars;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Tars context refreshed event listener.
 */
public class TarsContextRefreshedEventListener implements ApplicationListener<ContextRefreshedEvent> {

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final AtomicBoolean registered = new AtomicBoolean(false);
    
    private final String contextPath;
    
    private final String ipAndPort;

    private final String host;

    private final int port;
    
    /**
     * Instantiates a new Tars context refreshed event listener.
     *
     * @param clientConfig the client config
     */
    public TarsContextRefreshedEventListener(final PropertiesConfig clientConfig) {
        Properties props = clientConfig.getProps();
        String contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
        this.host = props.getProperty(ShenyuClientConstants.HOST);
        String port = props.getProperty(ShenyuClientConstants.PORT);
        if (StringUtils.isAnyBlank(contextPath, this.host, port)) {
            throw new ShenyuClientIllegalArgumentException("tars client must config the contextPath, ipAndPort");
        }
        this.contextPath = contextPath;
        this.ipAndPort = this.host + ":" + port;
        this.port = Integer.parseInt(port);
    }
    
    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        publisher.publishEvent(buildURIRegisterDTO());
    }
    
    private URIRegisterDTO buildURIRegisterDTO() {
        return URIRegisterDTO.builder()
                .contextPath(this.contextPath)
                .appName(this.ipAndPort)
                .rpcType(RpcTypeEnum.TARS.getName())
                .host(host)
                .port(port)
                .build();
    }
}
