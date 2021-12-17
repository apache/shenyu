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

package org.apache.shenyu.client.springmvc.init;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.lang.NonNull;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Context register listener.
 */
public class ContextRegisterListener implements ApplicationListener<ContextRefreshedEvent> {

    private static final Logger LOG = LoggerFactory.getLogger(ContextRegisterListener.class);

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final AtomicBoolean registered = new AtomicBoolean(false);

    private String contextPath;

    private final String appName;
    
    private final String protocol;

    private final String host;

    private final Integer port;

    private final Boolean isFull;

    /**
     * Instantiates a new Context register listener.
     *
     * @param clientConfig the client config
     */
    public ContextRegisterListener(final PropertiesConfig clientConfig) {
        Properties props = clientConfig.getProps();
        this.isFull = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString()));
        String contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
        this.contextPath = contextPath;
        if (isFull) {
            if (StringUtils.isBlank(contextPath)) {
                String errorMsg = "http register param must config the contextPath";
                LOG.error(errorMsg);
                throw new ShenyuClientIllegalArgumentException(errorMsg);
            }
            this.contextPath = contextPath + "/**";
        }
        this.port = Integer.parseInt(props.getProperty(ShenyuClientConstants.PORT));
        this.appName = props.getProperty(ShenyuClientConstants.APP_NAME);
        this.protocol = props.getProperty(ShenyuClientConstants.PROTOCOL, ShenyuClientConstants.HTTP);
        this.host = props.getProperty(ShenyuClientConstants.HOST);
    }

    @Override
    public void onApplicationEvent(@NonNull final ContextRefreshedEvent contextRefreshedEvent) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        if (isFull) {
            publisher.publishEvent(buildMetaDataDTO());
        }
        publisher.publishEvent(buildURIRegisterDTO());
    }

    private URIRegisterDTO buildURIRegisterDTO() {
        String host = IpUtils.isCompleteHost(this.host) ? this.host : IpUtils.getHost(this.host);
        return URIRegisterDTO.builder()
                .contextPath(this.contextPath)
                .appName(appName)
                .protocol(protocol)
                .host(host)
                .port(port)
                .rpcType(RpcTypeEnum.HTTP.getName())
                .build();
    }

    private MetaDataRegisterDTO buildMetaDataDTO() {
        String contextPath = this.contextPath;
        String appName = this.appName;
        return MetaDataRegisterDTO.builder()
                .contextPath(contextPath)
                .appName(appName)
                .path(contextPath)
                .rpcType(RpcTypeEnum.HTTP.getName())
                .enabled(true)
                .ruleName(contextPath)
                .build();
    }
}
