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
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.PathUtils;
import org.apache.shenyu.common.utils.PortUtils;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.lang.NonNull;

import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Context register listener.
 */
public class ContextRegisterListener implements ApplicationListener<ContextRefreshedEvent>, BeanFactoryAware {

    private static final Logger LOG = LoggerFactory.getLogger(ContextRegisterListener.class);

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final AtomicBoolean registered = new AtomicBoolean(false);

    private final String contextPath;

    private final String appName;

    private final String protocol;

    private final String host;

    private final Integer port;

    private final Boolean isFull;

    private BeanFactory beanFactory;

    /**
     * Instantiates a new Context register listener.
     *
     * @param clientConfig the client config
     */
    public ContextRegisterListener(final PropertiesConfig clientConfig) {
        final Properties props = clientConfig.getProps();
        this.isFull = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString()));
        this.contextPath = Optional.ofNullable(props.getProperty(ShenyuClientConstants.CONTEXT_PATH)).map(UriUtils::repairData).orElse(null);
        if (Boolean.TRUE.equals(isFull)) {
            if (StringUtils.isBlank(contextPath)) {
                final String errorMsg = "http register param must config the contextPath";
                LOG.error(errorMsg);
                throw new ShenyuClientIllegalArgumentException(errorMsg);
            }
        }
        this.port = Integer.parseInt(Optional.ofNullable(props.getProperty(ShenyuClientConstants.PORT)).orElseGet(() -> "-1"));
        this.appName = props.getProperty(ShenyuClientConstants.APP_NAME);
        this.protocol = props.getProperty(ShenyuClientConstants.PROTOCOL, ShenyuClientConstants.HTTP);
        this.host = props.getProperty(ShenyuClientConstants.HOST);
    }

    @Override
    public void setBeanFactory(final BeanFactory beanFactory) throws BeansException {
        this.beanFactory = beanFactory;
    }

    @Override
    public void onApplicationEvent(@NonNull final ContextRefreshedEvent contextRefreshedEvent) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        if (Boolean.TRUE.equals(isFull)) {
            publisher.publishEvent(buildMetaDataDTO());
        }
        try {
            final int mergedPort = port <= 0 ? PortUtils.findPort(beanFactory) : port;
            publisher.publishEvent(buildURIRegisterDTO(mergedPort));
        } catch (ShenyuException e) {
            throw new ShenyuException(e.getMessage() + "please config ${shenyu.client.http.props.port} in xml/yml !");
        }
    }

    private URIRegisterDTO buildURIRegisterDTO(final int port) {
        return URIRegisterDTO.builder()
            .contextPath(this.contextPath)
            .appName(appName)
            .protocol(protocol)
            .host(IpUtils.isCompleteHost(this.host) ? this.host : IpUtils.getHost(this.host))
            .port(port)
            .rpcType(RpcTypeEnum.HTTP.getName())
            .build();
    }

    private MetaDataRegisterDTO buildMetaDataDTO() {
        return MetaDataRegisterDTO.builder()
            .contextPath(contextPath)
            .appName(appName)
            .path(PathUtils.decoratorPathWithSlash(contextPath))
            .rpcType(RpcTypeEnum.HTTP.getName())
            .enabled(true)
            .ruleName(contextPath)
            .build();
    }
}
