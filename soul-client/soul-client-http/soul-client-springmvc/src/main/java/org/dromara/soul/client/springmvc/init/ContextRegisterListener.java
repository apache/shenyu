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

package org.dromara.soul.client.springmvc.init;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.client.core.disruptor.SoulClientRegisterEventPublisher;
import org.dromara.soul.client.core.register.SoulClientRegisterRepositoryFactory;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.IpUtils;
import org.dromara.soul.register.client.api.SoulClientRegisterRepository;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.dromara.soul.register.common.dto.MetaDataDTO;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.lang.NonNull;

/**
 * The type Context register listener.
 */
@Slf4j
public class ContextRegisterListener implements ApplicationListener<ContextRefreshedEvent> {
    
    private SoulClientRegisterEventPublisher publisher = SoulClientRegisterEventPublisher.getInstance();

    private final AtomicBoolean registered = new AtomicBoolean(false);
    
    private final String contextPath;
    
    private final String appName;
    
    private final String host;
    
    private final Integer port;
    
    private final Boolean isFull;

    /**
     * Instantiates a new Context register listener.
     *
     */
    public ContextRegisterListener(final SoulRegisterCenterConfig config) {
        String registerType = config.getRegisterType();
        String serverLists = config.getServerLists();
        Properties props = config.getProps();
        String contextPath = props.getProperty("contextPath");
        int port = Integer.parseInt(props.getProperty("port"));
        if (StringUtils.isBlank(contextPath) || StringUtils.isBlank(registerType)
                || StringUtils.isBlank(serverLists) || port <= 0) {
            String errorMsg = "spring cloud param must config the contextPath ,registerType , serverLists and port must > 0";
            log.error(errorMsg);
            throw new RuntimeException(errorMsg);
        }
        this.appName = props.getProperty("appName");
        this.host = props.getProperty("host");
        this.port = port;
        this.contextPath = contextPath;
        this.isFull = Boolean.parseBoolean(props.getProperty("isFull", "false"));
        SoulClientRegisterRepository soulClientRegisterRepository = SoulClientRegisterRepositoryFactory.newInstance(config);
        publisher.start(soulClientRegisterRepository);
    }

    @Override
    public void onApplicationEvent(@NonNull final ContextRefreshedEvent contextRefreshedEvent) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        if (isFull) {
            publisher.publishEvent(buildMetaDataDTO());
        }
    }

    private MetaDataDTO buildMetaDataDTO() {
        String contextPath = this.contextPath;
        String appName = this.appName;
        Integer port = this.port;
        String path = contextPath + "/**";
        String configHost = this.host;
        String host = StringUtils.isBlank(configHost) ? IpUtils.getHost() : configHost;
        return MetaDataDTO.builder()
                .contextPath(contextPath)
                .host(host)
                .port(port)
                .appName(appName)
                .path(path)
                .rpcType(RpcTypeEnum.HTTP.getName())
                .enabled(true)
                .ruleName(path)
                .build();
    }
}
