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

package org.apache.shenyu.client.springcloud.init;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.disruptor.SoulClientRegisterEventPublisher;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.SoulClientRegisterRepository;
import org.apache.shenyu.register.common.config.SoulRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;
import org.springframework.lang.NonNull;

import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Context register listener.
 *
 * @author tnnn
 */
@Slf4j
public class ContextRegisterListener implements ApplicationListener<ContextRefreshedEvent> {
    
    private SoulClientRegisterEventPublisher publisher = SoulClientRegisterEventPublisher.getInstance();
    
    private final AtomicBoolean registered = new AtomicBoolean(false);
    
    private final Boolean isFull;
    
    private Environment env;
    
    private String contextPath;
    
    /**
     * Instantiates a new Context register listener.
     *
     * @param config the config
     * @param env    the env
     * @param soulClientRegisterRepository the soulClientRegisterRepository
     */
    public ContextRegisterListener(final SoulRegisterCenterConfig config, final Environment env, final SoulClientRegisterRepository soulClientRegisterRepository) {
        Properties props = config.getProps();
        this.isFull = Boolean.parseBoolean(props.getProperty("isFull", "false"));
        if (isFull) {
            String registerType = config.getRegisterType();
            String serverLists = config.getServerLists();
            String contextPath = props.getProperty("contextPath");
            String appName = env.getProperty("spring.application.name");
            if (StringUtils.isBlank(contextPath) || StringUtils.isBlank(registerType)
                    || StringUtils.isBlank(serverLists) || StringUtils.isBlank(appName)) {
                String errorMsg = "spring cloud param must config the contextPath ,registerType , serverLists  and appName";
                log.error(errorMsg);
                throw new RuntimeException(errorMsg);
            }
            this.env = env;
            this.contextPath = contextPath;
            publisher.start(soulClientRegisterRepository);
        }
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
    
    private MetaDataRegisterDTO buildMetaDataDTO() {
        String contextPath = this.contextPath;
        String appName = env.getProperty("spring.application.name");
        String path = contextPath + "/**";
        return MetaDataRegisterDTO.builder()
                .contextPath(contextPath)
                .appName(appName)
                .path(path)
                .rpcType(RpcTypeEnum.SPRING_CLOUD.getName())
                .enabled(true)
                .ruleName(path)
                .build();
    }
}
