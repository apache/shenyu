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

package org.dromara.soul.client.springcloud.init;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.common.utils.RegisterUtils;
import org.dromara.soul.client.springcloud.config.SoulSpringCloudConfig;
import org.dromara.soul.client.springcloud.dto.SpringCloudRegisterDTO;
import org.dromara.soul.client.springcloud.utils.ValidateUtils;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.env.Environment;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Context register listener.
 * @author tnnn
 */
@Slf4j
public class ContextRegisterListener implements ApplicationListener<ContextRefreshedEvent> {

    private final AtomicBoolean registered = new AtomicBoolean(false);

    private final String url;

    private final SoulSpringCloudConfig config;

    private final Environment env;

    /**
     * Instantiates a new Context register listener.
     *
     * @param config the soul spring cloud config
     * @param env    the env
     */
    public ContextRegisterListener(final SoulSpringCloudConfig config, final Environment env) {
        ValidateUtils.validate(config, env);
        this.config = config;
        this.env = env;
        this.url = config.getAdminUrl() + "/soul-client/springcloud-register";
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        if (config.isFull()) {
            RegisterUtils.doRegister(buildJsonParams(), url, RpcTypeEnum.SPRING_CLOUD);
        }
    }

    private String buildJsonParams() {
        String contextPath = config.getContextPath();
        String appName = env.getProperty("spring.application.name");
        String path = contextPath + "/**";
        SpringCloudRegisterDTO registerDTO = SpringCloudRegisterDTO.builder()
                .context(contextPath)
                .appName(appName)
                .path(path)
                .rpcType(RpcTypeEnum.SPRING_CLOUD.getName())
                .enabled(true)
                .ruleName(path)
                .build();
        return OkHttpTools.getInstance().getGson().toJson(registerDTO);
    }
}
