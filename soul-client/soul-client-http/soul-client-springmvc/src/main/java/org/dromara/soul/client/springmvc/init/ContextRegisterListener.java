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

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.common.utils.RegisterUtils;
import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.dromara.soul.client.springmvc.dto.SpringMvcRegisterDTO;
import org.dromara.soul.client.springmvc.utils.ValidateUtils;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.dromara.soul.common.utils.IpUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

import java.util.concurrent.atomic.AtomicBoolean;

/**
 * The type Context register listener.
 */
@Slf4j
public class ContextRegisterListener implements ApplicationListener<ContextRefreshedEvent> {

    private final AtomicBoolean registered = new AtomicBoolean(false);

    private final String url;

    private final SoulSpringMvcConfig soulSpringMvcConfig;

    /**
     * Instantiates a new Context register listener.
     *
     * @param soulSpringMvcConfig the soul spring mvc config
     */
    public ContextRegisterListener(final SoulSpringMvcConfig soulSpringMvcConfig) {
        ValidateUtils.validate(soulSpringMvcConfig);
        this.soulSpringMvcConfig = soulSpringMvcConfig;
        url = soulSpringMvcConfig.getAdminUrl() + "/soul-client/springmvc-register";
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        if (soulSpringMvcConfig.isFull()) {
            RegisterUtils.doRegister(buildJsonParams(), url, RpcTypeEnum.HTTP);
        }
    }

    private String buildJsonParams() {
        String contextPath = soulSpringMvcConfig.getContextPath();
        String appName = soulSpringMvcConfig.getAppName();
        Integer port = soulSpringMvcConfig.getPort();
        String path = contextPath + "/**";
        String configHost = soulSpringMvcConfig.getHost();
        String host = StringUtils.isBlank(configHost) ? IpUtils.getHost() : configHost;
        SpringMvcRegisterDTO registerDTO = SpringMvcRegisterDTO.builder()
                .context(contextPath)
                .host(host)
                .port(port)
                .appName(appName)
                .path(path)
                .rpcType(RpcTypeEnum.HTTP.getName())
                .enabled(true)
                .ruleName(path)
                .build();
        return OkHttpTools.getInstance().getGson().toJson(registerDTO);
    }
}
