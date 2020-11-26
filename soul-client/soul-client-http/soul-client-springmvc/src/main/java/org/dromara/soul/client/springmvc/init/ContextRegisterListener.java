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

import java.io.IOException;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.dromara.soul.client.springmvc.dto.SpringMvcRegisterDTO;
import org.dromara.soul.client.springmvc.utils.IpUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;

/**
 * The type Context register listener.
 */
@Slf4j
public class ContextRegisterListener implements ApplicationListener<ContextRefreshedEvent> {

    private volatile AtomicBoolean registered = new AtomicBoolean(false);

    private final String url;

    private final SoulSpringMvcConfig soulSpringMvcConfig;

    /**
     * Instantiates a new Context register listener.
     *
     * @param soulSpringMvcConfig the soul spring mvc config
     */
    public ContextRegisterListener(final SoulSpringMvcConfig soulSpringMvcConfig) {
        String contextPath = soulSpringMvcConfig.getContextPath();
        String adminUrl = soulSpringMvcConfig.getAdminUrl();
        Integer port = soulSpringMvcConfig.getPort();
        if (contextPath == null || "".equals(contextPath)
                || adminUrl == null || "".equals(adminUrl)
                || port == null) {
            log.error("spring mvc param must config contextPath adminUrl and port");
            throw new RuntimeException("spring mvc param must config contextPath, adminUrl and port");
        }
        this.soulSpringMvcConfig = soulSpringMvcConfig;
        url = adminUrl + "/soul-client/springmvc-register";
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        if (soulSpringMvcConfig.isFull()) {
            post(buildJsonParams(soulSpringMvcConfig.getContextPath()));
        }
    }

    private void post(final String json) {
        try {
            String result = OkHttpTools.getInstance().post(url, json);
            if (Objects.equals(result, "success")) {
                log.info("http context register success :{} ", json);
            } else {
                log.error("http context register error :{} ", json);
            }
        } catch (IOException e) {
            log.error("cannot register soul admin param :{}", url + ":" + json);
        }
    }

    private String buildJsonParams(final String contextPath) {
        String appName = soulSpringMvcConfig.getAppName();
        Integer port = soulSpringMvcConfig.getPort();
        String path = contextPath + "/**";
        String configHost = soulSpringMvcConfig.getHost();
        String host = ("".equals(configHost) || null == configHost) ? IpUtils.getHost() : configHost;
        SpringMvcRegisterDTO registerDTO = SpringMvcRegisterDTO.builder()
                .context(contextPath)
                .host(host)
                .port(port)
                .appName(appName)
                .path(path)
                .rpcType("http")
                .enabled(true)
                .ruleName(path)
                .build();
        return OkHttpTools.getInstance().getGosn().toJson(registerDTO);
    }
}
