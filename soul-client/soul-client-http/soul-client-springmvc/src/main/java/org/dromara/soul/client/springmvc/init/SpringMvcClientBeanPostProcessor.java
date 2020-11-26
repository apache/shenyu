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
import java.lang.reflect.Method;
import java.util.Objects;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.springmvc.annotation.SoulSpringMvcClient;
import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.dromara.soul.client.springmvc.dto.SpringMvcRegisterDTO;
import org.dromara.soul.client.springmvc.utils.IpUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Controller;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * The type Soul spring mvc client bean post processor.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class SpringMvcClientBeanPostProcessor implements BeanPostProcessor {

    private final ThreadPoolExecutor executorService;

    private final String url;

    private final SoulSpringMvcConfig soulSpringMvcConfig;

    /**
     * Instantiates a new Soul client bean post processor.
     *
     * @param soulSpringMvcConfig the soul spring mvc config
     */
    public SpringMvcClientBeanPostProcessor(final SoulSpringMvcConfig soulSpringMvcConfig) {
        String contextPath = soulSpringMvcConfig.getContextPath();
        String adminUrl = soulSpringMvcConfig.getAdminUrl();
        Integer port = soulSpringMvcConfig.getPort();
        if (contextPath == null || "".equals(contextPath)
                || adminUrl == null || "".equals(adminUrl)
                || port == null) {
            log.error("spring mvc param must config contextPath, adminUrl and port");
            throw new RuntimeException("spring mvc param must config contextPath, adminUrl and port");
        }
        this.soulSpringMvcConfig = soulSpringMvcConfig;
        url = adminUrl + "/soul-client/springmvc-register";
        executorService = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
    }

    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        if (soulSpringMvcConfig.isFull()) {
            return bean;
        }
        Controller controller = AnnotationUtils.findAnnotation(bean.getClass(), Controller.class);
        RestController restController = AnnotationUtils.findAnnotation(bean.getClass(), RestController.class);
        RequestMapping requestMapping = AnnotationUtils.findAnnotation(bean.getClass(), RequestMapping.class);
        if (controller != null || restController != null || requestMapping != null) {
            String contextPath = soulSpringMvcConfig.getContextPath();
            SoulSpringMvcClient clazzAnnotation = AnnotationUtils.findAnnotation(bean.getClass(), SoulSpringMvcClient.class);
            String prePath = "";
            if (Objects.nonNull(clazzAnnotation)) {
                if (clazzAnnotation.path().indexOf("*") > 1) {
                    String finalPrePath = prePath;
                    executorService.execute(() -> post(buildJsonParams(clazzAnnotation, contextPath, finalPrePath)));
                    return bean;
                }
                prePath = clazzAnnotation.path();
            }
            final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(bean.getClass());
            for (Method method : methods) {
                SoulSpringMvcClient soulSpringMvcClient = AnnotationUtils.findAnnotation(method, SoulSpringMvcClient.class);
                if (Objects.nonNull(soulSpringMvcClient)) {
                    String finalPrePath = prePath;
                    executorService.execute(() -> post(buildJsonParams(soulSpringMvcClient, contextPath, finalPrePath)));
                }
            }
        }
        return bean;
    }

    private void post(final String json) {
        try {
            String result = OkHttpTools.getInstance().post(url, json);
            if (Objects.equals(result, "success")) {
                log.info("http client register success :{} ", json);
            } else {
                log.error("http client register error :{} ", json);
            }
        } catch (IOException e) {
            log.error("cannot register soul admin param :{}", url + ":" + json);
        }
    }

    private String buildJsonParams(final SoulSpringMvcClient soulSpringMvcClient, final String contextPath, final String prePath) {
        String appName = soulSpringMvcConfig.getAppName();
        Integer port = soulSpringMvcConfig.getPort();
        String path = contextPath + prePath + soulSpringMvcClient.path();
        String desc = soulSpringMvcClient.desc();
        String configHost = soulSpringMvcConfig.getHost();
        String host = ("".equals(configHost) || null == configHost) ? IpUtils.getHost() : configHost;
        String configRuleName = soulSpringMvcClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        SpringMvcRegisterDTO registerDTO = SpringMvcRegisterDTO.builder()
                .context(contextPath)
                .host(host)
                .port(port)
                .appName(appName)
                .path(path)
                .pathDesc(desc)
                .rpcType(soulSpringMvcClient.rpcType())
                .enabled(soulSpringMvcClient.enabled())
                .ruleName(ruleName)
                .registerMetaData(soulSpringMvcClient.registerMetaData())
                .build();
        return OkHttpTools.getInstance().getGosn().toJson(registerDTO);
    }
}


