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

package org.dromara.soul.client.http.spring;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.common.annotation.SoulHttpClient;
import org.dromara.soul.client.common.dto.HttpRegisterDTO;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.http.spring.config.SoulHttpConfig;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Controller;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * The type Soul client bean post processor.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class SpringHttpClientBeanPostProcessor implements BeanPostProcessor {
    
    private ExecutorService executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
    
    private final String url;
    
    private final SoulHttpConfig soulHttpConfig;
    
    /**
     * Instantiates a new Soul client bean post processor.
     *
     * @param soulHttpConfig the soul http config
     */
    public SpringHttpClientBeanPostProcessor(final SoulHttpConfig soulHttpConfig) {
        String contextPath = soulHttpConfig.getContextPath();
        String adminUrl = soulHttpConfig.getAdminUrl();
        if (contextPath == null || "".equals(contextPath)
                || adminUrl == null || "".equals(adminUrl)) {
            log.error("http param must config  contextPath ,adminUrl");
            throw new RuntimeException("ttp param must config  contextPath ,adminUrl");
        }
        this.soulHttpConfig = soulHttpConfig;
        url = adminUrl + "/soul-client/http-register";
    }
    
    @Override
    public Object postProcessBeforeInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        Controller controller = AnnotationUtils.findAnnotation(bean.getClass(), Controller.class);
        RestController restController = AnnotationUtils.findAnnotation(bean.getClass(), RestController.class);
        RequestMapping requestMapping = AnnotationUtils.findAnnotation(bean.getClass(), RequestMapping.class);
        if (controller != null || restController != null || requestMapping != null) {
            String contextPath = soulHttpConfig.getContextPath();
            //首先
            SoulHttpClient clazzAnnotation = AnnotationUtils.findAnnotation(bean.getClass(), SoulHttpClient.class);
            if (Objects.nonNull(clazzAnnotation)) {
                contextPath += clazzAnnotation.path();
                if (clazzAnnotation.path().indexOf("*") > 1) {
                    String finalContextPath1 = contextPath;
                    executorService.execute(() -> post(buildJsonParams(clazzAnnotation, finalContextPath1, bean, "")));
                    return bean;
                }
            }
            final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(bean.getClass());
            for (Method method : methods) {
                SoulHttpClient soulHttpClient = AnnotationUtils.findAnnotation(method, SoulHttpClient.class);
                if (Objects.nonNull(soulHttpClient)) {
                    String finalContextPath = contextPath;
                    executorService.execute(() -> post(buildJsonParams(soulHttpClient, finalContextPath, bean, method.getName())));
                }
            }
        }
        return bean;
    }
    
    private void post(final String json) {
        try {
            String result = OkHttpTools.getInstance().post(url, json);
            if (Objects.equals(result, "success")) {
                log.info("http client register success :{} " + json);
            } else {
                log.error("http client register error :{} " + json);
            }
        } catch (IOException e) {
            log.error("cannot register soul admin param :{}", url + ":" + json);
        }
    }
    
    private String buildJsonParams(final SoulHttpClient soulHttpClient, final String contextPath, final Object bean, final String methodName) {
        String appName = soulHttpConfig.getAppName();
        Integer port = soulHttpConfig.getPort();
        String path = contextPath + soulHttpClient.path();
        String desc = soulHttpClient.desc();
        String serviceName = bean.getClass().getSimpleName();
        HttpRegisterDTO registerDTO = HttpRegisterDTO.builder()
                .context(contextPath)
                .host(getHost())
                .port(port)
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .path(path)
                .pathDesc(desc)
                .rpcType(soulHttpClient.rpcType())
                .writeMetaData(soulHttpClient.writeMetaData())
                .enabled(soulHttpClient.enabled())
                .build();
        return OkHttpTools.getInstance().getGosn().toJson(registerDTO);
        
    }
    
    private String getHost() {
        try {
            return InetAddress.getLocalHost().getHostAddress();
        } catch (UnknownHostException e) {
            return "127.0.0.1";
        }
    }
    
}


