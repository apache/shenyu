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

package org.dromara.soul.client.springmvc;

import java.io.IOException;
import java.lang.reflect.Method;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.springmvc.annotation.SoulSpringMvcClient;
import org.dromara.soul.client.common.dto.HttpRegisterDTO;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.springmvc.config.SoulSpringMvcConfig;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
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
public class SpringMvcClientBeanPostProcessor implements BeanPostProcessor, ApplicationListener<ContextRefreshedEvent> {
    
    private ExecutorService executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
    
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
            log.error("spring mvc param must config  contextPath ,adminUrl and port ");
            throw new RuntimeException("spring mvc param must config  contextPath ,adminUrl and port");
        }
        this.soulSpringMvcConfig = soulSpringMvcConfig;
        url = adminUrl + "/soul-client/http-register";
    }
    
    @Override
    public Object postProcessBeforeInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        Controller controller = AnnotationUtils.findAnnotation(bean.getClass(), Controller.class);
        RestController restController = AnnotationUtils.findAnnotation(bean.getClass(), RestController.class);
        RequestMapping requestMapping = AnnotationUtils.findAnnotation(bean.getClass(), RequestMapping.class);
        if (controller != null || restController != null || requestMapping != null) {
            String contextPath = soulSpringMvcConfig.getContextPath();
            //首先
            SoulSpringMvcClient clazzAnnotation = AnnotationUtils.findAnnotation(bean.getClass(), SoulSpringMvcClient.class);
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
                SoulSpringMvcClient soulSpringMvcClient = AnnotationUtils.findAnnotation(method, SoulSpringMvcClient.class);
                if (Objects.nonNull(soulSpringMvcClient)) {
                    String finalContextPath = contextPath;
                    executorService.execute(() -> post(buildJsonParams(soulSpringMvcClient, finalContextPath, bean, method.getName())));
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
    
    private String buildJsonParams(final SoulSpringMvcClient soulSpringMvcClient, final String contextPath, final Object bean, final String methodName) {
        String appName = soulSpringMvcConfig.getAppName();
        Integer port = soulSpringMvcConfig.getPort();
        String path = contextPath + soulSpringMvcClient.path();
        String desc = soulSpringMvcClient.desc();
        String serviceName = bean.getClass().getSimpleName();
        String configHost = soulSpringMvcConfig.getHost();
        String host = ("".equals(configHost) || null == configHost) ? getHost() : configHost;
        String configRuleName = soulSpringMvcClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        HttpRegisterDTO registerDTO = HttpRegisterDTO.builder()
                .context(contextPath)
                .host(host)
                .port(port)
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .path(path)
                .pathDesc(desc)
                .rpcType(soulSpringMvcClient.rpcType())
                .writeMetaData(soulSpringMvcClient.writeMetaData())
                .enabled(soulSpringMvcClient.enabled())
                .ruleName(ruleName)
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
    
    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        executorService.shutdown();
    }
}


