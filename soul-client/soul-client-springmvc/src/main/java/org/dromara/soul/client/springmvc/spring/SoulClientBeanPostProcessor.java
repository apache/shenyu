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

package org.dromara.soul.client.springmvc.spring;

import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.common.annotation.SoulClient;
import org.dromara.soul.client.common.dto.MetaDataDTO;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.springmvc.config.SoulHttpConfig;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.env.Environment;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Controller;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;

/**
 * The type Soul client bean post processor.
 *
 * @author xiaoyu(Myth)
 */
@Slf4j
public class SoulClientBeanPostProcessor implements BeanPostProcessor {

    private ExecutorService executorService = Executors.newSingleThreadExecutor();

    private final Environment env;

    private SoulHttpConfig soulHttpConfig;

    private final String url;

    /**
     * Instantiates a new Soul client bean post processor.
     *
     * @param env            the env
     * @param soulHttpConfig the soul http config
     */
    public SoulClientBeanPostProcessor(final Environment env, final SoulHttpConfig soulHttpConfig) {
        this.env = env;
        this.soulHttpConfig = soulHttpConfig;
        url = soulHttpConfig.getAdminUrl() + "/meta-data/register";
    }

    @Override
    public Object postProcessBeforeInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        return bean;
    }

    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        Controller controller = AnnotationUtils.findAnnotation(bean.getClass(), Controller.class);
        RestController restController = AnnotationUtils.findAnnotation(bean.getClass(), RestController.class);
        RequestMapping requestMapping = AnnotationUtils.findAnnotation(bean.getClass(), RequestMapping.class);
        if (controller != null || restController != null || requestMapping != null) {
            String contextPath = soulHttpConfig.getContextPath();
            String adminUrl = soulHttpConfig.getAdminUrl();
            if (contextPath == null || "".equals(contextPath)
                    || adminUrl == null || "".equals(adminUrl)) {
                log.error("springMvc client must config context-path error");
                return bean;
            }
            final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(bean.getClass());
            for (Method method : methods) {
                SoulClient soulClient = AnnotationUtils.findAnnotation(method, SoulClient.class);
                if (Objects.nonNull(soulClient)) {
                    executorService.execute(() -> post(buildJsonParams(soulClient, contextPath, bean, method)));
                }
            }
        }
        return bean;
    }

    private void post(final String json) {
        try {
            String result = OkHttpTools.getInstance().post(url, json);
            if (Objects.equals(result, "success")) {
                log.info("springMvc client register success :{} " + json);
            } else {
                log.error("springMvc client register error :{} " + json);
            }
        } catch (IOException e) {
            log.error("cannot register soul admin param :{}", url + ":" + json);
        }
    }

    private String buildJsonParams(final SoulClient soulClient, final String contextPath, final Object bean, final Method method) {
        String appName = soulHttpConfig.getAppName();
        if (appName == null || "".equals(appName)) {
            appName = env.getProperty("spring.application.name");
        }
        String path = contextPath + soulClient.path();
        String desc = soulClient.desc();
        String serviceName = bean.getClass().getSimpleName();
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName).collect(Collectors.joining(","));
        MetaDataDTO metaDataDTO = MetaDataDTO.builder()
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .path(path)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcExt("")
                .rpcType("http")
                .enabled(soulClient.enabled())
                .build();
        return OkHttpTools.getInstance().getGosn().toJson(metaDataDTO);

    }

}


