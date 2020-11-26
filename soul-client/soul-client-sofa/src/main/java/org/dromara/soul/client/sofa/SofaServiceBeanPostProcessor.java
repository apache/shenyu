/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.client.sofa;

import com.alipay.sofa.runtime.service.component.Service;
import com.alipay.sofa.runtime.spring.factory.ServiceFactoryBean;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.sofa.common.annotation.SoulSofaClient;
import org.dromara.soul.client.sofa.common.config.SofaConfig;
import org.dromara.soul.client.sofa.common.dto.MetaDataDTO;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * The Sofa ServiceBean PostProcessor.
 *
 * @author tydhot
 */
@Slf4j
public class SofaServiceBeanPostProcessor implements BeanPostProcessor {

    private SofaConfig sofaConfig;

    private ExecutorService executorService;

    private final String url;

    public SofaServiceBeanPostProcessor(final SofaConfig sofaConfig) {
        String contextPath = sofaConfig.getContextPath();
        String adminUrl = sofaConfig.getAdminUrl();
        if (contextPath == null || "".equals(contextPath)
                || adminUrl == null || "".equals(adminUrl)) {
            throw new RuntimeException("sofa client must config the contextPath, adminUrl");
        }
        this.sofaConfig = sofaConfig;
        url = sofaConfig.getAdminUrl() + "/soul-client/sofa-register";
        executorService = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
    }

    @Override
    public Object postProcessAfterInitialization(final Object bean, final String beanName) throws BeansException {
        if (bean instanceof ServiceFactoryBean) {
            executorService.execute(() -> handler((ServiceFactoryBean) bean));
        }
        return bean;
    }

    private void handler(final ServiceFactoryBean serviceBean) {
        Class<?> clazz = null;
        try {
            clazz = ((Service) serviceBean.getObject()).getTarget().getClass();
        } catch (Exception e) {
            log.error("failed to get sofa target class");
            return;
        }
        if (ClassUtils.isCglibProxyClass(clazz)) {
            String superClassName = clazz.getGenericSuperclass().getTypeName();
            try {
                clazz = Class.forName(superClassName);
            } catch (ClassNotFoundException e) {
                log.error(String.format("class not found: %s", superClassName));
                return;
            }
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            SoulSofaClient soulDubboClient = method.getAnnotation(SoulSofaClient.class);
            if (Objects.nonNull(soulDubboClient)) {
                post(buildJsonParams(serviceBean, soulDubboClient, method));
            }
        }
    }

    private String buildJsonParams(final ServiceFactoryBean serviceBean, final SoulSofaClient soulSofaClient, final Method method) {
        String appName = sofaConfig.getAppName();
        String path = sofaConfig.getContextPath() + soulSofaClient.path();
        String desc = soulSofaClient.desc();
        String serviceName = serviceBean.getInterfaceClass().getName();
        String configRuleName = soulSofaClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        MetaDataDTO metaDataDTO = MetaDataDTO.builder()
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(sofaConfig.getContextPath())
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcType("sofa")
                .enabled(soulSofaClient.enabled())
                .build();
        return OkHttpTools.getInstance().getGosn().toJson(metaDataDTO);

    }

    private void post(final String json) {
        try {
            String result = OkHttpTools.getInstance().post(url, json);
            if (Objects.equals(result, "success")) {
                log.info("sofa client register success :{} ", json);
            } else {
                log.error("sofa client register error :{} ", json);
            }
        } catch (IOException e) {
            log.error("cannot register soul admin param :{}", url + ":" + json);
        }
    }
}
