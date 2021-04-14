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

package org.dromara.soul.client.sofa;

import com.alipay.sofa.runtime.service.component.Service;
import com.alipay.sofa.runtime.spring.factory.ServiceFactoryBean;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.client.core.disruptor.SoulClientRegisterEventPublisher;
import org.dromara.soul.client.sofa.common.annotation.SoulSofaClient;
import org.dromara.soul.client.sofa.common.dto.SofaRpcExt;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.common.utils.IpUtils;
import org.dromara.soul.register.client.api.SoulClientRegisterRepository;
import org.dromara.soul.register.common.config.SoulRegisterCenterConfig;
import org.dromara.soul.register.common.dto.MetaDataRegisterDTO;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Objects;
import java.util.Properties;
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
    
    private SoulClientRegisterEventPublisher publisher = SoulClientRegisterEventPublisher.getInstance();
    
    private final ExecutorService executorService;
    
    private final String contextPath;
    
    private final String appName;

    private final String host;

    private final String port;
    
    public SofaServiceBeanPostProcessor(final SoulRegisterCenterConfig config, final SoulClientRegisterRepository soulClientRegisterRepository) {
        Properties props = config.getProps();
        String contextPath = props.getProperty("contextPath");
        String appName = props.getProperty("appName");
        if (StringUtils.isEmpty(contextPath)) {
            throw new RuntimeException("sofa client must config the contextPath");
        }
        this.contextPath = contextPath;
        this.appName = appName;
        this.host = props.getProperty("host");
        this.port = props.getProperty("port");
        executorService = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
        publisher.start(soulClientRegisterRepository);
    }

    @Override
    public Object postProcessAfterInitialization(final Object bean, final String beanName) throws BeansException {
        if (bean instanceof ServiceFactoryBean) {
            executorService.execute(() -> handler((ServiceFactoryBean) bean));
        }
        return bean;
    }

    @SneakyThrows
    private void handler(final ServiceFactoryBean serviceBean) {
        Class<?> clazz;
        try {
            clazz = ((Service) Objects.requireNonNull(serviceBean.getObject())).getTarget().getClass();
        } catch (Exception e) {
            log.error("failed to get sofa target class");
            return;
        }
        if (AopUtils.isCglibProxy(clazz)) {
            String superClassName = clazz.getGenericSuperclass().getTypeName();
            try {
                clazz = Class.forName(superClassName);
            } catch (ClassNotFoundException e) {
                log.error(String.format("class not found: %s", superClassName));
                return;
            }
        }
        if (AopUtils.isJdkDynamicProxy(clazz)) {
            clazz = AopUtils.getTargetClass(serviceBean.getObject());
        }
        Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            SoulSofaClient soulSofaClient = method.getAnnotation(SoulSofaClient.class);
            if (Objects.nonNull(soulSofaClient)) {
                publisher.publishEvent(buildMetaDataDTO(serviceBean, soulSofaClient, method));
            }
        }
    }

    private MetaDataRegisterDTO buildMetaDataDTO(final ServiceFactoryBean serviceBean, final SoulSofaClient soulSofaClient, final Method method) {
        String appName = this.appName;
        String path = contextPath + soulSofaClient.path();
        String desc = soulSofaClient.desc();
        String serviceName = serviceBean.getInterfaceClass().getName();
        String host = StringUtils.isBlank(this.host) ? IpUtils.getHost() : this.host;
        int port = StringUtils.isBlank(this.port) ? -1 : Integer.parseInt(this.port);
        String configRuleName = soulSofaClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        return MetaDataRegisterDTO.builder()
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(contextPath)
                .host(host)
                .port(port)
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcType("sofa")
                .rpcExt(buildRpcExt(soulSofaClient))
                .enabled(soulSofaClient.enabled())
                .build();
    }

    private String buildRpcExt(final SoulSofaClient soulSofaClient) {
        SofaRpcExt build = SofaRpcExt.builder()
                .loadbalance(soulSofaClient.loadBalance())
                .retries(soulSofaClient.retries())
                .timeout(soulSofaClient.timeout())
                .build();
        return GsonUtils.getInstance().toJson(build);
    }
}
