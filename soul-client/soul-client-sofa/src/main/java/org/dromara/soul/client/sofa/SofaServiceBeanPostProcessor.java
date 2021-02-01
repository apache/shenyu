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
import org.dromara.soul.client.common.utils.RegisterUtils;
import org.dromara.soul.client.sofa.common.annotation.SoulSofaClient;
import org.dromara.soul.client.sofa.common.config.SofaConfig;
import org.dromara.soul.client.sofa.common.dto.MetaDataDTO;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * The Sofa ServiceBean PostProcessor.
 *
 * @author tydhot
 */
@Slf4j
public class SofaServiceBeanPostProcessor implements BeanPostProcessor {

    private final SofaConfig sofaConfig;

    private final ExecutorService executorService;

    private final String url;

    private final Pattern pattern = Pattern.compile("(?<=<).*?(?=>)");

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
        Class<?> clazz;
        try {
            clazz = ((Service) Objects.requireNonNull(serviceBean.getObject())).getTarget().getClass();
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
            SoulSofaClient soulSofaClient = method.getAnnotation(SoulSofaClient.class);
            if (Objects.nonNull(soulSofaClient)) {
                RegisterUtils.doRegister(buildJsonParams(serviceBean, soulSofaClient, method), url, RpcTypeEnum.SOFA);
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
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName).collect(Collectors.joining(","));
        MetaDataDTO metaDataDTO = MetaDataDTO.builder()
                .appName(appName)
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(sofaConfig.getContextPath())
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes + getGenericParameterTypes(method))
                .rpcType("sofa")
                .rpcExt(buildRpcExt(soulSofaClient))
                .enabled(soulSofaClient.enabled())
                .build();
        return OkHttpTools.getInstance().getGson().toJson(metaDataDTO);
    }

    private String getGenericParameterTypes(final Method method) {
        Type[] types = method.getGenericParameterTypes();
        String genericParameterTypes = Arrays.stream(types).map(Type::getTypeName).collect(Collectors.joining(","));
        Matcher matcher = pattern.matcher(genericParameterTypes);
        List<String> list = new LinkedList<>();
        while (matcher.find()) {
            String type = matcher.group();
            if (!type.startsWith("java")) {
                list.add(type);
            }
        }
        if (list.isEmpty()) {
            return "";
        }
        return "#" + String.join(",", list);
    }

    private String buildRpcExt(final SoulSofaClient soulSofaClient) {
        MetaDataDTO.RpcExt build = MetaDataDTO.RpcExt.builder()
                .loadbalance(soulSofaClient.loadBalance())
                .retries(soulSofaClient.retries())
                .timeout(soulSofaClient.timeout())
                .build();
        return OkHttpTools.getInstance().getGson().toJson(build);
    }
}
