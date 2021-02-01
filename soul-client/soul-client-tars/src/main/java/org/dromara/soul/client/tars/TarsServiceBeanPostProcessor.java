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

package org.dromara.soul.client.tars;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.tuple.Pair;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.common.utils.RegisterUtils;
import org.dromara.soul.client.tars.common.annotation.SoulTarsClient;
import org.dromara.soul.client.tars.common.annotation.SoulTarsService;
import org.dromara.soul.client.tars.common.config.TarsConfig;
import org.dromara.soul.client.tars.common.dto.MetaDataDTO;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.LocalVariableTableParameterNameDiscoverer;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * The Tars ServiceBean PostProcessor.
 *
 * @author tydhot
 */
@Slf4j
public class TarsServiceBeanPostProcessor implements BeanPostProcessor {
    private final TarsConfig tarsConfig;

    private final ExecutorService executorService;

    private final String url;

    private final LocalVariableTableParameterNameDiscoverer localVariableTableParameterNameDiscoverer = new LocalVariableTableParameterNameDiscoverer();

    public TarsServiceBeanPostProcessor(final TarsConfig tarsConfig) {
        String contextPath = tarsConfig.getContextPath();
        String adminUrl = tarsConfig.getAdminUrl();
        String ipAndPort = tarsConfig.getIpAndPort();
        if (contextPath == null || "".equals(contextPath)
                || adminUrl == null || "".equals(adminUrl) || "".equals(ipAndPort)) {
            throw new RuntimeException("tars client must config the contextPath, adminUrl");
        }
        this.tarsConfig = tarsConfig;
        url = tarsConfig.getAdminUrl() + "/soul-client/tars-register";
        executorService = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
    }

    @Override
    public Object postProcessAfterInitialization(final Object bean, final String beanName) throws BeansException {
        if (bean.getClass().getAnnotation(SoulTarsService.class) != null) {
            executorService.execute(() -> handler(bean));
        }
        return bean;
    }

    private void handler(final Object serviceBean) {
        Class<?> clazz = serviceBean.getClass();
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
        String serviceName = serviceBean.getClass().getAnnotation(SoulTarsService.class).serviceName();
        for (Method method : methods) {
            SoulTarsClient soulSofaClient = method.getAnnotation(SoulTarsClient.class);
            if (Objects.nonNull(soulSofaClient)) {
                RegisterUtils.doRegister(buildJsonParams(serviceName, soulSofaClient, method, buildRpcExt(methods)), url, RpcTypeEnum.TARS);
            }
        }
    }

    private String buildJsonParams(final String serviceName, final SoulTarsClient soulTarsClient, final Method method, final String rpcExt) {
        String ipAndPort = tarsConfig.getIpAndPort();
        String path = tarsConfig.getContextPath() + soulTarsClient.path();
        String desc = soulTarsClient.desc();
        String configRuleName = soulTarsClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        MetaDataDTO metaDataDTO = MetaDataDTO.builder()
                .appName(ipAndPort)
                .serviceName(serviceName)
                .methodName(methodName)
                .contextPath(tarsConfig.getContextPath())
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcType("tars")
                .rpcExt(rpcExt)
                .enabled(soulTarsClient.enabled())
                .build();
        return OkHttpTools.getInstance().getGson().toJson(metaDataDTO);
    }

    private MetaDataDTO.RpcExt buildRpcExt(final Method method) {
        String[] paramNames = localVariableTableParameterNameDiscoverer.getParameterNames(method);
        List<Pair<String, String>> params = new ArrayList<>();
        if (paramNames != null && paramNames.length > 0) {
            Class<?>[] paramTypes = method.getParameterTypes();
            for (int i = 0; i < paramNames.length; i++) {
                params.add(Pair.of(paramTypes[i].getName(), paramNames[i]));
            }
        }
        return MetaDataDTO.RpcExt.builder().methodName(method.getName())
                .params(params)
                .returnType(method.getReturnType().getName())
                .build();
    }

    private String buildRpcExt(final Method[] methods) {
        List<MetaDataDTO.RpcExt> list = new ArrayList<>();
        for (Method method : methods) {
            SoulTarsClient soulSofaClient = method.getAnnotation(SoulTarsClient.class);
            if (Objects.nonNull(soulSofaClient)) {
                list.add(buildRpcExt(method));
            }
        }
        MetaDataDTO.RpcExtList buildList = MetaDataDTO.RpcExtList.builder()
                .methodInfo(list)
                .build();
        return OkHttpTools.getInstance().getGson().toJson(buildList);
    }
}
