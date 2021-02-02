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

package org.dromara.soul.client.grpc;

import io.grpc.BindableService;
import lombok.extern.slf4j.Slf4j;
import org.dromara.soul.client.common.utils.OkHttpTools;
import org.dromara.soul.client.common.utils.RegisterUtils;
import org.dromara.soul.client.grpc.common.annotation.SoulGrpcClient;
import org.dromara.soul.client.grpc.common.config.GrpcConfig;
import org.dromara.soul.client.grpc.common.dto.MetaDataDTO;
import org.dromara.soul.common.enums.RpcTypeEnum;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.lang.NonNull;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * The type Soul grpc client bean post processor.
 *
 * @author zhanglei
 */
@Slf4j
public class GrpcClientBeanPostProcessor implements BeanPostProcessor {

    private final ThreadPoolExecutor executorService;

    private final String url;

    private final GrpcConfig grpcConfig;
    
    /**
     * Instantiates a new Soul client bean post processor.
     *
     * @param config the soul grpc config
     */
    public GrpcClientBeanPostProcessor(final GrpcConfig config) {
        String contextPath = config.getContextPath();
        String adminUrl = config.getAdminUrl();
        Integer port = config.getPort();
        if (contextPath == null || "".equals(contextPath)
                || adminUrl == null || "".equals(adminUrl)
                || port == null) {
            log.error("grpc param must config contextPath, adminUrl and port");
            throw new RuntimeException("grpc param must config contextPath, adminUrl and port");
        }
        this.grpcConfig = config;
        url = adminUrl + "/soul-client/grpc-register";
        executorService = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
    }

    @Override
    public Object postProcessAfterInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        if (bean instanceof BindableService) {
            executorService.execute(() -> handler(bean));
        }
        return bean;
    }

    private void handler(final Object serviceBean) {
        Class<?> clazz;
        try {
            clazz = serviceBean.getClass();
        } catch (Exception e) {
            log.error("failed to get grpc target class");
            return;
        }
        Class<?> parent = clazz.getSuperclass();
        Class<?> classes = parent.getDeclaringClass();
        String packageName;
        try {
            String serviceName = "SERVICE_NAME";
            Field field = classes.getField(serviceName);
            field.setAccessible(true);
            packageName = field.get(null).toString();
        } catch (Exception e) {
            log.error(String.format("SERVICE_NAME field not found: %s", classes));
            return;
        }
        if (StringUtils.isEmpty(packageName)) {
            log.error(String.format("grpc SERVICE_NAME can not found: %s", classes));
            return;
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            SoulGrpcClient grpcClient = method.getAnnotation(SoulGrpcClient.class);
            if (Objects.nonNull(grpcClient)) {
                RegisterUtils.doRegister(buildJsonParams(packageName, grpcClient, method), url, RpcTypeEnum.GRPC);
            }
        }
    }

    private String buildJsonParams(final String packageName, final SoulGrpcClient soulGrpcClient, final Method method) {
        String path = grpcConfig.getContextPath() + soulGrpcClient.path();
        String desc = soulGrpcClient.desc();
        String configRuleName = soulGrpcClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        MetaDataDTO grpcMetaDataDTO = MetaDataDTO.builder()
                .appName(String.join(":", grpcConfig.getHost(), grpcConfig.getPort().toString()))
                .serviceName(packageName)
                .methodName(methodName)
                .contextPath(grpcConfig.getContextPath())
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcType("grpc")
                .rpcExt(buildRpcExt(soulGrpcClient))
                .enabled(soulGrpcClient.enabled())
                .build();
        return OkHttpTools.getInstance().getGson().toJson(grpcMetaDataDTO);
    }

    private String buildRpcExt(final SoulGrpcClient soulGrpcClient) {
        MetaDataDTO.RpcExt build = MetaDataDTO.RpcExt.builder()
                .timeout(soulGrpcClient.timeout())
                .build();
        return OkHttpTools.getInstance().getGson().toJson(build);
    }
}


