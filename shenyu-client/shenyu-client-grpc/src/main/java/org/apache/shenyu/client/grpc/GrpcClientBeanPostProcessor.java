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

package org.apache.shenyu.client.grpc;

import io.grpc.BindableService;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.client.core.disruptor.SoulClientRegisterEventPublisher;
import org.apache.shenyu.client.grpc.common.annotation.SoulGrpcClient;
import org.apache.shenyu.client.grpc.common.dto.GrpcExt;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.SoulClientRegisterRepository;
import org.apache.shenyu.register.common.config.SoulRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.lang.NonNull;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

import java.lang.reflect.Field;
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
 * The type Soul grpc client bean post processor.
 *
 * @author zhanglei
 */
@Slf4j
public class GrpcClientBeanPostProcessor implements BeanPostProcessor {
    
    private SoulClientRegisterEventPublisher publisher = SoulClientRegisterEventPublisher.getInstance();
    
    private final ExecutorService executorService;
    
    private final String contextPath;
    
    private final String ipAndPort;

    private final String host;

    private final int port;
    
    /**
     * Instantiates a new Soul client bean post processor.
     *
     * @param config the soul grpc config
     * @param soulClientRegisterRepository the soulClientRegisterRepository
     */
    public GrpcClientBeanPostProcessor(final SoulRegisterCenterConfig config, final SoulClientRegisterRepository soulClientRegisterRepository) {
        Properties props = config.getProps();
        String contextPath = props.getProperty("contextPath");
        String ipAndPort = props.getProperty("ipAndPort");
        String port = props.getProperty("port");
        if (StringUtils.isEmpty(contextPath) || StringUtils.isEmpty(ipAndPort) || StringUtils.isEmpty(port)) {
            throw new RuntimeException("tars client must config the contextPath, ipAndPort");
        }
        this.ipAndPort = ipAndPort;
        this.contextPath = contextPath;
        this.host = props.getProperty("host");
        this.port = Integer.parseInt(port);
        executorService = new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, new LinkedBlockingQueue<>());
        publisher.start(soulClientRegisterRepository);
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
                publisher.publishEvent(buildMetaDataDTO(packageName, grpcClient, method));
            }
        }
    }

    private MetaDataRegisterDTO buildMetaDataDTO(final String packageName, final SoulGrpcClient soulGrpcClient, final Method method) {
        String path = this.contextPath + soulGrpcClient.path();
        String desc = soulGrpcClient.desc();
        String configHost = this.host;
        String host = org.apache.commons.lang3.StringUtils.isBlank(configHost) ? IpUtils.getHost() : configHost;
        String configRuleName = soulGrpcClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        return MetaDataRegisterDTO.builder()
                .appName(ipAndPort)
                .serviceName(packageName)
                .methodName(methodName)
                .contextPath(contextPath)
                .host(host)
                .port(port)
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcType("grpc")
                .rpcExt(buildRpcExt(soulGrpcClient))
                .enabled(soulGrpcClient.enabled())
                .build();
    }

    private String buildRpcExt(final SoulGrpcClient soulGrpcClient) {
        GrpcExt build = GrpcExt.builder().timeout(soulGrpcClient.timeout()).build();
        return GsonUtils.getInstance().toJson(build);
    }
}


