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

import com.google.common.collect.Lists;
import io.grpc.BindableService;
import io.grpc.MethodDescriptor;
import io.grpc.ServerServiceDefinition;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.grpc.common.annotation.ShenyuGrpcClient;
import org.apache.shenyu.client.grpc.common.dto.GrpcExt;
import org.apache.shenyu.client.grpc.json.JsonServerServiceInterceptor;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * The type Shenyu grpc client event listener.
 */
public class GrpcClientEventListener implements ApplicationListener<ContextRefreshedEvent> {

    private static final Logger LOG = LoggerFactory.getLogger(GrpcClientEventListener.class);

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final String contextPath;

    private final String ipAndPort;

    private final String host;

    private final int port;

    private final List<ServerServiceDefinition> serviceDefinitions = Lists.newArrayList();

    /**
     * Instantiates a new Shenyu client bean post processor.
     *
     * @param clientConfig the shenyu grpc client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     */
    public GrpcClientEventListener(final PropertiesConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        Properties props = clientConfig.getProps();
        String contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
        String ipAndPort = props.getProperty(ShenyuClientConstants.IP_PORT);
        String port = props.getProperty(ShenyuClientConstants.PORT);
        if (StringUtils.isAnyBlank(contextPath, ipAndPort, port)) {
            throw new ShenyuClientIllegalArgumentException("grpc client must config the contextPath, ipAndPort");
        }
        this.ipAndPort = ipAndPort;
        this.contextPath = contextPath;
        this.host = props.getProperty(ShenyuClientConstants.HOST);
        this.port = Integer.parseInt(port);
        publisher.start(shenyuClientRegisterRepository);
    }

    @Override
    public void onApplicationEvent(@NonNull final ContextRefreshedEvent contextRefreshedEvent) {
        Map<String, BindableService> beans = contextRefreshedEvent.getApplicationContext().getBeansOfType(BindableService.class);
        for (Map.Entry<String, BindableService> entry : beans.entrySet()) {
            exportJsonGenericService(entry.getValue());
            handler(entry.getValue());
        }
    }

    private void handler(final Object serviceBean) {
        Class<?> clazz;
        try {
            clazz = serviceBean.getClass();
        } catch (Exception e) {
            LOG.error("failed to get grpc target class", e);
            return;
        }
        if (AopUtils.isAopProxy(serviceBean)) {
            clazz = AopUtils.getTargetClass(serviceBean);
        }
        Class<?> parent = clazz.getSuperclass();
        Class<?> classes = parent.getDeclaringClass();
        String packageName;
        try {
            String serviceName = ShenyuClientConstants.SERVICE_NAME;
            Field field = classes.getField(serviceName);
            field.setAccessible(true);
            packageName = field.get(null).toString();
        } catch (Exception e) {
            LOG.error(String.format("SERVICE_NAME field not found: %s", classes), e);
            return;
        }
        if (StringUtils.isEmpty(packageName)) {
            LOG.error(String.format("grpc SERVICE_NAME can not found: %s", classes));
            return;
        }
        ShenyuGrpcClient beanShenyuClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuGrpcClient.class);
        String basePath = Optional.ofNullable(beanShenyuClient)
                .map(annotation -> StringUtils.defaultIfBlank(beanShenyuClient.path(), "")).orElse("");
        if (basePath.contains("*")) {
            Method[] methods = ReflectionUtils.getDeclaredMethods(clazz);
            for (Method method : methods) {
                publisher.publishEvent(buildMetaDataDTO(packageName, beanShenyuClient, method, basePath));
            }
            return;
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            ShenyuGrpcClient methodShenyuClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuGrpcClient.class);
            if (Objects.nonNull(methodShenyuClient)) {
                publisher.publishEvent(buildMetaDataDTO(packageName, methodShenyuClient, method, basePath));
            }
        }
    }

    private MetaDataRegisterDTO buildMetaDataDTO(final String packageName, final ShenyuGrpcClient shenyuGrpcClient,
                                                 final Method method, final String basePath) {
        String path = basePath.contains("*")
                ? buildAbsolutePath("/", contextPath, basePath.replace("*", ""), method.getName())
                : buildAbsolutePath("/", contextPath, basePath, shenyuGrpcClient.path());
        String desc = shenyuGrpcClient.desc();
        String host = IpUtils.isCompleteHost(this.host) ? this.host : IpUtils.getHost(this.host);
        String configRuleName = shenyuGrpcClient.ruleName();
        String ruleName = StringUtils.defaultIfBlank(configRuleName, path);
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
                .collect(Collectors.joining(","));
        MethodDescriptor.MethodType methodType = JsonServerServiceInterceptor.getMethodTypeMap().get(packageName + "/" + methodName);
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
                .rpcType(RpcTypeEnum.GRPC.getName())
                .rpcExt(buildRpcExt(shenyuGrpcClient, methodType))
                .enabled(shenyuGrpcClient.enabled())
                .build();
    }

    private String buildAbsolutePath(final String separator, final String... paths) {
        List<String> pathList = new ArrayList<>();
        for (String path : paths) {
            if (StringUtils.isBlank(path)) {
                continue;
            }
            String newPath = StringUtils.removeStart(path, separator);
            newPath = StringUtils.removeEnd(newPath, separator);
            pathList.add(newPath);
        }
        return separator + String.join(separator, pathList);
    }

    private String buildRpcExt(final ShenyuGrpcClient shenyuGrpcClient,
                               final MethodDescriptor.MethodType methodType) {
        GrpcExt build = GrpcExt.builder()
                .timeout(shenyuGrpcClient.timeout())
                .methodType(methodType)
                .build();
        return GsonUtils.getInstance().toJson(build);
    }

    private void exportJsonGenericService(final BindableService bindableService) {
        ServerServiceDefinition serviceDefinition = bindableService.bindService();

        try {
            ServerServiceDefinition jsonDefinition = JsonServerServiceInterceptor.useJsonMessages(serviceDefinition);
            serviceDefinitions.add(serviceDefinition);
            serviceDefinitions.add(jsonDefinition);
        } catch (Exception e) {
            LOG.error("export json generic service is fail", e);
        }
    }

    /**
     * get serviceDefinitions.
     *
     * @return serviceDefinitions
     */
    public List<ServerServiceDefinition> getServiceDefinitions() {
        return serviceDefinitions;
    }
}
