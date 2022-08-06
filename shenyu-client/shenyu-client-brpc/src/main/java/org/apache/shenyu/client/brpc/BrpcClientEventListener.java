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

package org.apache.shenyu.client.brpc;

import com.baidu.brpc.spring.annotation.RpcExporter;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.brpc.common.annotation.ShenyuBrpcClient;
import org.apache.shenyu.client.brpc.common.dto.BrpcRpcExt;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type Shenyu brpc client event listener.
 */
public class BrpcClientEventListener implements ApplicationListener<ContextRefreshedEvent> {
    
    /**
     * api path separator.
     */
    private static final String PATH_SEPARATOR = "/";
    
    private final AtomicBoolean registered = new AtomicBoolean(false);
    
    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
    
    private final String contextPath;
    
    private final String ipAndPort;
    
    private final String host;
    
    private final String port;
    
    public BrpcClientEventListener(final PropertiesConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        Properties props = clientConfig.getProps();
        this.contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
        this.ipAndPort = props.getProperty(ShenyuClientConstants.IP_PORT);
        this.port = props.getProperty(ShenyuClientConstants.PORT);
        if (StringUtils.isAnyBlank(contextPath, ipAndPort, port)) {
            throw new ShenyuClientIllegalArgumentException("brpc client must config the contextPath, ipAndPort");
        }
        this.host = props.getProperty(ShenyuClientConstants.HOST);
        publisher.start(shenyuClientRegisterRepository);
    }
    
    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {
        if (!registered.compareAndSet(false, true)) {
            return;
        }
        final Map<String, Object> beans = event.getApplicationContext().getBeansWithAnnotation(RpcExporter.class);
        if (MapUtils.isEmpty(beans)) {
            return;
        }
        for (Map.Entry<String, Object> entry : beans.entrySet()) {
            handler(entry.getValue());
        }
    }
    
    private void handler(final Object bean) {
        Class<?> clazz = bean.getClass();
        if (AopUtils.isAopProxy(bean)) {
            clazz = AopUtils.getTargetClass(bean);
        }
        final RpcExporter rpcExporter = AnnotatedElementUtils.findMergedAnnotation(clazz, RpcExporter.class);
        final ShenyuBrpcClient beanShenyuClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuBrpcClient.class);
        final String superPath = buildApiSuperPath(beanShenyuClient);
        if (superPath.contains("*") && Objects.nonNull(beanShenyuClient)) {
            Method[] methods = ReflectionUtils.getDeclaredMethods(clazz);
            for (Method method : methods) {
                publisher.publishEvent(buildMetaDataDTO(clazz, beanShenyuClient, rpcExporter, method, superPath));
            }
            return;
        }
        Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            ShenyuBrpcClient methodShenyuClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuBrpcClient.class);
            if (Objects.nonNull(methodShenyuClient)) {
                publisher.publishEvent(buildMetaDataDTO(clazz, methodShenyuClient, rpcExporter, method, superPath));
            }
        }
    }
    
    private MetaDataRegisterDTO buildMetaDataDTO(final Class<?> clazz, final ShenyuBrpcClient shenyuDubboClient,
                                                 final RpcExporter rpcExporter, final Method method, final String superPath) {
        String path = superPath.contains("*") ? pathJoin(contextPath, superPath.replace("*", ""), method.getName()) : pathJoin(contextPath, superPath, shenyuDubboClient.path());
        String desc = shenyuDubboClient.desc();
        String configRuleName = shenyuDubboClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName).collect(Collectors.joining(","));
        return MetaDataRegisterDTO.builder()
                .appName(ipAndPort)
                .serviceName(buildServiceName(clazz))
                .methodName(methodName)
                .contextPath(contextPath)
                .host(buildHost())
                .port(buildPort(rpcExporter))
                .path(path)
                .ruleName(ruleName)
                .pathDesc(desc)
                .parameterTypes(parameterTypes)
                .rpcExt(buildRpcExtJson(method))
                .rpcType(RpcTypeEnum.DUBBO.getName())
                .enabled(shenyuDubboClient.enabled())
                .build();
    }
    
    private BrpcRpcExt.RpcExt buildRpcExt(final Method method) {
        List<String> paramTypes = Stream.of(method.getParameterTypes())
                .map(Class::getName)
                .collect(Collectors.toList());
        return new BrpcRpcExt.RpcExt(method.getName(), paramTypes, method.getReturnType().getName());
    }
    
    private String buildRpcExtJson(final Method method) {
        List<BrpcRpcExt.RpcExt> list = new ArrayList<>();
        list.add(buildRpcExt(method));
        BrpcRpcExt buildList = new BrpcRpcExt(list);
        return GsonUtils.getInstance().toJson(buildList);
    }
    
    private String buildServiceName(final Class<?> clazz) {
        if (clazz.getInterfaces().length == 0) {
            return clazz.getName();
        }
        // if it is async interface, we should subscribe the sync interface
        return clazz.getInterfaces()[0].getName();
    }
    
    private String buildHost() {
        return IpUtils.isCompleteHost(this.host) ? this.host : IpUtils.getHost(this.host);
    }
    
    private int buildPort(final RpcExporter rpcExporter) {
        return Integer.parseInt(StringUtils.isBlank(this.port) ? rpcExporter.port() : this.port);
    }
    
    private String buildApiSuperPath(final ShenyuBrpcClient shenyuBrpcClient) {
        return Optional.ofNullable(shenyuBrpcClient).map(ShenyuBrpcClient::path).orElse("");
    }
    
    private String pathJoin(@NonNull final String... path) {
        StringBuilder result = new StringBuilder(PATH_SEPARATOR);
        for (String p : path) {
            if (!result.toString().endsWith(PATH_SEPARATOR)) {
                result.append(PATH_SEPARATOR);
            }
            result.append(p.startsWith(PATH_SEPARATOR) ? p.replaceFirst(PATH_SEPARATOR, "") : p);
        }
        return result.toString();
    }
}
