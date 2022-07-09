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

package org.apache.shenyu.client.tars;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.tars.common.annotation.ShenyuTarsClient;
import org.apache.shenyu.client.tars.common.annotation.ShenyuTarsService;
import org.apache.shenyu.client.tars.common.dto.TarsRpcExt;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.LocalVariableTableParameterNameDiscoverer;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;
import org.springframework.util.ReflectionUtils;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * The Tars ServiceBean EventListener.
 */
public class TarsServiceBeanEventListener implements ApplicationListener<ContextRefreshedEvent> {

    private final LocalVariableTableParameterNameDiscoverer localVariableTableParameterNameDiscoverer = new LocalVariableTableParameterNameDiscoverer();

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final String contextPath;

    private final String ipAndPort;

    private final String host;

    private final int port;

    public TarsServiceBeanEventListener(final PropertiesConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        Properties props = clientConfig.getProps();
        String contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
        this.host = props.getProperty(ShenyuClientConstants.HOST);
        String port = props.getProperty(ShenyuClientConstants.PORT);
        if (StringUtils.isAnyBlank(contextPath, this.host, port)) {
            throw new ShenyuClientIllegalArgumentException("tars client must config the contextPath, ipAndPort");
        }
        this.contextPath = contextPath;
        this.ipAndPort = this.host + ":" + port;
        this.port = Integer.parseInt(port);
        publisher.start(shenyuClientRegisterRepository);
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) {
        Map<String, Object> controllerBeans = event.getApplicationContext().getBeansWithAnnotation(ShenyuTarsService.class);
        for (Map.Entry<String, Object> entry : controllerBeans.entrySet()) {
            handler(entry.getValue());
        }
    }

    private void handler(final Object serviceBean) {
        Class<?> clazz = serviceBean.getClass();
        if (AopUtils.isAopProxy(serviceBean)) {
            clazz = AopUtils.getTargetClass(serviceBean);
        }
        String serviceName = clazz.getAnnotation(ShenyuTarsService.class).serviceName();
        final ShenyuTarsClient beanTarsClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuTarsClient.class);
        final String superPath = buildApiSuperPath(beanTarsClient);
        if (superPath.contains("*") && Objects.nonNull(beanTarsClient)) {
            Method[] declaredMethods = ReflectionUtils.getDeclaredMethods(clazz);
            for (Method declaredMethod : declaredMethods) {
                publisher.publishEvent(buildMetaDataDTO(serviceName, superPath, beanTarsClient, declaredMethod, ""));
            }
            return;
        }
        Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            ShenyuTarsClient shenyuTarsClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuTarsClient.class);
            if (Objects.nonNull(shenyuTarsClient)) {
                publisher.publishEvent(buildMetaDataDTO(serviceName, superPath, shenyuTarsClient, method, buildRpcExtJson(method)));
            }
        }
    }

    private String buildApiSuperPath(final ShenyuTarsClient shenyuTarsClient) {
        if (Objects.nonNull(shenyuTarsClient) && !StringUtils.isBlank(shenyuTarsClient.path())) {
            return shenyuTarsClient.path();
        }
        return "";
    }

    private MetaDataRegisterDTO buildMetaDataDTO(final String serviceName, final String superPath, final ShenyuTarsClient shenyuTarsClient, final Method method, final String rpcExt) {
        String ipAndPort = this.ipAndPort;
        String path = superPath.contains("*") ? pathJoin(contextPath, superPath.replace("*", ""), method.getName()) : pathJoin(contextPath, superPath, shenyuTarsClient.path());
        String desc = shenyuTarsClient.desc();
        String host = IpUtils.isCompleteHost(this.host) ? this.host : IpUtils.getHost(this.host);
        String configRuleName = shenyuTarsClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        String methodName = method.getName();
        Class<?>[] parameterTypesClazz = method.getParameterTypes();
        String parameterTypes = Arrays.stream(parameterTypesClazz).map(Class::getName)
            .collect(Collectors.joining(","));
        return MetaDataRegisterDTO.builder()
            .appName(ipAndPort)
            .serviceName(serviceName)
            .methodName(methodName)
            .contextPath(this.contextPath)
            .path(path)
            .host(host)
            .port(port)
            .ruleName(ruleName)
            .pathDesc(desc)
            .parameterTypes(parameterTypes)
            .rpcType(RpcTypeEnum.TARS.getName())
            .rpcExt(rpcExt)
            .enabled(shenyuTarsClient.enabled())
            .build();
    }

    private String pathJoin(@NonNull final String... path) {
        StringBuilder result = new StringBuilder(Constants.PATH_SEPARATOR);
        for (String p : path) {
            if (!result.toString().endsWith(Constants.PATH_SEPARATOR)) {
                result.append(Constants.PATH_SEPARATOR);
            }
            result.append(p.startsWith(Constants.PATH_SEPARATOR) ? p.replaceFirst(Constants.PATH_SEPARATOR, "") : p);
        }
        return result.toString();
    }

    private TarsRpcExt.RpcExt buildRpcExt(final Method method) {
        String[] paramNames = localVariableTableParameterNameDiscoverer.getParameterNames(method);
        List<Pair<String, String>> params = new ArrayList<>();
        if (paramNames != null && paramNames.length > 0) {
            Class<?>[] paramTypes = method.getParameterTypes();
            for (int i = 0; i < paramNames.length; i++) {
                params.add(Pair.of(paramTypes[i].getName(), paramNames[i]));
            }
        }
        return new TarsRpcExt.RpcExt(method.getName(), params, method.getReturnType().getName());
    }

    private String buildRpcExtJson(final Method method) {
        List<TarsRpcExt.RpcExt> list = new ArrayList<>();
        list.add(buildRpcExt(method));
        TarsRpcExt buildList = new TarsRpcExt(list);
        return GsonUtils.getInstance().toJson(buildList);
    }
}
