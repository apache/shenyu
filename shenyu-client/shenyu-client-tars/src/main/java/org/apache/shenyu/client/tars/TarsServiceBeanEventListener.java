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
import org.apache.shenyu.client.core.client.AbstractContextRefreshedEventListener;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.tars.common.annotation.ShenyuTarsClient;
import org.apache.shenyu.client.tars.common.annotation.ShenyuTarsService;
import org.apache.shenyu.client.tars.common.dto.TarsRpcExt;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.javatuples.Sextet;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.core.LocalVariableTableParameterNameDiscoverer;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * The Tars ServiceBean EventListener.
 */
public class TarsServiceBeanEventListener extends AbstractContextRefreshedEventListener<Object, ShenyuTarsClient> {

    private final LocalVariableTableParameterNameDiscoverer localVariableTableParameterNameDiscoverer = new LocalVariableTableParameterNameDiscoverer();

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final String contextPath;

    private final String ipAndPort;

    public TarsServiceBeanEventListener(final PropertiesConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        super(clientConfig, shenyuClientRegisterRepository);
        Properties props = clientConfig.getProps();
        String contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
        String port = props.getProperty(ShenyuClientConstants.PORT);
        if (StringUtils.isAnyBlank(contextPath, this.getHost(), port)) {
            throw new ShenyuClientIllegalArgumentException("tars client must config the contextPath, ipAndPort");
        }
        this.contextPath = contextPath;
        this.ipAndPort = this.getHost() + ":" + port;
        publisher.start(shenyuClientRegisterRepository);
    }

    @Override
    protected Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(final Method method, final Annotation annotation, final Map<String, Object> beans) {
        ShenyuTarsClient shenyuTarsClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuTarsClient.class);
        if (Objects.isNull(shenyuTarsClient)) {
            return null;
        }
        String produce = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String consume = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String[] values = new String[]{shenyuTarsClient.value()};
        ApiHttpMethodEnum[] apiHttpMethodEnums = new ApiHttpMethodEnum[]{ApiHttpMethodEnum.NOT_HTTP};
        String version = "v0.01";
        return Sextet.with(values, consume, produce, apiHttpMethodEnums, RpcTypeEnum.TARS, version);
    }

    @Override
    protected Map<String, Object> getBeans(final ApplicationContext context) {
        return context.getBeansWithAnnotation(ShenyuTarsService.class);
    }

    @Override
    protected URIRegisterDTO buildURIRegisterDTO(final ApplicationContext context,
                                                 final Map<String, Object> beans) {
        return URIRegisterDTO.builder()
                .contextPath(this.contextPath)
                .appName(this.ipAndPort)
                .rpcType(RpcTypeEnum.TARS.getName())
                .host(this.getHost())
                .port(Integer.parseInt(this.getPort()))
                .build();
    }

    @Override
    protected String buildApiSuperPath(final Class<?> clazz, @Nullable final ShenyuTarsClient shenyuTarsClient) {
        if (Objects.nonNull(shenyuTarsClient) && !StringUtils.isBlank(shenyuTarsClient.path())) {
            return shenyuTarsClient.path();
        }
        return "";
    }

    @Override
    protected Class<ShenyuTarsClient> getAnnotationType() {
        return ShenyuTarsClient.class;
    }

    @Override
    public void handle(final String beanName, final Object bean) {
        Class<?> clazz = bean.getClass();
        if (AopUtils.isAopProxy(bean)) {
            clazz = AopUtils.getTargetClass(bean);
        }
        final ShenyuTarsClient beanTarsClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuTarsClient.class);
        final String superPath = buildApiSuperPath(clazz, beanTarsClient);
        if (superPath.contains("*") && Objects.nonNull(beanTarsClient)) {
            Method[] declaredMethods = ReflectionUtils.getDeclaredMethods(clazz);
            for (Method declaredMethod : declaredMethods) {
                publisher.publishEvent(buildMetaDataDTO(bean, beanTarsClient, buildApiPath(declaredMethod, superPath, beanTarsClient), clazz, declaredMethod));
            }
            return;
        }
        Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            ShenyuTarsClient shenyuTarsClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuTarsClient.class);
            if (Objects.nonNull(shenyuTarsClient)) {
                publisher.publishEvent(buildMetaDataDTO(bean, shenyuTarsClient, buildApiPath(method, superPath, shenyuTarsClient), clazz, method));
            }
        }
    }

    @Override
    public MetaDataRegisterDTO buildMetaDataDTO(final Object bean,
                                                @NonNull final ShenyuTarsClient shenyuTarsClient,
                                                final String path, final Class<?> clazz,
                                                final Method method) {
        String serviceName = clazz.getAnnotation(ShenyuTarsService.class).serviceName();
        String ipAndPort = this.ipAndPort;
        String desc = shenyuTarsClient.desc();
        String host = IpUtils.isCompleteHost(this.getHost()) ? this.getHost() : IpUtils.getHost(this.getHost());
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
            .port(Integer.parseInt(this.getPort()))
            .ruleName(ruleName)
            .pathDesc(desc)
            .parameterTypes(parameterTypes)
            .rpcType(RpcTypeEnum.TARS.getName())
            .rpcExt(buildRpcExtJson(method))
            .enabled(shenyuTarsClient.enabled())
            .build();
    }

    @Override
    protected String buildApiPath(final Method method, final String superPath, final ShenyuTarsClient shenyuTarsClient) {
        return superPath.contains("*")
                ? pathJoin(contextPath, superPath.replace("*", ""), method.getName())
                : pathJoin(contextPath, superPath, shenyuTarsClient.path());
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
