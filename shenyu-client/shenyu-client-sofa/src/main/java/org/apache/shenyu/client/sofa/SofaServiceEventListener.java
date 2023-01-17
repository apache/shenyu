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

package org.apache.shenyu.client.sofa;

import com.alipay.sofa.runtime.service.component.Service;
import com.alipay.sofa.runtime.spring.factory.ServiceFactoryBean;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.client.AbstractContextRefreshedEventListener;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.sofa.common.annotation.ShenyuSofaClient;
import org.apache.shenyu.client.sofa.common.dto.SofaRpcExt;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.javatuples.Sextet;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The Sofa Service Event Listener.
 */
public class SofaServiceEventListener extends AbstractContextRefreshedEventListener<ServiceFactoryBean, ShenyuSofaClient> {

    private static final Logger LOG = LoggerFactory.getLogger(SofaServiceEventListener.class);

    public SofaServiceEventListener(final PropertiesConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        super(clientConfig, shenyuClientRegisterRepository);
    }

    @Override
    protected Map<String, ServiceFactoryBean> getBeans(final ApplicationContext context) {
        return context.getBeansOfType(ServiceFactoryBean.class);
    }

    @Override
    protected URIRegisterDTO buildURIRegisterDTO(final ApplicationContext context,
                                                 final Map<String, ServiceFactoryBean> beans) {
        return URIRegisterDTO.builder()
                .contextPath(this.getContextPath())
                .appName(this.getAppName())
                .rpcType(RpcTypeEnum.SOFA.getName())
                .host(buildHost())
                .port(buildPort())
                .build();
    }

    @Override
    protected String buildApiSuperPath(final Class<?> clazz,
                                       final ShenyuSofaClient beanShenyuClient) {
        if (Objects.nonNull(beanShenyuClient) && !StringUtils.isBlank(beanShenyuClient.path())) {
            return beanShenyuClient.path();
        }
        return "";
    }

    @Override
    protected Class<ShenyuSofaClient> getAnnotationType() {
        return ShenyuSofaClient.class;
    }

    @Override
    protected String buildApiPath(final Method method,
                                  final String superPath,
                                  @NonNull final ShenyuSofaClient shenyuSofaClient) {
        final String contextPath = this.getContextPath();
        return superPath.contains("*") ? pathJoin(contextPath, superPath.replace("*", ""), method.getName())
                : pathJoin(contextPath, superPath, shenyuSofaClient.path());
    }

    @Override
    protected MetaDataRegisterDTO buildMetaDataDTO(final ServiceFactoryBean serviceBean,
                                                   @NonNull final ShenyuSofaClient shenyuSofaClient,
                                                   final String superPath,
                                                   final Class<?> clazz,
                                                   final Method method) {
        String appName = this.getAppName();
        String contextPath = this.getContextPath();
        String path = pathJoin(contextPath, superPath, shenyuSofaClient.path());
        String serviceName = serviceBean.getInterfaceClass().getName();
        String host = buildHost();
        int port = buildPort();
        String desc = shenyuSofaClient.desc();
        String configRuleName = shenyuSofaClient.ruleName();
        String ruleName = StringUtils.isEmpty(configRuleName) ? path : configRuleName;
        String methodName = method.getName();
        String parameterTypes = Arrays.stream(method.getParameters())
                .map(parameter -> {
                    StringBuilder result = new StringBuilder(parameter.getType().getName());
                    final Type type = parameter.getParameterizedType();
                    if (type instanceof ParameterizedType) {
                        final Type[] actualTypeArguments = ((ParameterizedType) type).getActualTypeArguments();
                        for (Type actualTypeArgument : actualTypeArguments) {
                            result.append("#").append(actualTypeArgument.getTypeName());
                        }
                    }
                    return result.toString();
                }).collect(Collectors.joining(","));
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
                .rpcType(RpcTypeEnum.SOFA.getName())
                .rpcExt(buildRpcExt(shenyuSofaClient))
                .enabled(shenyuSofaClient.enabled())
                .build();
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        Map<String, ServiceFactoryBean> serviceBean = contextRefreshedEvent.getApplicationContext().getBeansOfType(ServiceFactoryBean.class);
        for (Map.Entry<String, ServiceFactoryBean> entry : serviceBean.entrySet()) {
            handler(entry.getValue());
        }
    }

    @Override
    protected Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(final Method method, final Annotation annotation,
                                                                                                           final Map<String, ServiceFactoryBean> beans) {
        ShenyuSofaClient shenyuSofaClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSofaClient.class);
        if (Objects.isNull(shenyuSofaClient)) {
            return null;
        }
        String produce = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String consume = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String[] values = new String[]{shenyuSofaClient.value()};
        ApiHttpMethodEnum[] apiHttpMethodEnums = new ApiHttpMethodEnum[]{ApiHttpMethodEnum.NOT_HTTP};
        String version = "v0.01";
        return Sextet.with(values, consume, produce, apiHttpMethodEnums, RpcTypeEnum.SOFA, version);
    }

    private void handler(final ServiceFactoryBean serviceBean) {
        Class<?> clazz;
        Object targetProxy;
        try {
            targetProxy = ((Service) Objects.requireNonNull(serviceBean.getObject())).getTarget();
            clazz = targetProxy.getClass();
        } catch (Exception e) {
            LOG.error("failed to get sofa target class", e);
            return;
        }
        if (AopUtils.isAopProxy(targetProxy)) {
            clazz = AopUtils.getTargetClass(targetProxy);
        }
        final ShenyuSofaClient beanSofaClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuSofaClient.class);
        final String superPath = buildApiSuperPath(clazz, beanSofaClient);
        if (superPath.contains("*") && Objects.nonNull(beanSofaClient)) {
            Method[] declaredMethods = ReflectionUtils.getDeclaredMethods(clazz);
            for (Method declaredMethod : declaredMethods) {
                getPublisher().publishEvent(buildMetaDataDTO(serviceBean, beanSofaClient, superPath, clazz, declaredMethod));
            }
            return;
        }
        Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            ShenyuSofaClient methodSofaClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSofaClient.class);
            if (Objects.nonNull(methodSofaClient)) {
                getPublisher().publishEvent(buildMetaDataDTO(serviceBean, methodSofaClient, superPath, clazz, method));
            }
        }
    }

    private String buildRpcExt(final ShenyuSofaClient shenyuSofaClient) {
        SofaRpcExt build = SofaRpcExt.builder()
            .loadbalance(shenyuSofaClient.loadBalance())
            .retries(shenyuSofaClient.retries())
            .timeout(shenyuSofaClient.timeout())
            .build();
        return GsonUtils.getInstance().toJson(build);
    }

    private String buildHost() {
        final String host = this.getHost();
        return IpUtils.isCompleteHost(host) ? host : IpUtils.getHost(host);
    }

    private int buildPort() {
        final String port = this.getPort();
        return StringUtils.isBlank(port) ? -1 : Integer.parseInt(port);
    }
}
