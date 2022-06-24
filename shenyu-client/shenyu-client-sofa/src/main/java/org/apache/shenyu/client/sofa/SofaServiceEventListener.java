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
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.sofa.common.annotation.ShenyuSofaClient;
import org.apache.shenyu.client.sofa.common.dto.SofaRpcExt;
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

import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Arrays;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * The Sofa Service Event Listener.
 */
public class SofaServiceEventListener implements ApplicationListener<ContextRefreshedEvent> {

    private static final Logger LOG = LoggerFactory.getLogger(SofaServiceEventListener.class);

    /**
     * api path separator.
     */
    private static final String PATH_SEPARATOR = "/";

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final String contextPath;

    private final String appName;

    private final String host;

    private final String port;

    public SofaServiceEventListener(final PropertiesConfig clientConfig, final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        Properties props = clientConfig.getProps();
        String contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
        String appName = props.getProperty(ShenyuClientConstants.APP_NAME);
        if (StringUtils.isEmpty(contextPath)) {
            throw new ShenyuClientIllegalArgumentException("sofa client must config the contextPath");
        }
        this.contextPath = contextPath;
        this.appName = appName;
        this.host = props.getProperty(ShenyuClientConstants.HOST);
        this.port = props.getProperty(ShenyuClientConstants.PORT);
        publisher.start(shenyuClientRegisterRepository);
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        Map<String, ServiceFactoryBean> serviceBean = contextRefreshedEvent.getApplicationContext().getBeansOfType(ServiceFactoryBean.class);
        for (Map.Entry<String, ServiceFactoryBean> entry : serviceBean.entrySet()) {
            handler(entry.getValue());
        }
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
        final String superPath = buildApiSuperPath(beanSofaClient);
        if (superPath.contains("*") && Objects.nonNull(beanSofaClient)) {
            Method[] declaredMethods = ReflectionUtils.getDeclaredMethods(clazz);
            for (Method declaredMethod : declaredMethods) {
                publisher.publishEvent(buildMetaDataDTO(serviceBean, beanSofaClient, declaredMethod, ""));
            }
            return;
        }
        Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            ShenyuSofaClient methodSofaClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSofaClient.class);
            if (Objects.nonNull(methodSofaClient)) {
                publisher.publishEvent(buildMetaDataDTO(serviceBean, methodSofaClient, method, superPath));
            }
        }
    }

    private String buildApiSuperPath(final ShenyuSofaClient shenyuSofaClient) {
        if (Objects.nonNull(shenyuSofaClient) && !StringUtils.isBlank(shenyuSofaClient.path())) {
            return shenyuSofaClient.path();
        }
        return "";
    }

    private MetaDataRegisterDTO buildMetaDataDTO(final ServiceFactoryBean serviceBean, final ShenyuSofaClient shenyuSofaClient, final Method method, final String superPath) {
        String appName = this.appName;
        String path = pathJoin(contextPath, superPath, shenyuSofaClient.path());
        String desc = shenyuSofaClient.desc();
        String serviceName = serviceBean.getInterfaceClass().getName();
        String host = IpUtils.isCompleteHost(this.host) ? this.host : IpUtils.getHost(this.host);
        int port = StringUtils.isBlank(this.port) ? -1 : Integer.parseInt(this.port);
        String configRuleName = shenyuSofaClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
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

    private String buildRpcExt(final ShenyuSofaClient shenyuSofaClient) {
        SofaRpcExt build = SofaRpcExt.builder()
            .loadbalance(shenyuSofaClient.loadBalance())
            .retries(shenyuSofaClient.retries())
            .timeout(shenyuSofaClient.timeout())
            .build();
        return GsonUtils.getInstance().toJson(build);
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
