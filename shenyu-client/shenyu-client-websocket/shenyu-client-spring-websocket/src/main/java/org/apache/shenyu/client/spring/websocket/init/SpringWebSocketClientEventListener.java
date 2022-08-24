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

package org.apache.shenyu.client.spring.websocket.init;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.spring.websocket.annotation.ShenyuServerEndpoint;
import org.apache.shenyu.client.spring.websocket.annotation.ShenyuSpringWebSocketClient;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationListener;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;

/**
 * The type Shenyu websocket client event listener.
 */
public class SpringWebSocketClientEventListener implements ApplicationListener<ContextRefreshedEvent> {

    /**
     * api path separator.
     */
    private static final String PATH_SEPARATOR = "/";

    private static final Logger LOG = LoggerFactory.getLogger(SpringWebSocketClientEventListener.class);

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final String contextPath;

    private final String appName;

    private final Boolean isFull;

    private final List<Class<? extends Annotation>> mappingAnnotation = new ArrayList<>(7);

    private final String[] pathAttributeNames = new String[] {"path", "value"};

    /**
     * Instantiates a new Spring websocket client event listener.
     *
     * @param clientConfig                   the client config
     * @param shenyuClientRegisterRepository the shenyu client register repository
     */
    public SpringWebSocketClientEventListener(final PropertiesConfig clientConfig,
                                              final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {

        Properties props = clientConfig.getProps();
        this.appName = props.getProperty(ShenyuClientConstants.APP_NAME);
        this.contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH, "");
        if (StringUtils.isBlank(appName) && StringUtils.isBlank(contextPath)) {
            String errorMsg = "websocket register param must config the appName or contextPath";
            LOG.error(errorMsg);
            throw new ShenyuClientIllegalArgumentException(errorMsg);
        }
        this.isFull = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString()));
        mappingAnnotation.add(ShenyuSpringWebSocketClient.class);

        publisher.start(shenyuClientRegisterRepository);
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        // Filter out is not controller out
        if (Boolean.TRUE.equals(isFull)) {
            return;
        }
        Map<String, Object> endpointBeans = contextRefreshedEvent.getApplicationContext().getBeansWithAnnotation(ShenyuServerEndpoint.class);
        handlerEndpintsBeans(contextRefreshedEvent.getApplicationContext(), endpointBeans);

        Map<String, Object> beans = contextRefreshedEvent.getApplicationContext().getBeansWithAnnotation(ShenyuSpringWebSocketClient.class);
        for (Map.Entry<String, Object> entry : beans.entrySet()) {
            handler(entry.getValue());
        }

    }

    private void handlerEndpintsBeans(final ApplicationContext context, final Map<String, Object> endpointBeans) {
        if (CollectionUtils.isEmpty(endpointBeans)) {
            return;
        }
        ShenyuServerEndpointerExporter exporter = (ShenyuServerEndpointerExporter) registerBean(context, ShenyuServerEndpointerExporter.class, "shenyuServerEndpointerExporter");
        for (Map.Entry<String, Object> entry : endpointBeans.entrySet()) {
            Object bean = entry.getValue();
            Class<?> clazz = bean.getClass();
            if (AopUtils.isAopProxy(bean)) {
                clazz = AopUtils.getTargetClass(bean);
            }
            exporter.registerEndpoint(clazz);
            handler(entry.getValue());
        }
    }

    private void handler(final Object bean) {
        Class<?> clazz = bean.getClass();
        if (AopUtils.isAopProxy(bean)) {
            clazz = AopUtils.getTargetClass(bean);
        }
        final String superPath = buildApiSuperPath(clazz);
        final ShenyuSpringWebSocketClient beanShenyuClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuSpringWebSocketClient.class);
        // Compatible with previous versions
        if (Objects.nonNull(beanShenyuClient) && superPath.contains("*")) {
            publisher.publishEvent(buildMetaDataDTO(beanShenyuClient, pathJoin(contextPath, superPath)));
            return;
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            ShenyuSpringWebSocketClient webSocketClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringWebSocketClient.class);
            webSocketClient = Objects.isNull(webSocketClient) ? beanShenyuClient : webSocketClient;
            if (Objects.nonNull(webSocketClient)) {
                publisher.publishEvent(buildMetaDataDTO(webSocketClient, buildApiPath(method, superPath)));
            }
        }
    }

    private String buildApiPath(@NonNull final Method method, @NonNull final String superPath) {
        ShenyuSpringWebSocketClient webSocketClient = AnnotationUtils.findAnnotation(method, ShenyuSpringWebSocketClient.class);
        if (Objects.nonNull(webSocketClient) && StringUtils.isNotBlank(webSocketClient.path())) {
            return pathJoin(contextPath, superPath, webSocketClient.path());
        }
        final String path = getPathByMethod(method);
        if (StringUtils.isNotBlank(path)) {
            return pathJoin(contextPath, superPath, path);
        }
        return pathJoin(contextPath, superPath);
    }

    private String getPathByMethod(@NonNull final Method method) {
        for (Class<? extends Annotation> mapping : mappingAnnotation) {
            final String pathByAnnotation = getPathByAnnotation(AnnotationUtils.findAnnotation(method, mapping), pathAttributeNames);
            if (StringUtils.isNotBlank(pathByAnnotation)) {
                return pathByAnnotation;
            }
        }
        return null;
    }

    private String getPathByAnnotation(@Nullable final Annotation annotation,
        @NonNull final String... pathAttributeName) {
        if (Objects.isNull(annotation)) {
            return null;
        }
        for (String s : pathAttributeName) {
            final Object value = AnnotationUtils.getValue(annotation, s);
            if (value instanceof String && StringUtils.isNotBlank((String) value)) {
                return (String) value;
            }
            // Only the first path is supported temporarily
            if (value instanceof String[] && ArrayUtils.isNotEmpty((String[]) value) && StringUtils.isNotBlank(((String[]) value)[0])) {
                return ((String[]) value)[0];
            }
        }
        return null;
    }

    private String buildApiSuperPath(@NonNull final Class<?> method) {
        ShenyuSpringWebSocketClient webSocketClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringWebSocketClient.class);
        if (Objects.nonNull(webSocketClient) && StringUtils.isNotBlank(webSocketClient.path())) {
            return webSocketClient.path();
        }
        return "";
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

    private MetaDataRegisterDTO buildMetaDataDTO(@NonNull final ShenyuSpringWebSocketClient webSocketClient,
        final String path) {
        return MetaDataRegisterDTO.builder()
            .contextPath(contextPath)
            .appName(appName)
            .path(path)
            .pathDesc(webSocketClient.desc())
            .rpcType(RpcTypeEnum.WEB_SOCKET.getName())
            .enabled(webSocketClient.enabled())
            .ruleName(StringUtils.defaultIfBlank(webSocketClient.ruleName(), path))
            .registerMetaData(webSocketClient.registerMetaData())
            .build();
    }

    private Object registerBean(final ApplicationContext applicationContext, final Class<?> requiredType, final String beanName) {
        ConfigurableApplicationContext configurableApplicationContext = (ConfigurableApplicationContext) applicationContext;
        DefaultListableBeanFactory defaultListableBeanFactory = (DefaultListableBeanFactory) configurableApplicationContext.getAutowireCapableBeanFactory();
        BeanDefinitionBuilder beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition(requiredType);
        defaultListableBeanFactory.registerBeanDefinition(beanName, beanDefinitionBuilder.getBeanDefinition());
        return configurableApplicationContext.getBean(requiredType);
    }

}
