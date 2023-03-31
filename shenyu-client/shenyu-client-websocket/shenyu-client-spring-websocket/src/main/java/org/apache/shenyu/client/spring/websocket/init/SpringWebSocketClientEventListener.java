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
import org.apache.shenyu.client.core.client.AbstractContextRefreshedEventListener;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.utils.PortUtils;
import org.apache.shenyu.client.spring.websocket.annotation.ShenyuServerEndpoint;
import org.apache.shenyu.client.spring.websocket.annotation.ShenyuSpringWebSocketClient;
import org.apache.shenyu.common.enums.ApiHttpMethodEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.IpUtils;
import org.apache.shenyu.common.utils.PathUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.javatuples.Sextet;
import org.springframework.beans.factory.support.BeanDefinitionBuilder;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ReflectionUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;

/**
 * The type Shenyu websocket client event listener.
 */
public class SpringWebSocketClientEventListener extends AbstractContextRefreshedEventListener<Object, ShenyuSpringWebSocketClient> {

    private final String[] pathAttributeNames = new String[] {"path", "value"};

    private final List<Class<? extends Annotation>> mappingAnnotation = new ArrayList<>(7);

    private final Boolean isFull;

    private final String protocol;

    /**
     * Instantiates a new Spring websocket client event listener.
     *
     * @param clientConfig                   the client config
     * @param shenyuClientRegisterRepository the shenyu client register repository
     */
    public SpringWebSocketClientEventListener(final PropertiesConfig clientConfig,
                                              final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        super(clientConfig, shenyuClientRegisterRepository);
        Properties props = clientConfig.getProps();
        this.isFull = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString()));
        this.protocol = props.getProperty(ShenyuClientConstants.PROTOCOL, ShenyuClientConstants.WS);
        mappingAnnotation.add(ShenyuSpringWebSocketClient.class);
    }

    @Override
    protected Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(final Method method, final Annotation annotation, final Map<String, Object> beans) {
        ShenyuSpringWebSocketClient shenyuSpringWebSocketClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringWebSocketClient.class);
        if (Objects.isNull(shenyuSpringWebSocketClient)) {
            return null;
        }
        String produce = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String consume = ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE;
        String[] values = new String[]{shenyuSpringWebSocketClient.value()};
        ApiHttpMethodEnum[] apiHttpMethodEnums = new ApiHttpMethodEnum[]{ApiHttpMethodEnum.NOT_HTTP};
        String version = "v0.01";
        return Sextet.with(values, consume, produce, apiHttpMethodEnums, RpcTypeEnum.WEB_SOCKET, version);
    }

    @Override
    protected Map<String, Object> getBeans(final ApplicationContext context) {
        // Filter out is not controller out
        if (Boolean.TRUE.equals(isFull)) {
            return Collections.emptyMap();
        }
        Map<String, Object> endpointBeans = context.getBeansWithAnnotation(ShenyuServerEndpoint.class);
        registerEndpointsBeans(context, endpointBeans);

        return context.getBeansWithAnnotation(ShenyuSpringWebSocketClient.class);
    }

    @Override
    protected URIRegisterDTO buildURIRegisterDTO(final ApplicationContext context, final Map<String, Object> beans) {
        try {
            final String host = IpUtils.isCompleteHost(getHost()) ? getHost() : IpUtils.getHost(getHost());
            final int port = Integer.parseInt(Optional.ofNullable(getPort()).orElseGet(() -> "-1"));
            final int mergedPort = port <= 0 ? PortUtils.findPort(context.getAutowireCapableBeanFactory()) : port;
            return URIRegisterDTO.builder()
                    .contextPath(getContextPath())
                    .appName(getAppName())
                    .protocol(protocol)
                    .host(host)
                    .port(mergedPort)
                    .rpcType(RpcTypeEnum.WEB_SOCKET.getName())
                    .build();
        } catch (ShenyuException e) {
            throw new ShenyuException(e.getMessage() + "please config ${shenyu.client.http.props.port} in xml/yml !");
        }
    }

    @Override
    protected void handle(final String beanName, final Object bean) {
        Class<?> clazz = getCorrectedClass(bean);
        final ShenyuSpringWebSocketClient beanShenyuClient = AnnotatedElementUtils.findMergedAnnotation(clazz, getAnnotationType());
        final String superPath = buildApiSuperPath(clazz, beanShenyuClient);
        // Compatible with previous versions
        if (Objects.nonNull(beanShenyuClient)) {
            handleClass(clazz, bean, beanShenyuClient, superPath);
            return;
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            handleMethod(bean, clazz, beanShenyuClient, method, superPath);
        }
    }

    @Override
    protected String buildApiSuperPath(final Class<?> clazz, final ShenyuSpringWebSocketClient webSocketClient) {
        if (Objects.nonNull(webSocketClient) && StringUtils.isNotBlank(webSocketClient.path())) {
            return webSocketClient.path();
        }
        return "";
    }

    @Override
    protected void handleClass(final Class<?> clazz,
                               final Object bean,
                               @NonNull final ShenyuSpringWebSocketClient beanShenyuClient,
                               final String superPath) {
        Method[] methods = ReflectionUtils.getDeclaredMethods(clazz);
        for (Method method : methods) {
            getPublisher().publishEvent(buildMetaDataDTO(bean, beanShenyuClient, pathJoin(getContextPath(), superPath), clazz, method));
        }
    }

    @Override
    protected void handleMethod(final Object bean,
                                final Class<?> clazz,
                                @Nullable final ShenyuSpringWebSocketClient beanShenyuClient,
                                final Method method,
                                final String superPath) {
        ShenyuSpringWebSocketClient methodShenyuClient = AnnotatedElementUtils.findMergedAnnotation(method, getAnnotationType());
        methodShenyuClient = Objects.isNull(methodShenyuClient) ? beanShenyuClient : methodShenyuClient;
        if (Objects.nonNull(methodShenyuClient)) {
            getPublisher().publishEvent(buildMetaDataDTO(bean, methodShenyuClient, buildApiPath(method, superPath, methodShenyuClient), clazz, method));
        }
    }

    @Override
    protected Class<ShenyuSpringWebSocketClient> getAnnotationType() {
        return ShenyuSpringWebSocketClient.class;
    }

    @Override
    protected String buildApiPath(final Method method, final String superPath, final ShenyuSpringWebSocketClient methodShenyuClient) {
        if (Objects.nonNull(methodShenyuClient) && StringUtils.isNotBlank(methodShenyuClient.path())) {
            return pathJoin(getContextPath(), superPath, methodShenyuClient.path());
        }
        final String path = getPathByMethod(method);
        if (StringUtils.isNotBlank(path)) {
            return pathJoin(getContextPath(), superPath, path);
        }
        return pathJoin(getContextPath(), superPath);
    }

    @Override
    protected MetaDataRegisterDTO buildMetaDataDTO(final Object bean, @NonNull final ShenyuSpringWebSocketClient webSocketClient, final String path, final Class<?> clazz, final Method method) {
        return MetaDataRegisterDTO.builder()
                .contextPath(getContextPath())
                .appName(getAppName())
                .path(PathUtils.decoratorPathWithSlash(getContextPath()))
                .rpcType(RpcTypeEnum.WEB_SOCKET.getName())
                .enabled(true)
                .ruleName(StringUtils.defaultIfBlank(webSocketClient.ruleName(), getContextPath()))
                .build();
    }

    private void registerEndpointsBeans(final ApplicationContext context, final Map<String, Object> endpointBeans) {
        if (CollectionUtils.isEmpty(endpointBeans)) {
            return;
        }
        ShenyuServerEndpointerExporter exporter = (ShenyuServerEndpointerExporter) registerBean(context, ShenyuServerEndpointerExporter.class, "shenyuServerEndpointerExporter");
        for (Map.Entry<String, Object> entry : endpointBeans.entrySet()) {
            Class<?> clazz = getCorrectedClass(entry.getValue());
            exporter.registerEndpoint(clazz);
        }
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

    private Object registerBean(final ApplicationContext applicationContext, final Class<?> requiredType, final String beanName) {
        ConfigurableApplicationContext configurableApplicationContext = (ConfigurableApplicationContext) applicationContext;
        DefaultListableBeanFactory defaultListableBeanFactory = (DefaultListableBeanFactory) configurableApplicationContext.getAutowireCapableBeanFactory();
        BeanDefinitionBuilder beanDefinitionBuilder = BeanDefinitionBuilder.genericBeanDefinition(requiredType);
        defaultListableBeanFactory.registerBeanDefinition(beanName, beanDefinitionBuilder.getBeanDefinition());
        return configurableApplicationContext.getBean(requiredType);
    }
}
