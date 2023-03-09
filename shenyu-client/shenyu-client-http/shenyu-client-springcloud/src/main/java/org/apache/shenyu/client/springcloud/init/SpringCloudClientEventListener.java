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

package org.apache.shenyu.client.springcloud.init;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.client.AbstractContextRefreshedEventListener;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.core.utils.PortUtils;
import org.apache.shenyu.client.springcloud.annotation.ShenyuSpringCloudClient;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationContext;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.env.Environment;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The type Spring cloud client event listener.
 */
public class SpringCloudClientEventListener extends AbstractContextRefreshedEventListener<Object, ShenyuSpringCloudClient> {

    private static final Logger LOG = LoggerFactory.getLogger(SpringCloudClientEventListener.class);

    private final Boolean isFull;
    
    private final Environment env;
    
    private final String servletContextPath;

    private final boolean addPrefixed;
    
    private final List<Class<? extends Annotation>> mappingAnnotation = new ArrayList<>(3);

    /**
     * Instantiates a new Spring cloud client bean post processor.
     *
     * @param clientConfig                   the shenyu client config
     * @param shenyuClientRegisterRepository the shenyuClientRegisterRepository
     * @param env                            the env
     */
    public SpringCloudClientEventListener(final PropertiesConfig clientConfig,
                                          final ShenyuClientRegisterRepository shenyuClientRegisterRepository,
                                          final Environment env) {
        super(clientConfig, shenyuClientRegisterRepository);
        Properties props = clientConfig.getProps();
        this.env = env;
        if (StringUtils.isBlank(getAppName())) {
            String errorMsg = "spring cloud param must config the appName";
            LOG.error(errorMsg);
            throw new ShenyuClientIllegalArgumentException(errorMsg);
        }
        this.addPrefixed = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.ADD_PREFIXED, Boolean.FALSE.toString()));
        this.isFull = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString()));
        this.servletContextPath = env.getProperty("server.servlet.context-path", "");
        mappingAnnotation.add(ShenyuSpringCloudClient.class);
        mappingAnnotation.add(RequestMapping.class);
    }

    @Override
    protected Sextet<String[], String, String, ApiHttpMethodEnum[], RpcTypeEnum, String> buildApiDocSextet(final Method method, final Annotation annotation, final Map<String, Object> beans) {
        RequestMapping requestMapping = AnnotatedElementUtils.findMergedAnnotation(method, RequestMapping.class);
        String produce = requestMapping.produces().length == 0 ? ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE : String.join(",", requestMapping.produces());
        String consume = requestMapping.consumes().length == 0 ? ShenyuClientConstants.MEDIA_TYPE_ALL_VALUE : String.join(",", requestMapping.consumes());
        String[] values = requestMapping.value();
        RequestMethod[] requestMethods = requestMapping.method();
        if (requestMethods.length == 0) {
            requestMethods = RequestMethod.values();
        }
        List<ApiHttpMethodEnum> collect = Stream.of(requestMethods).map(item -> ApiHttpMethodEnum.of(item.name())).collect(Collectors.toList());
        ApiHttpMethodEnum[] apiHttpMethodEnums = collect.toArray(new ApiHttpMethodEnum[]{});
        String version = "v0.01";
        return Sextet.with(values, consume, produce, apiHttpMethodEnums, RpcTypeEnum.SPRING_CLOUD, version);
    }

    @Override
    protected Map<String, Object> getBeans(final ApplicationContext context) {
        // Filter out is not controller out
        if (Boolean.TRUE.equals(isFull)) {
            getPublisher().publishEvent(MetaDataRegisterDTO.builder()
                    .contextPath(getContextPath())
                    .appName(getAppName())
                    .path(PathUtils.decoratorPathWithSlash(getContextPath()))
                    .rpcType(RpcTypeEnum.SPRING_CLOUD.getName())
                    .enabled(true)
                    .ruleName(getContextPath())
                    .build());
            return Collections.emptyMap();
        }
        return context.getBeansWithAnnotation(Controller.class);
    }

    @Override
    protected URIRegisterDTO buildURIRegisterDTO(final ApplicationContext context, final Map<String, Object> beans) {
        try {
            final String host = getHost();
            final int port = Integer.parseInt(Optional.ofNullable(getPort()).orElseGet(() -> "-1"));
            final int mergedPort = port <= 0 ? PortUtils.findPort(context.getAutowireCapableBeanFactory()) : port;
            return URIRegisterDTO.builder()
                    .contextPath(getContextPath())
                    .appName(getAppName())
                    .host(IpUtils.isCompleteHost(host) ? host : IpUtils.getHost(host))
                    .port(mergedPort)
                    .rpcType(RpcTypeEnum.SPRING_CLOUD.getName())
                    .build();
        } catch (ShenyuException e) {
            throw new ShenyuException(e.getMessage() + "please config ${shenyu.client.http.props.port} in xml/yml !");
        }
    }

    @Override
    protected void handleMethod(final Object bean, final Class<?> clazz,
                                @Nullable final ShenyuSpringCloudClient beanShenyuClient, final Method method,
                                final String superPath) {
        final RequestMapping requestMapping = AnnotatedElementUtils.findMergedAnnotation(method, RequestMapping.class);
        ShenyuSpringCloudClient methodShenyuClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringCloudClient.class);
        methodShenyuClient = Objects.isNull(methodShenyuClient) ? beanShenyuClient : methodShenyuClient;
        // the result of ReflectionUtils#getUniqueDeclaredMethods contains methods such as hashCode, wait, toSting
        // add Objects.nonNull(requestMapping) to make sure not register wrong method
        if (Objects.nonNull(methodShenyuClient) && Objects.nonNull(requestMapping)) {
            getPublisher().publishEvent(buildMetaDataDTO(bean, methodShenyuClient,
                    buildApiPath(method, superPath, methodShenyuClient), clazz, method));
        }
    }

    @Override
    protected String buildApiPath(final Method method, final String superPath,
                                  @NonNull final ShenyuSpringCloudClient methodShenyuClient) {
        final String contextPath = getContextPath();
        if (StringUtils.isNotBlank(methodShenyuClient.path())) {
            return pathJoin(contextPath, superPath, methodShenyuClient.path());
        }
        final String path = getPathByMethod(method);
        if (StringUtils.isNotBlank(path)) {
            return pathJoin(contextPath, superPath, path);
        }
        return pathJoin(contextPath, superPath);
    }
    
    private String getPathByMethod(@NonNull final Method method) {
        for (Class<? extends Annotation> mapping : mappingAnnotation) {
            final Annotation mergedAnnotation = AnnotatedElementUtils.findMergedAnnotation(method, mapping);
            final String pathByAnnotation = getPathByAnnotation(mergedAnnotation);
            if (StringUtils.isNotBlank(pathByAnnotation)) {
                return pathByAnnotation;
            }
        }
        return null;
    }
    
    private String getPathByAnnotation(@Nullable final Annotation annotation) {
        if (Objects.isNull(annotation)) {
            return null;
        }
        final Object value = AnnotationUtils.getValue(annotation, "value");
        if (value instanceof String && StringUtils.isNotBlank((String) value)) {
            return (String) value;
        }
        if (value instanceof String[] && ArrayUtils.isNotEmpty((String[]) value)
                && StringUtils.isNotBlank(((String[]) value)[0])) {
            return ((String[]) value)[0];
        }
        return null;
    }

    @Override
    protected String buildApiSuperPath(final Class<?> clazz, @Nullable final ShenyuSpringCloudClient beanShenyuClient) {
        if (Objects.nonNull(beanShenyuClient) && StringUtils.isNotBlank(beanShenyuClient.path())) {
            return beanShenyuClient.path();
        }
        RequestMapping requestMapping = AnnotationUtils.findAnnotation(clazz, RequestMapping.class);
        // Only the first path is supported temporarily
        if (Objects.nonNull(requestMapping) && ArrayUtils.isNotEmpty(requestMapping.path())
                && StringUtils.isNotBlank(requestMapping.path()[0])) {
            return requestMapping.path()[0];
        }
        return "";
    }

    @Override
    protected Class<ShenyuSpringCloudClient> getAnnotationType() {
        return ShenyuSpringCloudClient.class;
    }

    @Override
    protected MetaDataRegisterDTO buildMetaDataDTO(final Object bean,
                                                   final @NonNull ShenyuSpringCloudClient shenyuClient,
                                                   final String path, final Class<?> clazz, final Method method) {
        return MetaDataRegisterDTO.builder()
                .contextPath(StringUtils.defaultIfBlank(getContextPath(), this.servletContextPath))
                .addPrefixed(addPrefixed)
                .appName(getAppName())
                .serviceName(clazz.getName())
                .methodName(Optional.ofNullable(method).map(Method::getName).orElse(null))
                .path(path)
                .pathDesc(shenyuClient.desc())
                .parameterTypes(Optional.ofNullable(method)
                        .map(m -> Arrays.stream(m.getParameterTypes())
                                .map(Class::getName)
                                .collect(Collectors.joining(","))
                                ).orElse(null))
                .rpcType(RpcTypeEnum.SPRING_CLOUD.getName())
                .enabled(shenyuClient.enabled())
                .ruleName(StringUtils.defaultIfBlank(shenyuClient.ruleName(), path))
                .build();
    }

    @Override
    public String getAppName() {
        return env.getProperty("spring.application.name");
    }
}
