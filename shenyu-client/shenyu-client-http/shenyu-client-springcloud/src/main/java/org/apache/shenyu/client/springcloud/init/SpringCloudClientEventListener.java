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
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.springcloud.annotation.ShenyuSpringCloudClient;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.support.AopUtils;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.env.Environment;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Controller;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.RequestMapping;

import java.lang.annotation.Annotation;
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
 * The type Spring cloud client event listener.
 */
public class SpringCloudClientEventListener implements ApplicationListener<ContextRefreshedEvent> {
    
    /**
     * api path separator.
     */
    private static final String PATH_SEPARATOR = "/";
    
    private static final Logger LOG = LoggerFactory.getLogger(SpringCloudClientEventListener.class);
    
    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
    
    private final String contextPath;
    
    private final Boolean isFull;
    
    private final Environment env;
    
    private final String servletContextPath;
    
    private final List<Class<? extends Annotation>> mappingAnnotation = new ArrayList<>(7);
    
    private final String[] pathAttributeNames = new String[]{"path", "value"};
    
    /**
     * Instantiates a new Spring cloud client bean post processor.
     *
     * @param clientConfig                   the client config
     * @param shenyuClientRegisterRepository the shenyu client register repository
     * @param env                            the env
     */
    public SpringCloudClientEventListener(final PropertiesConfig clientConfig,
                                          final ShenyuClientRegisterRepository shenyuClientRegisterRepository,
                                          final Environment env) {
        String appName = env.getProperty("spring.application.name");
        Properties props = clientConfig.getProps();
        this.contextPath = Optional.ofNullable(props.getProperty(ShenyuClientConstants.CONTEXT_PATH)).map(UriUtils::repairData).orElse(null);
        if (StringUtils.isBlank(appName)) {
            String errorMsg = "spring cloud param must config the appName";
            LOG.error(errorMsg);
            throw new ShenyuClientIllegalArgumentException(errorMsg);
        }
        this.env = env;
        this.isFull = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString()));
        this.servletContextPath = env.getProperty("server.servlet.context-path", "");
        mappingAnnotation.add(ShenyuSpringCloudClient.class);
        mappingAnnotation.add(RequestMapping.class);
        publisher.start(shenyuClientRegisterRepository);
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent contextRefreshedEvent) {
        // Filter out is not controller out
        if (Boolean.TRUE.equals(isFull)) {
            return;
        }
        Map<String, Object> beans = contextRefreshedEvent.getApplicationContext().getBeansWithAnnotation(Controller.class);
        for (Map.Entry<String, Object> entry : beans.entrySet()) {
            handler(entry.getValue());
        }
    }

    private void handler(final Object bean) {
        Class<?> clazz = bean.getClass();
        if (AopUtils.isAopProxy(bean)) {
            clazz = AopUtils.getTargetClass(bean);
        }
        final ShenyuSpringCloudClient beanShenyuClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuSpringCloudClient.class);
        final String superPath = buildApiSuperPath(clazz, beanShenyuClient);
        // Compatible with previous versions
        if (Objects.nonNull(beanShenyuClient) && superPath.contains("*")) {
            publisher.publishEvent(buildMetaDataDTO(beanShenyuClient, pathJoin(contextPath, superPath), clazz, null));
            return;
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            final RequestMapping requestMapping = AnnotatedElementUtils.findMergedAnnotation(method, RequestMapping.class);
            ShenyuSpringCloudClient methodShenyuClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringCloudClient.class);
            methodShenyuClient = Objects.isNull(methodShenyuClient) ? beanShenyuClient : methodShenyuClient;
            // the result of ReflectionUtils#getUniqueDeclaredMethods contains methods such as hashCode, wait, toSting
            // add Objects.nonNull(requestMapping) to make sure not register wrong method
            if (Objects.nonNull(methodShenyuClient) && Objects.nonNull(requestMapping)) {
                publisher.publishEvent(buildMetaDataDTO(methodShenyuClient, buildApiPath(method, superPath, methodShenyuClient), clazz, method));
            }
        }
    }
    
    private String buildApiPath(@NonNull final Method method, @NonNull final String superPath, final ShenyuSpringCloudClient methodShenyuClient) {
        if (Objects.nonNull(methodShenyuClient) && StringUtils.isNotBlank(methodShenyuClient.path())) {
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
            final String pathByAnnotation = getPathByAnnotation(mergedAnnotation, pathAttributeNames);
            if (StringUtils.isNotBlank(pathByAnnotation)) {
                return pathByAnnotation;
            }
        }
        return null;
    }
    
    private String getPathByAnnotation(@Nullable final Annotation annotation, @NonNull final String... pathAttributeName) {
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
    
    private String buildApiSuperPath(@NonNull final Class<?> clazz, final ShenyuSpringCloudClient beanShenyuClient) {
        if (Objects.nonNull(beanShenyuClient) && StringUtils.isNotBlank(beanShenyuClient.path())) {
            return beanShenyuClient.path();
        }
        RequestMapping requestMapping = AnnotationUtils.findAnnotation(clazz, RequestMapping.class);
        // Only the first path is supported temporarily
        if (Objects.nonNull(requestMapping) && ArrayUtils.isNotEmpty(requestMapping.path()) && StringUtils.isNotBlank(requestMapping.path()[0])) {
            return requestMapping.path()[0];
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
    
    private MetaDataRegisterDTO buildMetaDataDTO(final ShenyuSpringCloudClient shenyuSpringCloudClient,
                                                 final String path, final Class<?> clazz, final Method method) {
        String appName = env.getProperty("spring.application.name");
        String desc = shenyuSpringCloudClient.desc();
        String configRuleName = shenyuSpringCloudClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        return MetaDataRegisterDTO.builder()
                .contextPath(StringUtils.defaultIfBlank(this.contextPath, this.servletContextPath))
                .appName(appName)
                .serviceName(clazz.getName())
                .methodName(Optional.ofNullable(method).map(Method::getName).orElse(null))
                .path(path)
                .pathDesc(desc)
                .parameterTypes(Optional.ofNullable(method)
                        .map(m -> Arrays.stream(m.getParameterTypes())
                                .map(Class::getName)
                                .collect(Collectors.joining(","))
                        ).orElse(null))
                .rpcType(RpcTypeEnum.SPRING_CLOUD.getName())
                .enabled(shenyuSpringCloudClient.enabled())
                .ruleName(ruleName)
                .build();
    }
}


