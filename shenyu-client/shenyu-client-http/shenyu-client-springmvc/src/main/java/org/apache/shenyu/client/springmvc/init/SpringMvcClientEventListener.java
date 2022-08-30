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

package org.apache.shenyu.client.springmvc.init;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.client.core.constant.ShenyuClientConstants;
import org.apache.shenyu.client.core.disruptor.ShenyuClientRegisterEventPublisher;
import org.apache.shenyu.client.core.exception.ShenyuClientIllegalArgumentException;
import org.apache.shenyu.client.springmvc.annotation.ShenyuSpringMvcClient;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.UriUtils;
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationListener;
import org.springframework.context.event.ContextRefreshedEvent;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Controller;
import org.springframework.util.ReflectionUtils;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Map;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;
import java.util.stream.Collectors;

/**
 * The type Shenyu spring mvc client event listener.
 */
public class SpringMvcClientEventListener implements ApplicationListener<ContextRefreshedEvent> {

    /**
     * api path separator.
     */
    private static final String PATH_SEPARATOR = "/";

    private static final Logger LOG = LoggerFactory.getLogger(SpringMvcClientEventListener.class);

    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();

    private final String contextPath;

    private final String appName;

    private final Boolean isFull;

    private final List<Class<? extends Annotation>> mappingAnnotation = new ArrayList<>(7);

    private final String[] pathAttributeNames = new String[]{"path", "value"};

    /**
     * Instantiates a new Spring mvc client event listener.
     *
     * @param clientConfig                   the client config
     * @param shenyuClientRegisterRepository the shenyu client register repository
     */
    public SpringMvcClientEventListener(final PropertiesConfig clientConfig,
                                        final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        Properties props = clientConfig.getProps();
        this.appName = props.getProperty(ShenyuClientConstants.APP_NAME);
        this.contextPath = Optional.ofNullable(props.getProperty(ShenyuClientConstants.CONTEXT_PATH)).map(UriUtils::repairData).orElse("");
        if (StringUtils.isBlank(appName) && StringUtils.isBlank(contextPath)) {
            String errorMsg = "http register param must config the appName or contextPath";
            LOG.error(errorMsg);
            throw new ShenyuClientIllegalArgumentException(errorMsg);
        }
        this.isFull = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString()));
        mappingAnnotation.add(ShenyuSpringMvcClient.class);
        mappingAnnotation.add(PostMapping.class);
        mappingAnnotation.add(GetMapping.class);
        mappingAnnotation.add(DeleteMapping.class);
        mappingAnnotation.add(PutMapping.class);
        mappingAnnotation.add(RequestMapping.class);
        publisher.start(shenyuClientRegisterRepository);
    }

    @Override
    public void onApplicationEvent(final ContextRefreshedEvent event) throws BeansException {
        // Filter out
        if (Boolean.TRUE.equals(isFull)) {
            return;
        }
        Map<String, Object> controllerBeans = event.getApplicationContext().getBeansWithAnnotation(Controller.class);
        for (Map.Entry<String, Object> entry : controllerBeans.entrySet()) {
            handler(entry.getValue());
        }
    }

    private void handler(@NonNull final Object bean) {
        Class<?> clazz = bean.getClass();
        if (AopUtils.isAopProxy(bean)) {
            clazz = AopUtils.getTargetClass(bean);
        }
        final ShenyuSpringMvcClient beanShenyuClient = AnnotatedElementUtils.findMergedAnnotation(clazz, ShenyuSpringMvcClient.class);
        final String superPath = buildApiSuperPath(clazz);
        // Compatible with previous versions
        if (Objects.nonNull(beanShenyuClient) && superPath.contains("*")) {
            publisher.publishEvent(buildMetaDataDTO(beanShenyuClient, pathJoin(contextPath, superPath), clazz, null));
            return;
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(clazz);
        for (Method method : methods) {
            final RequestMapping requestMapping = AnnotatedElementUtils.findMergedAnnotation(method, RequestMapping.class);
            ShenyuSpringMvcClient methodShenyuClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringMvcClient.class);
            methodShenyuClient = Objects.isNull(methodShenyuClient) ? beanShenyuClient : methodShenyuClient;
            // the result of ReflectionUtils#getUniqueDeclaredMethods contains method such as hashCode, wait, toSting
            // add Objects.nonNull(requestMapping) to make sure not register wrong method
            if (Objects.nonNull(methodShenyuClient) && Objects.nonNull(requestMapping)) {
                publisher.publishEvent(buildMetaDataDTO(methodShenyuClient, buildApiPath(method, superPath), clazz, method));
            }
        }
    }

    private String buildApiPath(@NonNull final Method method, @NonNull final String superPath) {
        ShenyuSpringMvcClient shenyuSpringMvcClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringMvcClient.class);
        if (Objects.nonNull(shenyuSpringMvcClient) && StringUtils.isNotBlank(shenyuSpringMvcClient.path())) {
            return pathJoin(contextPath, superPath, shenyuSpringMvcClient.path());
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

    private String buildApiSuperPath(@NonNull final Class<?> method) {
        ShenyuSpringMvcClient shenyuSpringMvcClient = AnnotatedElementUtils.findMergedAnnotation(method, ShenyuSpringMvcClient.class);
        if (Objects.nonNull(shenyuSpringMvcClient) && StringUtils.isNotBlank(shenyuSpringMvcClient.path())) {
            return shenyuSpringMvcClient.path();
        }
        RequestMapping requestMapping = AnnotationUtils.findAnnotation(method, RequestMapping.class);
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

    private MetaDataRegisterDTO buildMetaDataDTO(@NonNull final ShenyuSpringMvcClient shenyuSpringMvcClient,
                                                 final String path, final Class<?> clazz, final Method method) {
        return MetaDataRegisterDTO.builder()
            .contextPath(contextPath)
            .appName(appName)
            .serviceName(clazz.getName())
            .methodName(Optional.ofNullable(method).map(Method::getName).orElse(null))
            .path(path)
            .pathDesc(shenyuSpringMvcClient.desc())
            .parameterTypes(Optional.ofNullable(method)
                    .map(m -> Arrays.stream(m.getParameterTypes())
                            .map(Class::getName)
                            .collect(Collectors.joining(","))
                    ).orElse(null))
            .rpcType(RpcTypeEnum.HTTP.getName())
            .enabled(shenyuSpringMvcClient.enabled())
            .ruleName(StringUtils.defaultIfBlank(shenyuSpringMvcClient.ruleName(), path))
            .registerMetaData(shenyuSpringMvcClient.registerMetaData())
            .build();
    }

}
