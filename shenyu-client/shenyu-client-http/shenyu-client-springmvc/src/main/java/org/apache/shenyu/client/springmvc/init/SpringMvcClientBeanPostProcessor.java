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
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
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
import java.util.List;
import java.util.Objects;
import java.util.Properties;

/**
 * The type Shenyu spring mvc client bean post processor.
 */
public class SpringMvcClientBeanPostProcessor implements BeanPostProcessor {
    
    /**
     * api path separator.
     */
    private static final String PATH_SEPARATOR = "/";
    
    private static final Logger LOG = LoggerFactory.getLogger(SpringMvcClientBeanPostProcessor.class);
    
    private final ShenyuClientRegisterEventPublisher publisher = ShenyuClientRegisterEventPublisher.getInstance();
    
    private final String contextPath;
    
    private final String appName;
    
    private final Boolean isFull;
    
    private final List<Class<? extends Annotation>> mappingAnnotation = new ArrayList<>(7);
    
    private final String[] pathAttributeNames = new String[]{"path", "value"};
    
    /**
     * Instantiates a new Spring mvc client bean post processor.
     *
     * @param clientConfig                   the client config
     * @param shenyuClientRegisterRepository the shenyu client register repository
     */
    public SpringMvcClientBeanPostProcessor(final PropertiesConfig clientConfig,
                                            final ShenyuClientRegisterRepository shenyuClientRegisterRepository) {
        Properties props = clientConfig.getProps();
        this.appName = props.getProperty(ShenyuClientConstants.APP_NAME);
        this.contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH, "");
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
    public Object postProcessAfterInitialization(@NonNull final Object bean, @NonNull final String beanName) throws BeansException {
        // Filter out is not controller out
        if (Boolean.TRUE.equals(isFull) || !hasAnnotation(bean.getClass(), Controller.class)) {
            return bean;
        }
        
        final ShenyuSpringMvcClient beanShenyuClient = AnnotationUtils.findAnnotation(bean.getClass(), ShenyuSpringMvcClient.class);
        final String superPath = buildApiSuperPath(bean.getClass());
        // Compatible with previous versions
        if (Objects.nonNull(beanShenyuClient) && superPath.contains("*")) {
            publisher.publishEvent(buildMetaDataDTO(beanShenyuClient, pathJoin(contextPath, superPath)));
            return bean;
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(bean.getClass());
        for (Method method : methods) {
            ShenyuSpringMvcClient methodShenyuClient = AnnotationUtils.findAnnotation(method, ShenyuSpringMvcClient.class);
            methodShenyuClient = Objects.isNull(methodShenyuClient) ? beanShenyuClient : methodShenyuClient;
            if (Objects.nonNull(methodShenyuClient)) {
                publisher.publishEvent(buildMetaDataDTO(methodShenyuClient, buildApiPath(method, superPath)));
            }
        }
        
        return bean;
    }
    
    private <A extends Annotation> boolean hasAnnotation(final @NonNull Class<?> clazz, final @NonNull Class<A> annotationType) {
        return Objects.nonNull(AnnotationUtils.findAnnotation(clazz, annotationType));
    }
    
    private String buildApiPath(@NonNull final Method method, @NonNull final String superPath) {
        ShenyuSpringMvcClient shenyuSpringMvcClient = AnnotationUtils.findAnnotation(method, ShenyuSpringMvcClient.class);
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
        ShenyuSpringMvcClient shenyuSpringMvcClient = AnnotationUtils.findAnnotation(method, ShenyuSpringMvcClient.class);
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
    
    private MetaDataRegisterDTO buildMetaDataDTO(@NonNull final ShenyuSpringMvcClient shenyuSpringMvcClient, final String path) {
        return MetaDataRegisterDTO.builder()
                .contextPath(contextPath)
                .appName(appName)
                .path(path)
                .pathDesc(shenyuSpringMvcClient.desc())
                .rpcType(RpcTypeEnum.HTTP.getName())
                .enabled(shenyuSpringMvcClient.enabled())
                .ruleName(StringUtils.defaultIfBlank(shenyuSpringMvcClient.ruleName(), path))
                .registerMetaData(shenyuSpringMvcClient.registerMetaData())
                .build();
    }
    
}
