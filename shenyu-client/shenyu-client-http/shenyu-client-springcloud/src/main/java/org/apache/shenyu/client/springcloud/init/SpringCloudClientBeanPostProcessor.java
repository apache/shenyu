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
import org.apache.shenyu.register.client.api.ShenyuClientRegisterRepository;
import org.apache.shenyu.register.common.config.PropertiesConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.env.Environment;
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
 * The type Shenyu client bean post processor.
 */
public class SpringCloudClientBeanPostProcessor implements BeanPostProcessor {
    
    /**
     * api path separator.
     */
    private static final String PATH_SEPARATOR = "/";
    
    private static final Logger LOG = LoggerFactory.getLogger(SpringCloudClientBeanPostProcessor.class);
    
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
    public SpringCloudClientBeanPostProcessor(final PropertiesConfig clientConfig,
                                              final ShenyuClientRegisterRepository shenyuClientRegisterRepository,
                                              final Environment env) {
        String appName = env.getProperty("spring.application.name");
        Properties props = clientConfig.getProps();
        this.contextPath = props.getProperty(ShenyuClientConstants.CONTEXT_PATH);
        if (StringUtils.isBlank(appName)) {
            String errorMsg = "spring cloud param must config the appName";
            LOG.error(errorMsg);
            throw new ShenyuClientIllegalArgumentException(errorMsg);
        }
        this.env = env;
        this.isFull = Boolean.parseBoolean(props.getProperty(ShenyuClientConstants.IS_FULL, Boolean.FALSE.toString()));
        this.servletContextPath = env.getProperty("server.servlet.context-path", "");
        mappingAnnotation.add(ShenyuSpringCloudClient.class);
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
        
        final ShenyuSpringCloudClient beanShenyuClient = AnnotationUtils.findAnnotation(bean.getClass(), ShenyuSpringCloudClient.class);
        final String superPath = buildApiSuperPath(bean.getClass());
        // Compatible with previous versions
        if (Objects.nonNull(beanShenyuClient) && superPath.contains("*")) {
            publisher.publishEvent(buildMetaDataDTO(beanShenyuClient, pathJoin(contextPath, superPath)));
            return bean;
        }
        final Method[] methods = ReflectionUtils.getUniqueDeclaredMethods(bean.getClass());
        for (Method method : methods) {
            ShenyuSpringCloudClient methodShenyuClient = AnnotationUtils.findAnnotation(method, ShenyuSpringCloudClient.class);
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
        ShenyuSpringCloudClient shenyuSpringCloudClient = AnnotationUtils.findAnnotation(method, ShenyuSpringCloudClient.class);
        if (Objects.nonNull(shenyuSpringCloudClient) && StringUtils.isNotBlank(shenyuSpringCloudClient.path())) {
            return pathJoin(contextPath, superPath, shenyuSpringCloudClient.path());
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
        ShenyuSpringCloudClient shenyuSpringCloudClient = AnnotationUtils.findAnnotation(method, ShenyuSpringCloudClient.class);
        if (Objects.nonNull(shenyuSpringCloudClient) && StringUtils.isNotBlank(shenyuSpringCloudClient.path())) {
            return shenyuSpringCloudClient.path();
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
    
    private MetaDataRegisterDTO buildMetaDataDTO(final ShenyuSpringCloudClient shenyuSpringCloudClient, final String path) {
        String appName = env.getProperty("spring.application.name");
        String desc = shenyuSpringCloudClient.desc();
        String configRuleName = shenyuSpringCloudClient.ruleName();
        String ruleName = ("".equals(configRuleName)) ? path : configRuleName;
        return MetaDataRegisterDTO.builder()
                .contextPath(StringUtils.defaultIfBlank(this.contextPath, this.servletContextPath))
                .appName(appName)
                .path(path)
                .pathDesc(desc)
                .rpcType(RpcTypeEnum.SPRING_CLOUD.getName())
                .enabled(shenyuSpringCloudClient.enabled())
                .ruleName(ruleName)
                .build();
    }
}


