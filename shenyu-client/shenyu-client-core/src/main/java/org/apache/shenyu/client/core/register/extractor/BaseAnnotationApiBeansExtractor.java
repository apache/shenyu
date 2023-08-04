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

package org.apache.shenyu.client.core.register.extractor;

import org.springframework.context.ApplicationContext;
import org.springframework.core.annotation.AnnotatedElementUtils;
import org.springframework.lang.NonNull;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * AnnotationApiBeansExtractor.<br>
 * API extraction converter that supports annotation.
 */
public abstract class BaseAnnotationApiBeansExtractor extends BaseApiBeansExtractor implements RpcApiBeansExtractor {
    
    private final List<Class<? extends Annotation>> supportedApiAnnotations = new ArrayList<>(1);
    
    private final List<Class<? extends Annotation>> supportedApiDefinitionAnnotations = new ArrayList<>(1);
    
    @Override
    protected Map<String, Object> extractSupportBeans(final ApplicationContext applicationContext) {
        return supportedApiAnnotations().stream()
                .map(applicationContext::getBeansWithAnnotation)
                .flatMap(stringObjectMap -> stringObjectMap.entrySet().stream())
                .distinct()
                .collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }
    
    @Override
    protected List<Method> extractSupportMethods(final Object bean, final ApplicationContext applicationContext) {
        return super.extractSupportMethods(bean, applicationContext)
                .stream()
                .filter(method -> supportedApiDefinitionAnnotations()
                        .stream()
                        .anyMatch(annotationClass -> Objects.nonNull(AnnotatedElementUtils.findMergedAnnotation(method, annotationClass))))
                .collect(Collectors.toList());
    }
    
    /**
     * Supported annotations.
     *
     * @return class
     */
    @NonNull
    protected List<Class<? extends Annotation>> supportedApiAnnotations() {
        return supportedApiAnnotations;
    }
    
    /**
     * Supported annotations.
     *
     * @return class
     */
    @NonNull
    protected List<Class<? extends Annotation>> supportedApiDefinitionAnnotations() {
        return supportedApiDefinitionAnnotations;
    }
    
    /**
     * addSupportedApiDefinitionAnnotations.
     *
     * @param annotation annotation
     */
    public void addSupportedApiDefinitionAnnotations(final Class<? extends Annotation> annotation) {
        supportedApiDefinitionAnnotations.add(annotation);
    }
    
    /**
     * addSupportedApiAnnotations.
     *
     * @param annotation annotation
     */
    public void addSupportedApiAnnotations(final Class<? extends Annotation> annotation) {
        supportedApiAnnotations.add(annotation);
    }
    
}
