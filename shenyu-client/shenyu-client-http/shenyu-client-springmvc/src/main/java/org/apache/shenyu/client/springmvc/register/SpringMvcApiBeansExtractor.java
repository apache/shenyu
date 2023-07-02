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

package org.apache.shenyu.client.springmvc.register;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.shenyu.client.core.register.ApiBean;
import org.apache.shenyu.client.core.register.extractor.BaseAnnotationApiBeansExtractor;
import org.apache.shenyu.client.core.register.extractor.ApiBeansExtractor;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.jetbrains.annotations.NotNull;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class SpringMvcApiBeansExtractor extends BaseAnnotationApiBeansExtractor implements ApiBeansExtractor {
    
    private final List<Class<? extends Annotation>> supportedApiAnnotations = new ArrayList<>(1);
    
    private final List<Class<? extends Annotation>> supportedApiDefinitionAnnotations = new ArrayList<>(1);
    
    public SpringMvcApiBeansExtractor() {
        // Annotations supported by class
        supportedApiAnnotations.add(Controller.class);
        supportedApiAnnotations.add(RequestMapping.class);
        
        // Annotations supported by the method
        supportedApiDefinitionAnnotations.add(RequestMapping.class);
    }
    
    @Override
    public String clientName() {
        return RpcTypeEnum.HTTP.getName();
    }
    
    @Override
    protected void apiPostProcess(final ApiBean api) {
        // Get from annotations
        // Currently only RequestMapping is supported
        final RequestMapping requestMapping = api.getAnnotation(RequestMapping.class);
        
        String beanPath = Objects.isNull(requestMapping) ? "" : getPath(requestMapping);
        // rewrite api path
        api.setBeanPath(beanPath);
        
        // Get additional values from the annotation.
        // TO_DO : Provides support annotation extensions
    }
    
    @Override
    protected void definitionPostProcess(final ApiBean.ApiDefinition apiDefinition) {
        // Get from annotations
        // Currently only RequestMapping is supported
        final RequestMapping requestMapping = apiDefinition.getAnnotation(RequestMapping.class);
        // rewrite api path
        apiDefinition.setMethodPath(getPath(requestMapping));
        
        // Get additional values from the annotation.
        // TO_DO : Provides support annotation extensions
    }
    
    /**
     * Add supported class annotations.
     *
     * @param annotation annotation
     */
    public void addSupportedApiAnnotations(final Class<? extends Annotation> annotation) {
        supportedApiAnnotations.add(annotation);
    }
    
    /**
     * Add supported method annotations.
     *
     * @param annotation annotation
     */
    public void addSupportedApiDefinitionAnnotations(final Class<? extends Annotation> annotation) {
        supportedApiDefinitionAnnotations.add(annotation);
    }
    
    private String getPath(@NonNull final RequestMapping requestMapping) {
        if (ArrayUtils.isEmpty(requestMapping.path())) {
            return "";
        }
        return requestMapping.path()[0];
    }
    
    @NotNull
    @Override
    protected List<Class<? extends Annotation>> supportedApiAnnotations() {
        return supportedApiAnnotations;
    }
    
    @NotNull
    @Override
    protected List<Class<? extends Annotation>> supportedApiDefinitionAnnotations() {
        return supportedApiDefinitionAnnotations;
    }
}
