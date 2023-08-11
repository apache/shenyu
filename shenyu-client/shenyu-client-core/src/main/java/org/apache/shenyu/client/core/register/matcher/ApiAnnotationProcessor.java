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

package org.apache.shenyu.client.core.register.matcher;

import org.apache.shenyu.client.core.register.ApiBean;

import java.lang.annotation.Annotation;
import java.util.Objects;

/**
 * ApiAnnotationProcessor.
 */
public interface ApiAnnotationProcessor<T extends Annotation> extends ApiBeanProcessor {
    
    /**
     * process.
     *
     * @param apiBean apiBean
     */
    @Override
    default void process(ApiBean apiBean) {
        T annotation = getAnnotation(apiBean);
        if (Objects.nonNull(annotation)) {
            process(apiBean, annotation);
        }
    }
    
    /**
     * process API Bean.
     *
     * @param apiBean    apiBean
     * @param annotation annotation
     */
    void process(ApiBean apiBean, T annotation);
    
    /**
     * process API.
     *
     * @param definition definition
     * @param annotation annotation
     */
    void process(ApiBean.ApiDefinition definition, T annotation);
    
    /**
     * process.
     *
     * @param definition definition
     */
    @Override
    default void process(ApiBean.ApiDefinition definition) {
        T annotation = getAnnotation(definition);
        if (Objects.nonNull(annotation)) {
            process(definition, annotation);
        }
    }
    
    /**
     * getAnnotation.
     *
     * @param apiBean apiBean
     * @return t
     */
    default T getAnnotation(ApiBean apiBean) {
        return apiBean.getAnnotation(matchAnnotation());
    }
    
    /**
     * getAnnotation.
     *
     * @param definition definition
     * @return Annotation
     */
    default T getAnnotation(ApiBean.ApiDefinition definition) {
        return definition.getAnnotation(matchAnnotation());
    }
    
    /**
     * support annotation.
     *
     * @return annotation
     */
    Class<T> matchAnnotation();
}
