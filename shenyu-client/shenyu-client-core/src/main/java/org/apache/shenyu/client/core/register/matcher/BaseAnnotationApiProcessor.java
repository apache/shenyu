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
 * BaseAnnotationApiProcessor.<br>
 * API processor that supports annotations.
 */
public abstract class BaseAnnotationApiProcessor<T extends Annotation> implements ApiAnnotationProcessor<T>, ApiRegisterProcessor {
    
    @Override
    public void process(final ApiBean apiBean) {
        final T annotation = apiBean.getAnnotation(matchAnnotation());
        if (match(apiBean) && Objects.nonNull(annotation)) {
            process(apiBean, annotation);
        }
        for (ApiBean.ApiDefinition definition : apiBean.getApiDefinitions()) {
            final T definitionAnnotation = definition.getAnnotation(matchAnnotation());
            if (match(definition) && Objects.nonNull(definitionAnnotation)) {
                process(definition, definitionAnnotation);
            }
        }
    }
    
    @Override
    public boolean match(final ApiBean element) {
        return false;
    }
    
    /**
     * match.
     *
     * @param definition definition
     * @return true
     */
    public boolean match(final ApiBean.ApiDefinition definition) {
        return false;
    }
}
