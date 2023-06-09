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

package org.apache.shenyu.sdk.spring.factory;

import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.core.util.Types;
import org.apache.shenyu.sdk.core.util.Util;
import org.apache.shenyu.sdk.spring.ShenyuClientFactoryBean;
import org.springframework.context.ResourceLoaderAware;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Parameter;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.apache.shenyu.sdk.core.util.Util.checkState;


/**
 * Defines what annotations and values are valid on interfaces.
 */
public interface Contract extends ResourceLoaderAware {

    /**
     * parseAndValidateMetadata.
     *
     * @param targetType              targetType
     * @param shenyuClientFactoryBean shenyuClientFactoryBean
     * @return {@link List}
     */
    List<RequestTemplate> parseAndValidateRequestTemplate(Class<?> targetType, ShenyuClientFactoryBean shenyuClientFactoryBean);

    /**
     * BaseContract.
     */
    abstract class BaseContract implements Contract {

        @Override
        public List<RequestTemplate> parseAndValidateRequestTemplate(final Class<?> targetType, final ShenyuClientFactoryBean shenyuClientFactoryBean) {
            checkState(targetType.getTypeParameters().length == 0, "Parameterized types unsupported: %s",
                    targetType.getSimpleName());
            checkState(targetType.getInterfaces().length <= 1, "Only single inheritance supported: %s",
                    targetType.getSimpleName());
            final Map<Method, RequestTemplate> result = new LinkedHashMap<>();
            for (final Method method : targetType.getMethods()) {
                if (method.getDeclaringClass() == Object.class
                        || (method.getModifiers() & Modifier.STATIC) != 0
                        || Util.isDefault(method)) {
                    continue;
                }
                final RequestTemplate parseRequestTemplate = parseRequestTemplate(method, shenyuClientFactoryBean);

                // paramMetadata
                parseRequestTemplate.setParamMetadataList(analysisParamMetadata(method));

                if (result.containsKey(method)) {
                    RequestTemplate existingMetadata = result.get(method);
                    Type existingReturnType = existingMetadata.getReturnType();
                    Type overridingReturnType = parseRequestTemplate.getReturnType();
                    Type resolvedType = Types.resolveReturnType(existingReturnType, overridingReturnType);
                    if (resolvedType.equals(overridingReturnType)) {
                        result.put(method, parseRequestTemplate);
                    }
                    result.put(method, existingMetadata);
                    continue;
                }
                result.put(method, parseRequestTemplate);
            }
            return new ArrayList<>(result.values());
        }

        /**
         * analysisParamMetadata.
         *
         * @param method method
         * @return {@link List}
         */
        private List<RequestTemplate.ParamMetadata> analysisParamMetadata(final Method method) {
            Parameter[] parameters = method.getParameters();
            if (parameters == null || parameters.length == 0) {
                return Collections.emptyList();
            }
            List<RequestTemplate.ParamMetadata> params = new ArrayList<>(parameters.length);
            for (int index = 0; index < parameters.length; index++) {
                Annotation[] annotations = parameters[index].getAnnotations();
                if (annotations == null || annotations.length == 0) {
                    continue;
                }
                params.add(new RequestTemplate.ParamMetadata(annotations, parameters[index].getType(), index));
            }
            return params;
        }

        /**
         * parseRequestTemplate.
         *
         * @param method                  method
         * @param shenyuClientFactoryBean shenyuClientFactoryBean
         * @return {@link RequestTemplate}
         */
        public abstract RequestTemplate parseRequestTemplate(Method method, ShenyuClientFactoryBean shenyuClientFactoryBean);
    }

}
