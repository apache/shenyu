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

package org.apache.shenyu.sdk.starter.core.factory;

import org.apache.shenyu.sdk.starter.core.RequestTemplate;
import org.apache.shenyu.sdk.starter.core.util.Util;
import org.springframework.context.ResourceLoaderAware;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.apache.shenyu.sdk.starter.core.util.Util.checkState;


/**
 * Defines what annotations and values are valid on interfaces.
 */
public interface Contract extends ResourceLoaderAware {

    /**
     * parseAndValidateMetadata.
     *
     * @param targetType targetType
     * @return {@link List}
     */
    List<RequestTemplate> parseAndValidateMetadata(Class<?> targetType);

    /**
     * BaseContract.
     */
    abstract class BaseContract implements Contract {

        @Override
        public List<RequestTemplate> parseAndValidateMetadata(final Class<?> targetType) {
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
                final RequestTemplate metadata = parseRequestTemplate(method);
                if (result.containsKey(method)) {
//                    RequestTemplate existingMetadata = result.get(metadata.configKey());
//                    Type existingReturnType = existingMetadata.returnType();
//                    Type overridingReturnType = metadata.returnType();
//                    Type resolvedType = Types.resolveReturnType(existingReturnType, overridingReturnType);
//                    if (resolvedType.equals(overridingReturnType)) {
//                        result.put(metadata.configKey(), metadata);
//                    }
//                    result.put(method, existingMetadata);
                    continue;
                }
                result.put(method, metadata);
            }
            return new ArrayList<>(result.values());
        }

        /**
         * parseRequestTemplate.
         *
         * @param method method
         * @return {@link RequestTemplate}
         */
        RequestTemplate parseRequestTemplate(final Method method) {
            return null;
        }
    }

}
