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

package org.apache.shenyu.sdk.starter.core.annotation;

import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.starter.core.factory.AnnotatedParameterProcessor;
import org.springframework.web.bind.annotation.RequestParam;

import java.lang.annotation.Annotation;

/**
 * {@link RequestParam} parameter processor.
 */
public class RequestParamParameterProcessor implements AnnotatedParameterProcessor {

    private static final Class<RequestParam> ANNOTATION = RequestParam.class;

    @Override
    public Class<? extends Annotation> getAnnotationType() {
        return ANNOTATION;
    }

    @Override
    public boolean processArgument(final RequestTemplate requestTemplate, final Annotation annotation, final Object arg) {
//        int parameterIndex = context.getParameterIndex();
//        Class<?> parameterType = method.getParameterTypes()[parameterIndex];
//        MethodMetadata data = context.getMethodMetadata();
//
//        if (Map.class.isAssignableFrom(parameterType)) {
//            checkState(data.queryMapIndex() == null, "Query map can only be present once.");
//            data.queryMapIndex(parameterIndex);
//
//            return true;
//        }
//
//        RequestParam requestParam = ANNOTATION.cast(annotation);
//        String name = requestParam.value();
//        checkState(emptyToNull(name) != null, "RequestParam.value() was empty on parameter %s", parameterIndex);
//        context.setParameterName(name);
//
//        Collection<String> query = context.setTemplateParameter(name, data.template().queries().get(name));
//        data.template().query(name, query);
        return true;
    }

}
