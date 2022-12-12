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

package org.apache.shenyu.sdk.spring.annotation;

import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import org.springframework.web.bind.annotation.RequestParam;

import java.lang.annotation.Annotation;
import java.util.Map;

import static com.google.common.base.Strings.emptyToNull;
import static org.apache.shenyu.sdk.core.util.Util.checkState;

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
    public boolean processArgument(final ShenyuRequest shenyuRequest, final Annotation annotation, final Object arg) {
        RequestTemplate requestTemplate = shenyuRequest.getRequestTemplate();
        RequestParam requestParam = ANNOTATION.cast(annotation);
        String name = requestParam.value();
        checkState(emptyToNull(name) != null, "RequestParam.value() was empty on parameter %s#%s",
                requestTemplate.getMethod().getDeclaringClass().getSimpleName(), requestTemplate.getMethod().getName());
        StringBuilder pathResult = new StringBuilder(requestTemplate.getPath());
        if (arg instanceof String) {
            if (pathResult.indexOf("?") > 0) {
                pathResult.append("&");
            } else {
                pathResult.append("?");
            }
            pathResult.append(name).append("=").append(arg);
        } else if (arg instanceof Map) {
            ((Map<?, ?>) arg).forEach((key, value) -> {
                pathResult.append(key).append("=").append(value);
            });
        }
        shenyuRequest.setUrl(requestTemplate.getUrl() + pathResult);
        return true;
    }

}
