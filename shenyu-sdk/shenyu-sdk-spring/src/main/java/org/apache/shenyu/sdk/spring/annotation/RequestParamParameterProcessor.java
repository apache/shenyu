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

import com.google.common.collect.Maps;
import java.lang.annotation.Annotation;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import static org.apache.shenyu.sdk.core.util.Util.checkState;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.multipart.MultipartFile;

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
        checkState(StringUtils.isNotBlank(name) || arg instanceof Map, "RequestParam.value() was empty on parameter %s#%s",
            requestTemplate.getMethod().getDeclaringClass().getSimpleName(), requestTemplate.getMethod().getName());
        StringBuilder pathResult = new StringBuilder(requestTemplate.getPath());
        Map<Object, Object> params = Maps.newHashMap();
        if (!(arg instanceof Map) && !(arg instanceof MultipartFile)) {
            params.put(name, arg);
        } else if (arg instanceof Map) {
            params = (Map<Object, Object>) arg;
        }
        params.forEach((key, value) -> {
            if (pathResult.indexOf("?") > 0) {
                pathResult.append("&");
            } else {
                pathResult.append("?");
            }
            pathResult.append(key).append("=").append(value);
        });
        shenyuRequest.setUrl(requestTemplate.getUrl() + pathResult);
        return true;
    }

}
