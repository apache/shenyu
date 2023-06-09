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

package org.apache.shenyu.sdk.spring.proxy;

import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.apache.shenyu.sdk.core.client.ShenyuSdkClient;
import org.apache.shenyu.sdk.core.common.RequestTemplate;
import org.apache.shenyu.sdk.spring.ShenyuClient;
import org.apache.shenyu.sdk.spring.factory.AnnotatedParameterProcessor;
import org.springframework.http.HttpStatus;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;
import org.springframework.web.client.HttpClientErrorException;

import java.io.IOException;
import java.lang.annotation.Annotation;
import java.util.Map;

/**
 * ShenyuClientMethodHandler.
 */
public class ShenyuClientMethodHandler {

    private final ShenyuClient shenyuClient;

    private final ShenyuSdkClient shenyuHttpClient;

    private RequestTemplate requestTemplate;

    private final Map<Class<? extends Annotation>, AnnotatedParameterProcessor> annotatedArgumentProcessors;

    public ShenyuClientMethodHandler(final ShenyuClient shenyuClient,
                                     final RequestTemplate requestTemplate,
                                     final ShenyuSdkClient shenyuHttpClient,
                                     final Map<Class<? extends Annotation>, AnnotatedParameterProcessor> annotatedArgumentProcessors) {
        this.shenyuClient = shenyuClient;
        this.requestTemplate = requestTemplate;
        this.shenyuHttpClient = shenyuHttpClient;
        this.annotatedArgumentProcessors = annotatedArgumentProcessors;
    }

    /**
     * invoke.
     *
     * @param args args
     * @return {@link Object}
     * @throws IOException err
     */
    public Object invoke(final Object[] args) throws IOException {
        final ShenyuRequest shenyuRequest = targetProcessor(requestTemplate, args);
        final ShenyuResponse shenyuResponse = shenyuHttpClient.execute(shenyuRequest);
        return handlerResponse(shenyuResponse, shenyuRequest.getRequestTemplate().getReturnType());
    }

    private Object handlerResponse(final ShenyuResponse shenyuResponse, final Class<?> returnType) {
        if (shenyuResponse == null || void.class == returnType) {
            return null;
        } else if (ShenyuResponse.class == returnType) {
            return shenyuResponse;
        } else if (shenyuResponse.getStatus() != HttpStatus.OK.value()) {
            throw new HttpClientErrorException(HttpStatus.valueOf(shenyuResponse.getStatus()));
        } else if (StringUtils.hasText(shenyuResponse.getBody())) {
            return JsonUtils.jsonToObject(shenyuResponse.getBody(), returnType);
        } else {
            return null;
        }
    }

    private ShenyuRequest targetProcessor(final RequestTemplate requestTemplate, final Object[] args) {
        final RequestTemplate requestTemplateFrom = RequestTemplate.from(requestTemplate);
        ShenyuRequest request = requestTemplateFrom.request();
        for (RequestTemplate.ParamMetadata paramMetadata : requestTemplateFrom.getParamMetadataList()) {
            final Annotation[] paramAnnotations = paramMetadata.getParamAnnotations();
            for (Annotation paramAnnotation : paramAnnotations) {
                final AnnotatedParameterProcessor processor = annotatedArgumentProcessors.get(paramAnnotation.annotationType());
                if (ObjectUtils.isEmpty(processor)) {
                    continue;
                }
                processor.processArgument(request, paramAnnotation, args[paramMetadata.getParamIndexOnMethod()]);
            }
        }
        return request;
    }
}
