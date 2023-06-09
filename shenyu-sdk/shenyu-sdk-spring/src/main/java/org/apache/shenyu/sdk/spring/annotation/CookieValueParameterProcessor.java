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
import org.springframework.http.HttpHeaders;
import org.springframework.web.bind.annotation.CookieValue;

import java.lang.annotation.Annotation;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import static com.google.common.base.Strings.emptyToNull;
import static org.apache.shenyu.sdk.core.util.Util.checkState;


/**
 * annotation processor.
 */
public class CookieValueParameterProcessor implements AnnotatedParameterProcessor {

    private static final Class<CookieValue> ANNOTATION = CookieValue.class;

    @Override
    public Class<? extends Annotation> getAnnotationType() {
        return ANNOTATION;
    }

    @Override
    public boolean processArgument(final ShenyuRequest shenyuRequest, final Annotation annotation, final Object arg) {
        RequestTemplate requestTemplate = shenyuRequest.getRequestTemplate();
        CookieValue cookie = ANNOTATION.cast(annotation);
        String name = cookie.value().trim();
        checkState(emptyToNull(name) != null, "Cookie.name() was empty on parameter %s", requestTemplate.getMethod());
        Collection<String> cookieExpression = requestTemplate.getHeaders().getOrDefault(HttpHeaders.COOKIE, new ArrayList<>());
        cookieExpression.add(String.format("%s=%s", name, arg));
        Map<String, Collection<String>> headers = shenyuRequest.getHeaders();
        headers.remove(HttpHeaders.COOKIE);
        headers.put(HttpHeaders.COOKIE, cookieExpression);
        shenyuRequest.setHeaders(headers);
        return true;
    }

}
