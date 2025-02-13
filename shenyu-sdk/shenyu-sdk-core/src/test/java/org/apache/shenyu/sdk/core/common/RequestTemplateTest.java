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

package org.apache.shenyu.sdk.core.common;

import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Test for {@link RequestTemplate}.
 */
public class RequestTemplateTest {

    private RequestTemplate requestTemplate;

    @Before
    public void setUp() throws NoSuchMethodException {
        requestTemplate = new RequestTemplate();

        final Method method = createMethod();
        requestTemplate.setReturnType(method.getReturnType());
        requestTemplate.setMethod(method);

        requestTemplate.setName("testClientName");
        requestTemplate.setUrl("/url/a/b");
        requestTemplate.setPath("/id/{id}");
        // if there is shenyuClientFactoryBean path not null, template should append clientFactoryBean path
        // if (StringUtils.hasText(shenyuClientFactoryBean.getPath())) {
        //     requestTemplate.setPath(shenyuClientFactoryBean.getPath() + requestTemplate.getPath());
        // }

        requestTemplate.setContextId(UUIDUtils.getInstance().generateShortUuid());
        requestTemplate.setHttpMethod(ShenyuRequest.HttpMethod.GET);

        Map<String, Collection<String>> headerMap = new HashMap<>();
        headerMap.put("header", Arrays.asList("header1", "header2"));
        String body = "{key1:\"value1\"}";
        requestTemplate.setHeaders(headerMap);
        requestTemplate.setBody(body);

        requestTemplate.setParamMetadataList(analysisParamMetadata(method));

    }

    @Test
    public void testFrom() {
        assertNotNull(requestTemplate);
        RequestTemplate newRequestTemplate = RequestTemplate.from(requestTemplate);
        assertNotNull(newRequestTemplate);
    }

    @Test
    public void testRequest() {
        assertNotNull(requestTemplate);
        ShenyuRequest shenyuRequest = requestTemplate.request();
        Assert.assertNotNull(shenyuRequest);
    }

    public void assertNotNull(final RequestTemplate requestTemplate) {
        Assert.assertNotNull(requestTemplate);
        Assert.assertNotNull(requestTemplate.getReturnType());
        Assert.assertNotNull(requestTemplate.getMethod());
        Assert.assertNotNull(requestTemplate.getName());
        Assert.assertNotNull(requestTemplate.getUrl());
        Assert.assertNotNull(requestTemplate.getPath());
        Assert.assertNotNull(requestTemplate.getContextId());
        Assert.assertNotNull(requestTemplate.getHttpMethod());
        Assert.assertNotNull(requestTemplate.getHeaders());
        Assert.assertNotNull(requestTemplate.getBody());
        Assert.assertNotNull(requestTemplate.getParamMetadataList());
    }

    /**
     * analysisParamMetadata.
     *
     * @param method method
     * @return {@link List}
     */
    public static List<RequestTemplate.ParamMetadata> analysisParamMetadata(final Method method) {
        Parameter[] parameters = method.getParameters();
        if (Objects.isNull(parameters) || parameters.length == 0) {
            return Collections.emptyList();
        }
        List<RequestTemplate.ParamMetadata> params = new ArrayList<>(parameters.length);
        for (int index = 0; index < parameters.length; index++) {
            Annotation[] annotations = parameters[index].getAnnotations();
            if (Objects.isNull(annotations) || annotations.length == 0) {
                continue;
            }
            params.add(new RequestTemplate.ParamMetadata(annotations, parameters[index].getType(), index));
        }
        return params;
    }

    private static Method createMethod() throws NoSuchMethodException {
        return Controller.class.getDeclaredMethod("getControllerMethod");
    }

    static class Controller {
        private Object controllerMethod;

        public Object getControllerMethod() {
            return controllerMethod;
        }

    }

}
