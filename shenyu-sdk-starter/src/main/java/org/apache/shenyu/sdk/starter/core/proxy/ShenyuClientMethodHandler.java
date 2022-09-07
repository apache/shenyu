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

package org.apache.shenyu.sdk.starter.core.proxy;

import org.apache.shenyu.sdk.starter.core.RequestTemplate;
import org.apache.shenyu.sdk.starter.core.ShenyuClient;
import org.apache.shenyu.sdk.starter.core.ShenyuHttpClient;
import org.apache.shenyu.sdk.starter.core.ShenyuRequest;
import org.apache.shenyu.sdk.starter.core.factory.RequestPostProcessor;

import java.io.IOException;
import java.lang.reflect.Method;
import java.util.Collection;

/**
 * ShenyuClientMethodHandler.
 */
public class ShenyuClientMethodHandler {

    private final Class<?> returnType;

    private final ShenyuClient shenyuClient;

    private final ShenyuHttpClient shenyuHttpClient;

    private final Method method;

    private RequestTemplate requestTemplate;

    private final Collection<RequestPostProcessor> requestPostProcessors;

    public ShenyuClientMethodHandler(final ShenyuClient shenyuClient, final ShenyuHttpClient shenyuHttpClient,
                                     final Method method, final RequestTemplate requestTemplate,
                                     final Collection<RequestPostProcessor> requestPostProcessors) {
        this.returnType = method.getReturnType();
        this.shenyuHttpClient = shenyuHttpClient;
        this.shenyuClient = shenyuClient;
        this.method = method;
        this.requestTemplate = requestTemplate;
        this.requestPostProcessors = requestPostProcessors;
    }

    /**
     * invoke.
     *
     * @param args args
     * @return {@link Object}
     * @throws IOException err
     */
    public Object invoke(final Object[] args) throws IOException {

        for (RequestPostProcessor requestPostProcessor : requestPostProcessors) {
            requestTemplate = requestPostProcessor.postProcessor(requestTemplate, args);
        }
        final ShenyuRequest shenyuRequest = requestTemplate.request();
        shenyuHttpClient.execute(shenyuRequest);
        throw new IOException("请求失败！");
    }

}
