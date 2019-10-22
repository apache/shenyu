/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.plugins.http.hystrix;

import com.netflix.hystrix.HystrixCommand;
import com.netflix.hystrix.exception.HystrixRuntimeException;
import com.netflix.hystrix.exception.HystrixTimeoutException;
import org.asynchttpclient.AsyncHttpClient;
import org.asynchttpclient.BoundRequestBuilder;
import org.asynchttpclient.DefaultAsyncHttpClient;
import org.asynchttpclient.DefaultAsyncHttpClientConfig;
import org.asynchttpclient.Response;
import org.dromara.plugins.api.dto.SoulRequest;
import org.dromara.soul.common.http.HttpMethod;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.concurrent.CompletableFuture;

/**
 * The type Http command.
 *
 * @author xiaoyu
 */
public class HttpCommand extends HystrixCommand<Object> {

    private static final Logger LOGGER = LoggerFactory.getLogger(HttpCommand.class);

    private static final AsyncHttpClient asyncHttpClient =
            new DefaultAsyncHttpClient(new DefaultAsyncHttpClientConfig.Builder()
                    .setConnectTimeout(3000)
                    .setRequestTimeout(3000)
                    .build());

    private SoulRequest soulRequest;

    /**
     * Instantiates a new Http command.
     *
     * @param setter      the setter
     * @param soulRequest the soul request
     */
    public HttpCommand(final Setter setter, final SoulRequest soulRequest) {
        super(setter);
        this.soulRequest = soulRequest;
    }

    @Override
    protected Object getFallback() {
        if (isFailedExecution()) {
            LOGGER.error("http execute have error:", getExecutionException());
        }
        final Throwable exception = getExecutionException();
        if (exception instanceof HystrixRuntimeException) {
            HystrixRuntimeException e = (HystrixRuntimeException) getExecutionException();
            if (e.getFailureType() == HystrixRuntimeException.FailureType.TIMEOUT) {

            } else {

            }
        } else if (exception instanceof HystrixTimeoutException) {

        } else {

        }
        return "";
    }

    @Override
    protected Object run() {
        BoundRequestBuilder boundRequestBuilder;
        if (soulRequest.getHttpMethod() == HttpMethod.GET) {
            boundRequestBuilder = asyncHttpClient.prepareGet(soulRequest.getUrl());
        } else if (soulRequest.getHttpMethod() == HttpMethod.POST) {
            boundRequestBuilder = asyncHttpClient.preparePost(soulRequest.getUrl());
            boundRequestBuilder.setBody(soulRequest.getBody());
        } else {
            boundRequestBuilder = asyncHttpClient.preparePut(soulRequest.getUrl());
        }
        CompletableFuture<Response> whenResponse = boundRequestBuilder
                .setSingleHeaders(soulRequest.getHeaders())
                .execute()
                .toCompletableFuture()
                .thenApply(response -> response);
        return whenResponse.join().getResponseBody();

    }
}
