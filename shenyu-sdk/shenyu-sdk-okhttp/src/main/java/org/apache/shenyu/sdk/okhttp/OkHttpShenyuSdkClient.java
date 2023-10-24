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

package org.apache.shenyu.sdk.okhttp;

import okhttp3.ConnectionPool;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.RequestBody;
import okhttp3.Response;
import okhttp3.internal.Util;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.apache.shenyu.sdk.core.client.AbstractShenyuSdkClient;
import org.apache.shenyu.spi.Join;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * shenyu okhttp.
 */
@Join
public class OkHttpShenyuSdkClient extends AbstractShenyuSdkClient {
    
    private OkHttpClient okHttpClient;

    @Override
    protected void initClient(final Properties props) {
        final String maxIdleConnections = props.getProperty("http.maxIdleConnections", "200");
        final String keepAliveDuration = props.getProperty("http.keepAliveDuration", "2");
        final String connectTimeout = props.getProperty("http.connectTimeout", "60");
        final String readTimeout = props.getProperty("http.readTimeout", "60");
        this.okHttpClient = new OkHttpClient.Builder()
                .retryOnConnectionFailure(true)
                .connectionPool(pool(Integer.parseInt(maxIdleConnections), Long.parseLong(keepAliveDuration)))
                .connectTimeout(Long.parseLong(connectTimeout), TimeUnit.SECONDS)
                .readTimeout(Long.parseLong(readTimeout), TimeUnit.SECONDS)
                .build();
    }

    @Override
    protected ShenyuResponse doRequest(final ShenyuRequest request) throws IOException {
        String url = request.getUrl();
        String body = request.getBody();
        Map<String, Collection<String>> headers = request.getHeaders();
        Request.Builder builder = new Request.Builder().url(url);
        for (String name : headers.keySet()) {
            for (String value : headers.get(name)) {
                builder.addHeader(name, value);
            }
        }
        RequestBody requestBody = StringUtils.isNotBlank(request.getBody()) 
                ? RequestBody.create(MediaType.parse("application/json;charset=UTF-8"), body) 
                : Util.EMPTY_REQUEST;
        switch (request.getHttpMethod()) {
            case GET:
                builder = builder.get();
                break;
            case HEAD:
                builder = builder.head();
                break;
            case POST:
                builder = builder.post(requestBody);
                break;
            case PUT:
                builder = builder.put(requestBody);
                break;
            case DELETE:
                builder = builder.delete(requestBody);
                break;
            case OPTIONS:
                builder.method("OPTIONS", requestBody);
                break;
            case TRACE:
                builder.method("TRACE", requestBody);
                break;
            default:
                builder.patch(requestBody);
                break;
        }
        Request okhttpRequest = builder.build();
        try (Response okhttpResponse = okHttpClient
                .newCall(okhttpRequest)
                .execute()) {
            String bodyStr = okhttpResponse.body() == null ? null : okhttpResponse.body().string();
            return new ShenyuResponse(okhttpResponse.code(), null,
                    okhttpResponse.headers().names().stream().collect(Collectors.toMap(name -> name, name -> okhttpResponse.headers().values(name))),
                    bodyStr, request);
        }
    }

    private ConnectionPool pool(final int maxIdleConnections, final long keepAliveDuration) {
        return new ConnectionPool(maxIdleConnections, keepAliveDuration, TimeUnit.MINUTES);
    }
}
