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

package org.apache.shenyu.sdk.httpclient;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.Header;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.http.conn.HttpClientConnectionManager;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.apache.shenyu.sdk.core.http.AbstractShenyuHttpClient;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.LinkedList;
import java.util.stream.Collectors;

/**
 * shenyu httpclient.
 */
public class HttpShenyuSdkClient extends AbstractShenyuHttpClient {

    private final HttpClientConnectionManager connectionManager;

    public HttpShenyuSdkClient(final HttpClientConnectionManager connectionManager) {
        this.connectionManager = connectionManager;
    }

    private HttpClient getHttpClient() {
        return HttpClients.custom().setConnectionManager(connectionManager).build();
    }

    @Override
    protected ShenyuResponse doRequest(final ShenyuRequest request) throws IOException {
        String url = request.getUrl();
        String body = request.getBody();
        RequestBuilder requestBuilder;

        switch (request.getHttpMethod()) {
            case GET:
                requestBuilder = RequestBuilder.get(url);
                break;
            case HEAD:
                requestBuilder = RequestBuilder.head(url);
                break;
            case POST:
                requestBuilder = RequestBuilder.post(url);
                break;
            case PUT:
                requestBuilder = RequestBuilder.put(url);
                break;
            case DELETE:
                requestBuilder = RequestBuilder.delete(url);
                break;
            case OPTIONS:
                requestBuilder = RequestBuilder.options(url);
                break;
            case TRACE:
                requestBuilder = RequestBuilder.trace(url);
                break;
            default:
                requestBuilder = RequestBuilder.patch(url);
                break;
        }
        if (StringUtils.isNotBlank(body)) {
            requestBuilder.setEntity(createStringEntity(body));
        }

        Map<String, Collection<String>> headers = request.getHeaders();
        for (String name : headers.keySet()) {
            for (String value : headers.get(name)) {
                requestBuilder.addHeader(name, value);
            }
        }

        HttpResponse response = getHttpClient().execute(requestBuilder.build());
        return new ShenyuResponse(response.getStatusLine().getStatusCode(), response.getStatusLine().getReasonPhrase(),
                Arrays.stream(response.getAllHeaders()).collect(Collectors.groupingBy(Header::getName, HashMap::new,
                        Collectors.mapping(Header::getValue, Collectors.toCollection(LinkedList::new)))),
                EntityUtils.toString(response.getEntity(), StandardCharsets.UTF_8), request);
    }

    private StringEntity createStringEntity(final String body) {
        StringEntity stringEntity = new StringEntity(body, StandardCharsets.UTF_8);
        stringEntity.setContentType("application/json;charset=UTF-8");
        return stringEntity;
    }

}
