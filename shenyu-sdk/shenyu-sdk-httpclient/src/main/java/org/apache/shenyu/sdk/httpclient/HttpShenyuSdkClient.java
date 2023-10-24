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
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.methods.RequestBuilder;
import org.apache.http.concurrent.FutureCallback;
import org.apache.http.config.Registry;
import org.apache.http.config.RegistryBuilder;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.nio.client.CloseableHttpAsyncClient;
import org.apache.http.impl.nio.client.HttpAsyncClients;
import org.apache.http.impl.nio.conn.PoolingNHttpClientConnectionManager;
import org.apache.http.impl.nio.reactor.DefaultConnectingIOReactor;
import org.apache.http.nio.client.HttpAsyncClient;
import org.apache.http.nio.conn.NoopIOSessionStrategy;
import org.apache.http.nio.conn.SchemeIOSessionStrategy;
import org.apache.http.nio.conn.ssl.SSLIOSessionStrategy;
import org.apache.http.nio.reactor.ConnectingIOReactor;
import org.apache.http.util.EntityUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.sdk.core.ShenyuRequest;
import org.apache.shenyu.sdk.core.ShenyuResponse;
import org.apache.shenyu.sdk.core.client.AbstractShenyuSdkClient;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

/**
 * shenyu httpclient.
 */
@Join
public class HttpShenyuSdkClient extends AbstractShenyuSdkClient {

    private static final Logger LOG = LoggerFactory.getLogger(HttpShenyuSdkClient.class);

    private PoolingNHttpClientConnectionManager connectionManager;

    private RequestConfig requestConfig;

    private HttpAsyncClient httpAsyncClient;

    @Override
    protected void initClient(final Properties props) {
        try {
            final String maxTotal = props.getProperty("http.maxTotal", "800");
            final String maxPerRoute = props.getProperty("http.maxPerRoute", "200");
            final String serverRequestTimeOut = props.getProperty("http.serverRequestTimeOut", "2000");
            final String serverResponseTimeOut = props.getProperty("http.serverResponseTimeOut", "2000");
            final String connectionRequestTimeOut = props.getProperty("http.connectionRequestTimeOut ", "2000");
            Registry<SchemeIOSessionStrategy> sessionStrategyRegistry = RegistryBuilder.<SchemeIOSessionStrategy>create()
                    .register("https", SSLIOSessionStrategy.getDefaultStrategy())
                    .register("http", NoopIOSessionStrategy.INSTANCE)
                    .build();
            ConnectingIOReactor ioReactor = new DefaultConnectingIOReactor();
            this.connectionManager = new PoolingNHttpClientConnectionManager(ioReactor, sessionStrategyRegistry);
            connectionManager.setMaxTotal(Integer.parseInt(maxTotal));
            connectionManager.setDefaultMaxPerRoute(Integer.parseInt(maxPerRoute));
            this.requestConfig = RequestConfig.custom()
                    .setSocketTimeout(Integer.parseInt(serverRequestTimeOut))
                    .setConnectTimeout(Integer.parseInt(serverResponseTimeOut))
                    .setConnectionRequestTimeout(Integer.parseInt(connectionRequestTimeOut))
                    .build();
            this.httpAsyncClient = getHttpClient();
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    private HttpAsyncClient getHttpClient() {
        CloseableHttpAsyncClient client = HttpAsyncClients.custom().setDefaultRequestConfig(requestConfig)
                .setConnectionManager(connectionManager)
                .build();
        client.start();
        return client;
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
        Future<HttpResponse> execute = httpAsyncClient.execute(requestBuilder.build(), new FutureCallback<HttpResponse>() {
            @Override
            public void completed(final HttpResponse response) {
                LOG.debug("HttpResponse completed statusLine={}", response.getStatusLine());
            }

            @Override
            public void failed(final Exception ex) {
                LOG.error("HttpResponse failed", ex);
            }

            @Override
            public void cancelled() {
                LOG.debug("HttpResponse cancelled.");
            }
        });
        try {
            HttpResponse response = execute.get();
            return new ShenyuResponse(response.getStatusLine().getStatusCode(), response.getStatusLine().getReasonPhrase(),
                    Arrays.stream(response.getAllHeaders()).collect(Collectors.groupingBy(Header::getName, HashMap::new,
                            Collectors.mapping(Header::getValue, Collectors.toCollection(LinkedList::new)))),
                    EntityUtils.toString(response.getEntity(), StandardCharsets.UTF_8), request);
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    private StringEntity createStringEntity(final String body) {
        StringEntity stringEntity = new StringEntity(body, StandardCharsets.UTF_8);
        stringEntity.setContentType("application/json;charset=UTF-8");
        return stringEntity;
    }
}
