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

package org.apache.shenyu.register.client.polaris.common;

import org.apache.hc.core5.http.Header;
import org.apache.hc.core5.http.NameValuePair;
import org.apache.shenyu.register.client.polaris.model.ResponseResult;
import org.apache.hc.client5.http.classic.methods.HttpGet;
import org.apache.hc.client5.http.classic.methods.HttpPost;
import org.apache.hc.client5.http.classic.methods.HttpPut;
import org.apache.hc.client5.http.config.RequestConfig;
import org.apache.hc.client5.http.impl.classic.CloseableHttpClient;
import org.apache.hc.client5.http.impl.classic.HttpClientBuilder;
import org.apache.hc.core5.http.Method;
import org.apache.hc.core5.http.io.entity.EntityUtils;
import org.apache.hc.core5.http.io.entity.StringEntity;
import org.apache.hc.core5.http.message.BasicClassicHttpRequest;
import org.apache.hc.core5.net.URIBuilder;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * HttpOperator.
 */
public class HttpOperator {

    private static final int DEFAULT_HTTP_TIMEOUT = 5000;

    private final CloseableHttpClient httpClient;

    public HttpOperator() {
        this.httpClient = HttpClientBuilder.create()
                .setDefaultRequestConfig(RequestConfig.custom()
                        .setConnectionRequestTimeout(DEFAULT_HTTP_TIMEOUT, TimeUnit.MILLISECONDS)
                        .setResponseTimeout(DEFAULT_HTTP_TIMEOUT, TimeUnit.MILLISECONDS).build())
                .build();
    }

    /**
     * send api.
     *
     * @param request request
     * @return ResponseResult
     * @throws IOException IOException
     */
    public ResponseResult send(final BasicClassicHttpRequest request) throws IOException {

        return httpClient.execute(request, response -> {
            ResponseResult responseResult = new ResponseResult();
            responseResult.setStatus(response.getCode());
            responseResult.setData(EntityUtils.toString(response.getEntity()));
            response.close();
            return responseResult;
        });
    }

    /**
     * build request on body.
     *
     * @param url     url
     * @param method  method
     * @param headers headers
     * @param body    body
     * @return BasicClassicHttpRequest
     * @throws IOException IOException
     */
    public BasicClassicHttpRequest buildReqOnBody(final String url, final Method method, final List<Header> headers, final String body) throws IOException {
        URI uri = URI.create(url);

        if (Method.POST.equals(method)) {
            HttpPost httpPost = new HttpPost(uri);
            for (Header header : headers) {
                httpPost.setHeader(header);
            }
            httpPost.setEntity(new StringEntity(body));
            return httpPost;
        }

        if (Method.PUT.equals(method)) {
            HttpPut httpPut = new HttpPut(uri);
            for (Header header : headers) {
                httpPut.setHeader(header);
            }
            httpPut.setEntity(new StringEntity(body));
            return httpPut;
        }
        throw new UnsupportedOperationException("Unsupported request method");
    }

    /**
     * build request on url param.
     *
     * @param url     url
     * @param method  method
     * @param headers headers
     * @param param   param
     * @return BasicClassicHttpRequest
     * @throws URISyntaxException URISyntaxException
     */
    public BasicClassicHttpRequest buildReqOnUrlParam(final String url,
                                                      final Method method,
                                                      final List<Header> headers,
                                                      final List<NameValuePair> param) throws URISyntaxException {

        URIBuilder uriBuilder = new URIBuilder(url);
        URI build = uriBuilder.addParameters(param).build();
        if (Method.GET.equals(method)) {
            HttpGet httpGet = new HttpGet(build);
            for (Header header : headers) {
                httpGet.setHeader(header);
            }
            return httpGet;
        }
        throw new UnsupportedOperationException("Unsupported request method");
    }

}
