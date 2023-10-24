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

package org.apache.shenyu.sdk.core;

import org.apache.shenyu.sdk.core.common.RequestTemplate;

import java.io.Serializable;
import java.util.Collection;
import java.util.Map;

import static com.google.common.base.Preconditions.checkNotNull;

/**
 * An immutable request to an http server.
 */
public final class ShenyuRequest implements Serializable {

    private final HttpMethod httpMethod;

    private String url;

    private final String name;

    private Map<String, Collection<String>> headers;

    private String body;

    private final RequestTemplate requestTemplate;

    /**
     * Creates a new Request.
     *
     * @param method of the request.
     * @param url for the request.
     * @param headers for the request.
     * @param body for the request, optional.
     * @param requestTemplate used to build the request.
     */
    ShenyuRequest(final HttpMethod method,
                  final String url,
                  final Map<String, Collection<String>> headers,
                  final String body,
                  final String name,
                  final RequestTemplate requestTemplate) {
        this.httpMethod = checkNotNull(method, "httpMethod of %s", method.name());
        this.url = checkNotNull(url, "url");
        this.headers = checkNotNull(headers, "headers of %s %s", method, url);
        this.body = body;
        this.requestTemplate = requestTemplate;
        this.name = name;
    }

    public enum HttpMethod {

        /**
         * HttpMethod.
         */
        GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE, PATCH
    }

    /**
     * Builds a Request. All parameters must be effectively immutable, via safe copies.
     *
     * @param httpMethod for the request.
     * @param url for the request.
     * @param headers to include.
     * @param body of the request, can be {@literal null}
     * @param requestTemplate requestTemplate
     * @param name contextId
     * @return a Request
     */
    public static ShenyuRequest create(final HttpMethod httpMethod,
                                       final String url,
                                       final Map<String, Collection<String>> headers,
                                       final String body,
                                       final String name,
                                       final RequestTemplate requestTemplate) {
        return new ShenyuRequest(httpMethod, url, headers, body, name, requestTemplate);
    }

    /**
     * Builds a Request. All parameters must be effectively immutable, via safe copies.
     *
     * @param url for the request.
     * @param request to include.
     * @return a Request
     */
    public static ShenyuRequest create(final String url, final ShenyuRequest request) {
        return new ShenyuRequest(request.getHttpMethod(), url, request.getHeaders(), request.getBody(), request.getName(), request.getRequestTemplate());
    }

    /**
     * getHttpMethod.
     *
     * @return {@link HttpMethod}
     */
    public HttpMethod getHttpMethod() {
        return httpMethod;
    }

    /**
     * getUrl.
     *
     * @return {@link String}
     */
    public String getUrl() {
        return url;
    }

    /**
     * getHeaders.
     *
     * @return {@link Map}
     */
    public Map<String, Collection<String>> getHeaders() {
        return headers;
    }

    /**
     * getBody.
     *
     * @return {@link String}
     */
    public String getBody() {
        return body;
    }

    /**
     * getRequestTemplate.
     *
     * @return {@link RequestTemplate}
     */
    public RequestTemplate getRequestTemplate() {
        return requestTemplate;
    }

    /**
     * getName.
     *
     * @return {@link String}
     */
    public String getName() {
        return name;
    }

    /**
     * setUrl.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * setHeaders.
     *
     * @param headers headers
     */
    public void setHeaders(final Map<String, Collection<String>> headers) {
        this.headers = headers;
    }

    /**
     * setBody.
     *
     * @param body body
     */
    public void setBody(final String body) {
        this.body = body;
    }
}
