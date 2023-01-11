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

import org.apache.shenyu.sdk.core.ShenyuRequest;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Request Builder for an HTTP Target.
 * <p>
 * This class is a variation on a UriTemplate, where, in addition to the uri, Headers and Query
 * information also support template expressions.
 * </p>
 */
public final class RequestTemplate {

    private transient Class<?> returnType;

    private transient Method method;

    private String name;

    private String url;

    private String contextId;

    private String path;

    private ShenyuRequest.HttpMethod httpMethod;

    private List<ParamMetadata> paramMetadataList;

    private Map<String, Collection<String>> headers = new HashMap<>();

    private String body;

    public RequestTemplate() {
    }

    public RequestTemplate(final Class<?> returnType, final Method method, final String name, final String url, final String contextId,
                           final String path, final ShenyuRequest.HttpMethod httpMethod, final List<ParamMetadata> paramMetadataList,
                           final Map<String, Collection<String>> headers, final String body) {
        this.returnType = returnType;
        this.method = method;
        this.name = name;
        this.url = url;
        this.contextId = contextId;
        this.path = path;
        this.httpMethod = httpMethod;
        this.paramMetadataList = paramMetadataList;
        this.headers = headers;
        this.body = body;
    }

    /**
     * request.
     *
     * @return {@link ShenyuRequest}
     */
    public ShenyuRequest request() {
        return ShenyuRequest.create(this.httpMethod, this.url + this.path, new HashMap<>(this.headers), this.body, name, this);
    }

    /**
     * Create a Request Template from an existing Request Template.
     *
     * @param requestTemplate to copy from.
     * @return a new Request Template.
     */
    public static RequestTemplate from(final RequestTemplate requestTemplate) {
        return new RequestTemplate(
                requestTemplate.getReturnType(),
                requestTemplate.getMethod(),
                requestTemplate.getName(),
                requestTemplate.getUrl(),
                requestTemplate.getContextId(),
                requestTemplate.getPath(),
                requestTemplate.getHttpMethod(),
                requestTemplate.getParamMetadataList(),
                requestTemplate.getHeaders(),
                requestTemplate.getBody());
    }

    /**
     * url.
     *
     * @return Url
     */
    public String getUrl() {
        return url;
    }

    /**
     * set url.
     *
     * @param url url
     */
    public void setUrl(final String url) {
        this.url = url;
    }

    /**
     * path.
     *
     * @return Path
     */
    public String getPath() {
        return path;
    }

    /**
     * set path.
     *
     * @param path path
     */
    public void setPath(final String path) {
        this.path = path;
    }

    /**
     * httpMethod.
     *
     * @return HttpMethod
     */
    public ShenyuRequest.HttpMethod getHttpMethod() {
        return httpMethod;
    }

    /**
     * set httpMethod.
     *
     * @param httpMethod httpMethod
     */
    public void setHttpMethod(final ShenyuRequest.HttpMethod httpMethod) {
        this.httpMethod = httpMethod;
    }

    /**
     * paramMetadataList.
     *
     * @return ParamMetadataList
     */
    public List<ParamMetadata> getParamMetadataList() {
        return paramMetadataList;
    }

    /**
     * set paramMetadataList.
     *
     * @param paramMetadataList paramMetadataList
     */
    public void setParamMetadataList(final List<ParamMetadata> paramMetadataList) {
        this.paramMetadataList = paramMetadataList;
    }

    /**
     * headers.
     *
     * @return Headers
     */
    public Map<String, Collection<String>> getHeaders() {
        return headers;
    }

    /**
     * set headers.
     *
     * @param headers headers
     */
    public void setHeaders(final Map<String, Collection<String>> headers) {
        this.headers = headers;
    }

    /**
     * body.
     *
     * @return Body
     */
    public String getBody() {
        return body;
    }

    /**
     * set body.
     *
     * @param body body
     */
    public void setBody(final String body) {
        this.body = body;
    }

    /**
     * returnType.
     *
     * @return ReturnType
     */
    public Class<?> getReturnType() {
        return returnType;
    }

    /**
     * set returnType.
     *
     * @param returnType returnType
     */
    public void setReturnType(final Class<?> returnType) {
        this.returnType = returnType;
    }

    /**
     * method.
     *
     * @return Method
     */
    public Method getMethod() {
        return method;
    }

    /**
     * set method.
     *
     * @param method method
     */
    public void setMethod(final Method method) {
        this.method = method;
    }

    /**
     * contextId.
     *
     * @return ContextId
     */
    public String getContextId() {
        return contextId;
    }

    /**
     * set contextId.
     *
     * @param contextId contextId
     */
    public void setContextId(final String contextId) {
        this.contextId = contextId;
    }

    /**
     * name.
     *
     * @return Name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    public static class ParamMetadata {

        /**
         * paramAnnotations.
         */
        private final Annotation[] paramAnnotations;

        /**
         * paramType.
         */
        private final Class<?> paramType;

        /**
         * paramIndexOnMethod.
         */
        private final int paramIndexOnMethod;

        public ParamMetadata(final Annotation[] paramAnnotations, final Class<?> paramType, final int paramIndexOnMethod) {
            this.paramAnnotations = paramAnnotations;
            this.paramType = paramType;
            this.paramIndexOnMethod = paramIndexOnMethod;
        }

        /**
         * paramAnnotations.
         *
         * @return Annotation
         */
        public Annotation[] getParamAnnotations() {
            return paramAnnotations;
        }

        /**
         * paramType.
         *
         * @return Class
         */
        public Class<?> getParamType() {
            return paramType;
        }

        /**
         * paramIndexOnMethod.
         *
         * @return int
         */
        public int getParamIndexOnMethod() {
            return paramIndexOnMethod;
        }
    }
}
