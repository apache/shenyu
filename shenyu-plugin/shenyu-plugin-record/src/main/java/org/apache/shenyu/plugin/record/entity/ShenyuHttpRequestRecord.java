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

package org.apache.shenyu.plugin.record.entity;

import java.util.Map;

/**
 * ShenyuHttpRequestRecord.
 *
 * <p>Entity representing a recorded HTTP request and its response,
 * used for capture and later replay.</p>
 */
public class ShenyuHttpRequestRecord {

    private String taskId;

    private String traceId;

    private String method;

    private String requestUri;

    private String queryParams;

    private String requestBody;

    private Map<String, String> requestHeaders;

    private Integer status;

    private Map<String, String> responseHeaders;

    private String responseBody;

    /**
     * Get task id.
     *
     * @return the task id
     */
    public String getTaskId() {
        return taskId;
    }

    /**
     * Set task id.
     *
     * @param taskId the task id
     */
    public void setTaskId(final String taskId) {
        this.taskId = taskId;
    }

    /**
     * Set request body.
     *
     * @param requestBody the request body
     */
    public void setRequestBody(final String requestBody) {
        this.requestBody = requestBody;
    }

    /**
     * Set method.
     *
     * @param method the HTTP method
     */
    public void setMethod(final String method) {
        this.method = method;
    }

    /**
     * Set request uri.
     *
     * @param requestUri the request URI
     */
    public void setRequestUri(final String requestUri) {
        this.requestUri = requestUri;
    }

    /**
     * Set query params.
     *
     * @param queryParams the query parameters
     */
    public void setQueryParams(final String queryParams) {
        this.queryParams = queryParams;
    }

    /**
     * Set request headers.
     *
     * @param requestHeaders the request headers
     */
    public void setRequestHeaders(final Map<String, String> requestHeaders) {
        this.requestHeaders = requestHeaders;
    }

    /**
     * Set status.
     *
     * @param status the HTTP status code
     */
    public void setStatus(final Integer status) {
        this.status = status;
    }

    /**
     * Set response headers.
     *
     * @param responseHeaders the response headers
     */
    public void setResponseHeaders(final Map<String, String> responseHeaders) {
        this.responseHeaders = responseHeaders;
    }

    /**
     * Set response body.
     *
     * @param responseBody the response body
     */
    public void setResponseBody(final String responseBody) {
        this.responseBody = responseBody;
    }

    /**
     * Set trace id.
     *
     * @param traceId the trace id
     */
    public void setTraceId(final String traceId) {
        this.traceId = traceId;
    }

    /**
     * Get trace id.
     *
     * @return the trace id
     */
    public String getTraceId() {
        return traceId;
    }

    /**
     * Get method.
     *
     * @return the HTTP method
     */
    public String getMethod() {
        return method;
    }

    /**
     * Get request uri.
     *
     * @return the request URI
     */
    public String getRequestUri() {
        return requestUri;
    }

    /**
     * Get query params.
     *
     * @return the query parameters
     */
    public String getQueryParams() {
        return queryParams;
    }

    /**
     * Get request headers.
     *
     * @return the request headers
     */
    public Map<String, String> getRequestHeaders() {
        return requestHeaders;
    }

    /**
     * Get status.
     *
     * @return the HTTP status code
     */
    public Integer getStatus() {
        return status;
    }

    /**
     * Get response body.
     *
     * @return the response body
     */
    public String getResponseBody() {
        return responseBody;
    }

    /**
     * Get response headers.
     *
     * @return the response headers
     */
    public Map<String, String> getResponseHeaders() {
        return responseHeaders;
    }

    /**
     * Get request body.
     *
     * @return the request body
     */
    public String getRequestBody() {
        return requestBody;
    }

}
