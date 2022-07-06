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

package org.apache.shenyu.plugin.logging.common.entity;

/**
 * shenyu gateway access log.
 */
public class ShenyuRequestLog {

    private String clientIp;

    private String timeLocal;

    private String method;

    private String requestHeader;

    private String responseHeader;

    private String queryParams;

    private String requestBody;

    private String requestUri;

    private String responseBody;

    private int responseContentLength;

    private String rpcType;

    private int status;

    private String upstreamIp;

    private long upstreamResponseTime;

    private String userAgent;

    private String host;

    private String module;

    private String traceId;

    /**
     * path.
     */
    private String path;

    /**
     * get module.
     *
     * @return module
     */
    public String getModule() {
        return module;
    }

    /**
     * set module.
     *
     * @param module module
     */
    public void setModule(final String module) {
        this.module = module;
    }

    /**
     * get responseContentLength.
     *
     * @return ResponseContentLength
     */
    public int getResponseContentLength() {
        return responseContentLength;
    }

    /**
     * set ResponseContentLength.
     *
     * @param responseContentLength ResponseContentLength
     */
    public void setResponseContentLength(final int responseContentLength) {
        this.responseContentLength = responseContentLength;
    }

    /**
     * get userAgent.
     *
     * @return userAgent
     */
    public String getUserAgent() {
        return userAgent;
    }

    /**
     * set userAgent.
     *
     * @param userAgent userAgent
     */
    public void setUserAgent(final String userAgent) {
        this.userAgent = userAgent;
    }

    /**
     * get host.
     *
     * @return host
     */
    public String getHost() {
        return host;
    }

    /**
     * set host.
     *
     * @param host host
     */
    public void setHost(final String host) {
        this.host = host;
    }

    /**
     * get clientIp.
     *
     * @return clientIp
     */
    public String getClientIp() {
        return clientIp;
    }

    /**
     * set clientIp.
     *
     * @param clientIp clientIp
     */
    public void setClientIp(final String clientIp) {
        this.clientIp = clientIp;
    }

    /**
     * get timeLocal.
     *
     * @return timeLocal
     */
    public String getTimeLocal() {
        return timeLocal;
    }

    /**
     * set timeLocal.
     *
     * @param timeLocal timeLocal
     */
    public void setTimeLocal(final String timeLocal) {
        this.timeLocal = timeLocal;
    }

    /**
     * get method.
     *
     * @return method
     */
    public String getMethod() {
        return method;
    }

    /**
     * set method.
     *
     * @param method method
     */
    public void setMethod(final String method) {
        this.method = method;
    }

    /**
     * get requestHeader.
     *
     * @return requestHeader
     */
    public String getRequestHeader() {
        return requestHeader;
    }

    /**
     * set requestHeader.
     *
     * @param requestHeader requestHeader
     */
    public void setRequestHeader(final String requestHeader) {
        this.requestHeader = requestHeader;
    }

    /**
     * get responseHeader.
     *
     * @return responseHeader
     */
    public String getResponseHeader() {
        return responseHeader;
    }

    /**
     * set responseHeader.
     *
     * @param responseHeader responseHeader
     */
    public void setResponseHeader(final String responseHeader) {
        this.responseHeader = responseHeader;
    }

    /**
     * get queryParams.
     *
     * @return queryParams
     */
    public String getQueryParams() {
        return queryParams;
    }

    /**
     * set queryParams.
     *
     * @param queryParams queryParams
     */
    public void setQueryParams(final String queryParams) {
        this.queryParams = queryParams;
    }

    /**
     * get requestBody.
     *
     * @return requestBody
     */
    public String getRequestBody() {
        return requestBody;
    }

    /**
     * set requestBody.
     *
     * @param requestBody requestBody
     */
    public void setRequestBody(final String requestBody) {
        this.requestBody = requestBody;
    }

    /**
     * get requestUri.
     *
     * @return requestUri
     */
    public String getRequestUri() {
        return requestUri;
    }

    /**
     * set requestUri.
     *
     * @param requestUri requestUri
     */
    public void setRequestUri(final String requestUri) {
        this.requestUri = requestUri;
    }

    /**
     * get responseBody.
     *
     * @return responseBody
     */
    public String getResponseBody() {
        return responseBody;
    }

    /**
     * set responseBody.
     *
     * @param responseBody responseBody
     */
    public void setResponseBody(final String responseBody) {
        this.responseBody = responseBody;
    }

    /**
     * get rpcType.
     *
     * @return rpcType
     */
    public String getRpcType() {
        return rpcType;
    }

    /**
     * set rpcType.
     *
     * @param rpcType rpcType
     */
    public void setRpcType(final String rpcType) {
        this.rpcType = rpcType;
    }

    /**
     * set status.
     *
     * @return status
     */
    public int getStatus() {
        return status;
    }

    /**
     * set status.
     *
     * @param status status
     */
    public void setStatus(final int status) {
        this.status = status;
    }

    /**
     * get upstreamIp.
     *
     * @return upstreamIp
     */
    public String getUpstreamIp() {
        return upstreamIp;
    }

    /**
     * set upstreamIp.
     *
     * @param upstreamIp upstreamIp
     */
    public void setUpstreamIp(final String upstreamIp) {
        this.upstreamIp = upstreamIp;
    }

    /**
     * get upstreamResponseTime.
     *
     * @return upstreamResponseTime
     */
    public long getUpstreamResponseTime() {
        return upstreamResponseTime;
    }

    /**
     * set UpstreamResponseTime.
     *
     * @param upstreamResponseTime upstreamResponseTime
     */
    public void setUpstreamResponseTime(final long upstreamResponseTime) {
        this.upstreamResponseTime = upstreamResponseTime;
    }

    /**
     * get traceId.
     *
     * @return traceId
     */
    public String getTraceId() {
        return traceId;
    }

    /**
     * set traceId.
     *
     * @param traceId tracing id
     */
    public void setTraceId(final String traceId) {
        this.traceId = traceId;
    }

    /**
     * get request path.
     *
     * @return request path
     */
    public String getPath() {
        return path;
    }

    /**
     * request path.
     *
     * @param path request path
     */
    public void setPath(final String path) {
        this.path = path;
    }
}
