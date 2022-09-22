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

package org.apache.shenyu.admin.model.dto;

/**
 * this User Role Dto.
 */
public class ProxyGatewayDTO {

    /**
     * request Url.
     */
    private String requestUrl;

    /**
     * request appKey.
     */
    private String appKey;

    /**
     * request cookie.
     */
    private String cookie;

    /**
     * request header.
     */
    private Object headers;

    /**
     * request bizParam.
     */
    private Object bizParam;

    /**
     * request httpMethod.
     */
    private String httpMethod = "get";

    /**
     * getRequestUrl.
     *
     * @return request Url
     */
    public String getRequestUrl() {
        return requestUrl;
    }

    /**
     * setRequestUrl.
     *
     * @param requestUrl requestUrl
     */
    public void setRequestUrl(final String requestUrl) {
        this.requestUrl = requestUrl;
    }

    /**
     * getAppKey.
     *
     * @return appKey
     */
    public String getAppKey() {
        return appKey;
    }

    /**
     * setAppKey.
     *
     * @param appKey appKey
     */
    public void setAppKey(final String appKey) {
        this.appKey = appKey;
    }

    /**
     * getCookie.
     *
     * @return cookie
     */
    public String getCookie() {
        return cookie;
    }

    /**
     * setCookie.
     *
     * @param cookie cookie
     */
    public void setCookie(final String cookie) {
        this.cookie = cookie;
    }

    /**
     * get header json.
     * @return header json
     */
    public Object getHeaders() {
        return headers;
    }

    /**
     * set headers.
     *
     * @param headers headers
     */
    public void setHeaders(final Object headers) {
        this.headers = headers;
    }

    /**
     * get bizParam json.
     *
     * @return bizParam
     */
    public Object getBizParam() {
        return bizParam;
    }

    /**
     * set bizParam.
     *
     * @param bizParam bizParam
     */
    public void setBizParam(final Object bizParam) {
        this.bizParam = bizParam;
    }

    /**
     * getHttpMethod.
     *
     * @return httpMethod
     */
    public String getHttpMethod() {
        return httpMethod;
    }

    /**
     * httpMethod.
     *
     * @param httpMethod httpMethod
     */
    public void setHttpMethod(final String httpMethod) {
        this.httpMethod = httpMethod;
    }
}
