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

package org.apache.shenyu.loadbalancer.entity;

import java.net.URI;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * The type Load balance data.
 */
public class LoadBalanceData {
    
    private String httpMethod = "GET";
    
    private String ip = "127.0.0.1";
    
    private URI url;
    
    private Map<String, Collection<String>> headers = new HashMap<>();
    
    private Map<String, String> cookies = new HashMap<>();
    
    private Map<String, Object> attributes = new HashMap<>();
    
    private Map<String, Collection<String>> queryParams = new HashMap<>();
    
    /**
     * Instantiates a new Load balance data.
     */
    public LoadBalanceData() {
    }
    
    /**
     * Instantiates a new Load balance data.
     *
     * @param httpMethod the http method
     * @param ip the ip
     * @param url the url
     * @param headers the headers
     * @param cookies the cookies
     * @param attributes the attributes
     * @param queryParams the query params
     */
    public LoadBalanceData(final String httpMethod,
                           final String ip,
                           final URI url,
                           final Map<String, Collection<String>> headers,
                           final Map<String, String> cookies,
                           final Map<String, Object> attributes,
                           final Map<String, Collection<String>> queryParams) {
        this.httpMethod = httpMethod;
        this.ip = ip;
        this.url = url;
        this.headers = headers;
        this.cookies = cookies;
        this.attributes = attributes;
        this.queryParams = queryParams;
    }
    
    
    /**
     * Gets http method.
     *
     * @return the http method
     */
    public String getHttpMethod() {
        return httpMethod;
    }
    
    /**
     * Gets ip.
     *
     * @return the ip
     */
    public String getIp() {
        return ip;
    }
    
    
    /**
     * Gets url.
     *
     * @return the url
     */
    public URI getUrl() {
        return url;
    }
    
    /**
     * Gets headers.
     *
     * @return the headers
     */
    public Map<String, Collection<String>> getHeaders() {
        return headers;
    }
    
    /**
     * Gets cookies.
     *
     * @return the cookies
     */
    public Map<String, String> getCookies() {
        return cookies;
    }
    
    /**
     * Gets attributes.
     *
     * @return the attributes
     */
    public Map<String, Object> getAttributes() {
        return attributes;
    }
    
    /**
     * Gets query params.
     *
     * @return the query params
     */
    public Map<String, Collection<String>> getQueryParams() {
        return queryParams;
    }
    
    /**
     * Sets http method.
     *
     * @param httpMethod the http method
     */
    public void setHttpMethod(final String httpMethod) {
        this.httpMethod = httpMethod;
    }
    
    /**
     * Sets query params.
     *
     * @param queryParams the query params
     */
    public void setQueryParams(final Map<String, Collection<String>> queryParams) {
        this.queryParams = queryParams;
    }
    
    /**
     * Sets attributes.
     *
     * @param attributes the attributes
     */
    public void setAttributes(final Map<String, Object> attributes) {
        this.attributes = attributes;
    }
    
    /**
     * Sets cookies.
     *
     * @param cookies the cookies
     */
    public void setCookies(final Map<String, String> cookies) {
        this.cookies = cookies;
    }
    
    /**
     * Sets headers.
     *
     * @param headers the headers
     */
    public void setHeaders(final Map<String, Collection<String>> headers) {
        this.headers = headers;
    }
    
    /**
     * Sets url.
     *
     * @param url the url
     */
    public void setUrl(final URI url) {
        this.url = url;
    }
    
    /**
     * Sets ip.
     *
     * @param ip the ip
     */
    public void setIp(final String ip) {
        this.ip = ip;
    }
}
