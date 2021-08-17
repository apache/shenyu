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

package org.apache.shenyu.plugin.api.context;

import java.io.Serializable;
import java.time.LocalDateTime;

/**
 * the shenyu context.
 */
public class ShenyuContext implements Serializable {

    private static final long serialVersionUID = 8668695964617280718L;

    /**
     * is module data.
     */
    private String module;

    /**
     * is method name .
     */
    private String method;

    /**
     * is rpcType data. now we only support "http","dubbo","springCloud","sofa".
     */
    private String rpcType;

    /**
     * httpMethod .
     */
    private String httpMethod;

    /**
     * this is sign .
     */
    private String sign;

    /**
     * timestamp .
     */
    private String timestamp;

    /**
     * appKey .
     */
    private String appKey;

    /**
     * path.
     */
    private String path;

    /**
     * the contextPath.
     */
    private String contextPath;

    /**
     * realUrl.
     */
    private String realUrl;

    /**
     * startDateTime.
     */
    private LocalDateTime startDateTime;

    /**
     * Gets module.
     *
     * @return the module
     */
    public String getModule() {
        return module;
    }

    /**
     * Sets module.
     *
     * @param module the module
     */
    public void setModule(final String module) {
        this.module = module;
    }

    /**
     * Gets method.
     *
     * @return the method
     */
    public String getMethod() {
        return method;
    }

    /**
     * Sets method.
     *
     * @param method the method
     */
    public void setMethod(final String method) {
        this.method = method;
    }

    /**
     * Gets rpc type.
     *
     * @return the rpc type
     */
    public String getRpcType() {
        return rpcType;
    }

    /**
     * Sets rpc type.
     *
     * @param rpcType the rpc type
     */
    public void setRpcType(final String rpcType) {
        this.rpcType = rpcType;
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
     * Sets http method.
     *
     * @param httpMethod the http method
     */
    public void setHttpMethod(final String httpMethod) {
        this.httpMethod = httpMethod;
    }

    /**
     * Gets sign.
     *
     * @return the sign
     */
    public String getSign() {
        return sign;
    }

    /**
     * Sets sign.
     *
     * @param sign the sign
     */
    public void setSign(final String sign) {
        this.sign = sign;
    }

    /**
     * Gets timestamp.
     *
     * @return the timestamp
     */
    public String getTimestamp() {
        return timestamp;
    }

    /**
     * Sets timestamp.
     *
     * @param timestamp the timestamp
     */
    public void setTimestamp(final String timestamp) {
        this.timestamp = timestamp;
    }

    /**
     * Gets app key.
     *
     * @return the app key
     */
    public String getAppKey() {
        return appKey;
    }

    /**
     * Sets app key.
     *
     * @param appKey the app key
     */
    public void setAppKey(final String appKey) {
        this.appKey = appKey;
    }

    /**
     * Gets path.
     *
     * @return the path
     */
    public String getPath() {
        return path;
    }

    /**
     * Sets path.
     *
     * @param path the path
     */
    public void setPath(final String path) {
        this.path = path;
    }

    /**
     * Gets context path.
     *
     * @return the context path
     */
    public String getContextPath() {
        return contextPath;
    }

    /**
     * Sets context path.
     *
     * @param contextPath the context path
     */
    public void setContextPath(final String contextPath) {
        this.contextPath = contextPath;
    }

    /**
     * Gets real url.
     *
     * @return the real url
     */
    public String getRealUrl() {
        return realUrl;
    }

    /**
     * Sets real url.
     *
     * @param realUrl the real url
     */
    public void setRealUrl(final String realUrl) {
        this.realUrl = realUrl;
    }

    /**
     * Gets start date time.
     *
     * @return the start date time
     */
    public LocalDateTime getStartDateTime() {
        return startDateTime;
    }

    /**
     * Sets start date time.
     *
     * @param startDateTime the start date time
     */
    public void setStartDateTime(final LocalDateTime startDateTime) {
        this.startDateTime = startDateTime;
    }
}
