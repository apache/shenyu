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

package org.apache.shenyu.register.common.dto;

import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

/**
 * The type Meta data dto.
 */
public class ApiDocRegisterDTO implements DataTypeParent {

    /**
     * the context_path.
     */
    private String contextPath;

    /**
     * the apiPath.
     */
    private String apiPath;

    /**
     * 0-get,1-head,2-post,3-put,4-patch,5-delete,6-options,7-trace.
     */
    private Integer httpMethod;

    /**
     * specify the submitted content type for processing requests, such as application/json, text/html;.
     */
    private String consume;

    /**
     * specify the content type to be returned. only when the (accept) type in the request header contains the specified type can it be returned;.
     */
    private String produce;

    /**
     * api version,for example V0.01.
     */
    private String version;

    /**
     * http,dubbo,sofa,tars,websocket,springCloud,motan,grpc.
     */
    private String rpcType;

    /**
     * 0-unpublished1-published2-offline.
     */
    private Integer state;

    /**
     * extended fields.
     */
    private String ext;

    /**
     * apiOwner.
     */
    private String apiOwner;

    /**
     * the api description.
     */
    private String apiDesc;

    /**
     * 0-swagger,1-annotation generation,2-create manuallym,3-import swagger,4-import yapi.
     */
    private Integer apiSource;

    /**
     * complete documentation of the api, including request parameters and response parameters.
     */
    private String document;

    @Override
    public DataType getType() {
        return DataType.API_DOC;
    }

    /**
     * getContextPath.
     *
     * @return context path
     */
    public String getContextPath() {
        return contextPath;
    }

    /**
     * set context path.
     *
     * @param contextPath context path
     */
    public void setContextPath(final String contextPath) {
        this.contextPath = contextPath;
    }

    /**
     * getApiPath.
     *
     * @return apiPath
     */
    public String getApiPath() {
        return apiPath;
    }

    /**
     * setApiPath.
     *
     * @param apiPath apiPath
     */
    public void setApiPath(final String apiPath) {
        this.apiPath = apiPath;
    }

    /**
     * getHttpMethod.
     *
     * @return http method
     */
    public Integer getHttpMethod() {
        return httpMethod;
    }

    /**
     * setHttpMethod.
     *
     * @param httpMethod http method
     */
    public void setHttpMethod(final Integer httpMethod) {
        this.httpMethod = httpMethod;
    }

    /**
     * getConsume.
     *
     * @return consume
     */
    public String getConsume() {
        return consume;
    }

    /**
     * setConsume.
     *
     * @param consume consume
     */
    public void setConsume(final String consume) {
        this.consume = consume;
    }

    /**
     * getProduce.
     *
     * @return produce
     */
    public String getProduce() {
        return produce;
    }

    /**
     * setProduce.
     *
     * @param produce the produce
     */
    public void setProduce(final String produce) {
        this.produce = produce;
    }

    /**
     * getVersion.
     *
     * @return version
     */
    public String getVersion() {
        return version;
    }

    /**
     * setVersion.
     *
     * @param version the version
     */
    public void setVersion(final String version) {
        this.version = version;
    }

    /**
     * getRpcType.
     *
     * @return rpc type
     */
    public String getRpcType() {
        return rpcType;
    }

    /**
     * setRpcType.
     *
     * @param rpcType the rpc type
     */
    public void setRpcType(final String rpcType) {
        this.rpcType = rpcType;
    }

    /**
     * getState.
     *
     * @return state
     */
    public Integer getState() {
        return state;
    }

    /**
     * setState.
     *
     * @param state state
     */
    public void setState(final Integer state) {
        this.state = state;
    }

    /**
     * getExt.
     *
     * @return extension.
     */
    public String getExt() {
        return ext;
    }

    /**
     * setExt.
     *
     * @param ext extension
     */
    public void setExt(final String ext) {
        this.ext = ext;
    }

    /**
     * getApiOwner.
     *
     * @return apiOwner
     */
    public String getApiOwner() {
        return apiOwner;
    }

    /**
     * setApiOwner.
     *
     * @param apiOwner apiOwner
     */
    public void setApiOwner(final String apiOwner) {
        this.apiOwner = apiOwner;
    }

    /**
     * getApiDesc.
     *
     * @return apiDesc
     */
    public String getApiDesc() {
        return apiDesc;
    }

    /**
     * setApiDesc.
     *
     * @param apiDesc apiDesc
     */
    public void setApiDesc(final String apiDesc) {
        this.apiDesc = apiDesc;
    }

    /**
     * getApiSource.
     *
     * @return apiSource
     */
    public Integer getApiSource() {
        return apiSource;
    }

    /**
     * setSource.
     *
     * @param apiSource apiSource
     */
    public void setApiSource(final Integer apiSource) {
        this.apiSource = apiSource;
    }

    /**
     * getDocument.
     *
     * @return document
     */
    public String getDocument() {
        return document;
    }

    /**
     * setDocument.
     *
     * @param document document
     */
    public void setDocument(final String document) {
        this.document = document;
    }
}
