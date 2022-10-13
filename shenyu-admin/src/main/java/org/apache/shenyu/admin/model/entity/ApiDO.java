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

package org.apache.shenyu.admin.model.entity;

import java.util.Date;

/**
 * api.
 */
public class ApiDO {
    /**
     * primary key id.
     */
    private String id;

    /**
     * the context_path.
     */
    private String contextPath;

    /**
     * the path.
     */
    private String path;

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
    private Byte status;

    /**
     * extended fields.
     */
    private String ext;

    /**
     * owner.
     */
    private String owner;

    /**
     * the api description.
     */
    private String apiDesc;

    /**
     * complete documentation of the api, including request parameters and response parameters.
     */
    private String document;

    /**
     * document_md5.
     */
    private String documentMd5;

    /**
     * 0-swagger,1-annotation generation,2-create manuallym,3-import swagger,4-import yapi.
     */
    private Integer apiSource;

    /**
     * create time.
     */
    private Date dateCreated;

    /**
     * update time.
     */
    private Date dateUpdated;

    /**
     * getId.
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * getContextPath.
     * @return context path
     */
    public String getContextPath() {
        return contextPath;
    }

    /**
     * set context path.
     * @param contextPath context path
     */
    public void setContextPath(final String contextPath) {
        this.contextPath = contextPath;
    }

    /**
     * getPath.
     * @return path
     */
    public String getPath() {
        return path;
    }

    /**
     * setPath.
     * @param path path
     */
    public void setPath(final String path) {
        this.path = path;
    }

    /**
     * getHttpMethod.
     * @return http method
     */
    public Integer getHttpMethod() {
        return httpMethod;
    }

    /**
     * setHttpMethod.
     * @param httpMethod http method
     */
    public void setHttpMethod(final Integer httpMethod) {
        this.httpMethod = httpMethod;
    }

    /**
     * getConsume.
     * @return consume
     */
    public String getConsume() {
        return consume;
    }

    /**
     * setConsume.
     * @param consume consume
     */
    public void setConsume(final String consume) {
        this.consume = consume;
    }

    /**
     * getProduce.
     * @return produce
     */
    public String getProduce() {
        return produce;
    }

    /**
     * setProduce.
     * @param produce the produce
     */
    public void setProduce(final String produce) {
        this.produce = produce;
    }

    /**
     * getVersion.
     * @return version
     */
    public String getVersion() {
        return version;
    }

    /**
     * setVersion.
     * @param version the version
     */
    public void setVersion(final String version) {
        this.version = version;
    }

    /**
     * getRpcType.
     * @return rpc type
     */
    public String getRpcType() {
        return rpcType;
    }

    /**
     * setRpcType.
     * @param rpcType the rpc type
     */
    public void setRpcType(final String rpcType) {
        this.rpcType = rpcType;
    }

    /**
     * getStatus.
     * @return status.
     */
    public Byte getStatus() {
        return status;
    }

    /**
     * setStatus.
     * @param status status
     */
    public void setStatus(final Byte status) {
        this.status = status;
    }

    /**
     * getExt.
     * @return  extension.
     */
    public String getExt() {
        return ext;
    }

    /**
     * setExt.
     * @param ext extension
     */
    public void setExt(final String ext) {
        this.ext = ext;
    }

    /**
     * getOwner.
     * @return owner
     */
    public String getOwner() {
        return owner;
    }

    /**
     * setOwner.
     * @param owner owner
     */
    public void setOwner(final String owner) {
        this.owner = owner;
    }

    /**
     * getApiDesc.
     * @return apiDesc
     */
    public String getApiDesc() {
        return apiDesc;
    }

    /**
     * setApiDesc.
     * @param apiDesc apiDesc
     */
    public void setApiDesc(final String apiDesc) {
        this.apiDesc = apiDesc;
    }

    /**
     * getDocument.
     * @return document
     */
    public String getDocument() {
        return document;
    }

    /**
     * setDocument.
     * @param document document
     */
    public void setDocument(final String document) {
        this.document = document;
    }

    /**
     * getDocumentMd5.
     * @return document md5
     */
    public String getDocumentMd5() {
        return documentMd5;
    }

    /**
     * setDocumentMd5.
     * @param documentMd5 documentMd5
     */
    public void setDocumentMd5(final String documentMd5) {
        this.documentMd5 = documentMd5;
    }

    /**
     * getApiSource.
     * @return apiSource
     */
    public Integer getApiSource() {
        return apiSource;
    }

    /**
     * setSource.
     * @param apiSource apiSource
     */
    public void setApiSource(final Integer apiSource) {
        this.apiSource = apiSource;
    }

    /**
     * getDateCreated.
     * @return dateCreated
     */
    public Date getDateCreated() {
        return dateCreated;
    }

    /**
     * setDateCreated.
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * getDateUpdated.
     * @return dateUpdated
     */
    public Date getDateUpdated() {
        return dateUpdated;
    }

    /**
     * setDateUpdated.
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }
}
