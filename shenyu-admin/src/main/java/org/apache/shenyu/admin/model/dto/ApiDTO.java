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

import org.apache.shenyu.admin.mapper.ApiMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Date;
import java.util.List;
import java.util.Objects;

/**
 * this is api from by web front.
 */
public class ApiDTO implements Serializable {

    private static final long serialVersionUID = -1859047620316026098L;

    /**
     * primary key id.
     */
    @Existed(provider = ApiMapper.class, nullOfIgnore = true, message = "the api is not exited")
    private String id;

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
    @NotNull(message = "httpMethod not null")
    @Min(value = 0, message = "httpMethod min 0")
    @Max(value = 7, message = "httpMethod max 7")
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
    @NotNull(message = "state not null")
    @Min(value = 0, message = "state min 0")
    @Max(value = 2, message = "state max 2")
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
    @NotNull(message = "apiSource not null")
    @Min(value = 0, message = "state min 0")
    @Max(value = 4, message = "state max 4")
    private Integer apiSource;

    /**
     * complete documentation of the api, including request parameters and response parameters.
     */
    private String document;

    /**
     * document_md5.
     */
    private String documentMd5;

    /**
     * create time.
     */
    private Date dateCreated;

    /**
     * update time.
     */
    private Date dateUpdated;

    /**
     * tagIds.
     */
    private List<String> tagIds;

    /**
     * getId.
     *
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
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

    /**
     * getDocumentMd5.
     *
     * @return document md5
     */
    public String getDocumentMd5() {
        return documentMd5;
    }

    /**
     * setDocumentMd5.
     *
     * @param documentMd5 documentMd5
     */
    public void setDocumentMd5(final String documentMd5) {
        this.documentMd5 = documentMd5;
    }

    /**
     * getDateCreated.
     *
     * @return dateCreated
     */
    public Date getDateCreated() {
        return dateCreated;
    }

    /**
     * setDateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * getDateUpdated.
     *
     * @return dateUpdated
     */
    public Date getDateUpdated() {
        return dateUpdated;
    }

    /**
     * setDateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    /**
     * getTagIds.
     *
     * @return tagIds
     */
    public List<String> getTagIds() {
        return tagIds;
    }

    /**
     * setTagIds.
     *
     * @param tagIds tagIds
     */
    public void setTagIds(final List<String> tagIds) {
        this.tagIds = tagIds;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof ApiDTO)) {
            return false;
        }
        ApiDTO apiDTO = (ApiDTO) o;
        return Objects.equals(id, apiDTO.id) && Objects.equals(contextPath, apiDTO.contextPath) && Objects.equals(apiPath, apiDTO.apiPath) && Objects.equals(httpMethod, apiDTO.httpMethod)
                && Objects.equals(consume, apiDTO.consume) && Objects.equals(produce, apiDTO.produce) && Objects.equals(version, apiDTO.version) && Objects.equals(rpcType, apiDTO.rpcType)
                && Objects.equals(state, apiDTO.state) && Objects.equals(ext, apiDTO.ext) && Objects.equals(apiOwner, apiDTO.apiOwner) && Objects.equals(apiDesc, apiDTO.apiDesc)
                && Objects.equals(apiSource, apiDTO.apiSource) && Objects.equals(document, apiDTO.document) && Objects.equals(documentMd5, apiDTO.documentMd5)
                && Objects.equals(dateCreated, apiDTO.dateCreated) && Objects.equals(dateUpdated, apiDTO.dateUpdated) && Objects.equals(tagIds, apiDTO.tagIds);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, contextPath, apiPath, httpMethod, consume, produce, version,
                rpcType, state, ext, apiOwner, apiDesc, apiSource, document, documentMd5, dateCreated, dateUpdated, tagIds);
    }

}
