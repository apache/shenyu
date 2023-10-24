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

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.ApiDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Date;
import java.util.Optional;

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
     * getApiPath.
     * @return apiPath
     */
    public String getApiPath() {
        return apiPath;
    }

    /**
     * setApiPath.
     * @param apiPath apiPath
     */
    public void setApiPath(final String apiPath) {
        this.apiPath = apiPath;
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
     * getState.
     * @return state
     */
    public Integer getState() {
        return state;
    }

    /**
     * setState.
     * @param state state
     */
    public void setState(final Integer state) {
        this.state = state;
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
     * getApiOwner.
     * @return apiOwner
     */
    public String getApiOwner() {
        return apiOwner;
    }

    /**
     * setApiOwner.
     * @param apiOwner apiOwner
     */
    public void setApiOwner(final String apiOwner) {
        this.apiOwner = apiOwner;
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

    /**
     * builder.
     * @return ApiDOBuilder
     */
    public static ApiDOBuilder builder() {
        return new ApiDOBuilder();
    }

    /**
     * buildApiDO.
     * @param apiDTO apiDTO
     * @return ApiDO
     */
    public static ApiDO buildApiDO(final ApiDTO apiDTO) {

        return Optional.ofNullable(apiDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            ApiDO apiDO = ApiDO.builder()
                    .contextPath(item.getContextPath())
                    .apiPath(item.getApiPath())
                    .httpMethod(item.getHttpMethod())
                    .consume(item.getConsume())
                    .produce(item.getProduce())
                    .version(item.getVersion())
                    .rpcType(item.getRpcType())
                    .state(item.getState())
                    .ext(item.getExt())
                    .apiOwner(item.getApiOwner())
                    .apiDesc(item.getApiDesc())
                    .apiSource(item.getApiSource())
                    .document(item.getDocument())
                    .documentMd5(DigestUtils.md5Hex(item.getDocument()))
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                apiDO.setId(UUIDUtils.getInstance().generateShortUuid());
                apiDO.setDateCreated(currentTime);
            } else {
                apiDO.setId(item.getId());
            }
            return apiDO;
        }).orElse(null);
    }

    public static final class ApiDOBuilder {

        private String id;

        private String contextPath;

        private String apiPath;

        private Integer httpMethod;

        private String consume;

        private String produce;

        private String version;

        private String rpcType;

        private Integer state;

        private String ext;

        private String apiOwner;

        private String apiDesc;

        private Integer apiSource;

        private String document;

        private String documentMd5;

        private Date dateCreated;

        private Date dateUpdated;

        /**
         * Construct.
         */
        private ApiDOBuilder() {
        }

        /**
         * builder id.
         * @param id id
         * @return ApiDOBuilder
         */
        public ApiDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * builder contextPath.
         * @param contextPath contextPath
         * @return ApiDOBuilder
         */
        public ApiDOBuilder contextPath(final String contextPath) {
            this.contextPath = contextPath;
            return this;
        }

        /**
         * builder apiPath.
         * @param apiPath apiPath
         * @return ApiDOBuilder
         */
        public ApiDOBuilder apiPath(final String apiPath) {
            this.apiPath = apiPath;
            return this;
        }

        /**
         * builder httpMethod.
         * @param httpMethod httpMethod
         * @return ApiDOBuilder
         */
        public ApiDOBuilder httpMethod(final Integer httpMethod) {
            this.httpMethod = httpMethod;
            return this;
        }

        /**
         * builder httpMethod.
         * @param consume consume
         * @return ApiDOBuilder
         */
        public ApiDOBuilder consume(final String consume) {
            this.consume = consume;
            return this;
        }

        /**
         * builder produce.
         * @param produce produce
         * @return ApiDOBuilder
         */
        public ApiDOBuilder produce(final String produce) {
            this.produce = produce;
            return this;
        }

        /**
         * builder version.
         * @param version version
         * @return ApiDOBuilder
         */
        public ApiDOBuilder version(final String version) {
            this.version = version;
            return this;
        }

        /**
         * builder rpcType.
         * @param rpcType rpcType
         * @return ApiDOBuilder
         */
        public ApiDOBuilder rpcType(final String rpcType) {
            this.rpcType = rpcType;
            return this;
        }

        /**
         * builder state.
         * @param state state
         * @return ApiDOBuilder
         */
        public ApiDOBuilder state(final Integer state) {
            this.state = state;
            return this;
        }

        /**
         * builder ext.
         * @param ext ext
         * @return ApiDOBuilder
         */
        public ApiDOBuilder ext(final String ext) {
            this.ext = ext;
            return this;
        }

        /**
         * builder apiOwner.
         * @param apiOwner apiOwner
         * @return ApiDOBuilder
         */
        public ApiDOBuilder apiOwner(final String apiOwner) {
            this.apiOwner = apiOwner;
            return this;
        }

        /**
         * builder apiDesc.
         * @param apiDesc apiDesc
         * @return ApiDOBuilder
         */
        public ApiDOBuilder apiDesc(final String apiDesc) {
            this.apiDesc = apiDesc;
            return this;
        }

        /**
         * builder apiSource.
         * @param apiSource apiSource
         * @return ApiDOBuilder
         */
        public ApiDOBuilder apiSource(final Integer apiSource) {
            this.apiSource = apiSource;
            return this;
        }

        /**
         * builder document.
         * @param document document
         * @return ApiDOBuilder
         */
        public ApiDOBuilder document(final String document) {
            this.document = document;
            return this;
        }

        /**
         * builder documentMd5.
         * @param documentMd5 documentMd5
         * @return ApiDOBuilder
         */
        public ApiDOBuilder documentMd5(final String documentMd5) {
            this.documentMd5 = documentMd5;
            return this;
        }

        /**
         * builder dateCreated.
         * @param dateCreated dateCreated
         * @return ApiDOBuilder
         */
        public ApiDOBuilder dateCreated(final Date dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * builder dateUpdated.
         * @param dateUpdated dateUpdated
         * @return ApiDOBuilder
         */
        public ApiDOBuilder dateUpdated(final Date dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * builder.
         * @return ApiDO
         */
        public ApiDO build() {
            ApiDO apiDO = new ApiDO();
            apiDO.setId(id);
            apiDO.setContextPath(contextPath);
            apiDO.setApiPath(apiPath);
            apiDO.setHttpMethod(httpMethod);
            apiDO.setConsume(consume);
            apiDO.setProduce(produce);
            apiDO.setVersion(version);
            apiDO.setRpcType(rpcType);
            apiDO.setState(state);
            apiDO.setExt(ext);
            apiDO.setApiOwner(apiOwner);
            apiDO.setApiDesc(apiDesc);
            apiDO.setApiSource(apiSource);
            apiDO.setDocument(document);
            apiDO.setDocumentMd5(documentMd5);
            apiDO.setDateCreated(dateCreated);
            apiDO.setDateUpdated(dateUpdated);
            return apiDO;
        }
    }
}
