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

package org.apache.shenyu.admin.model.vo;

import org.apache.shenyu.admin.model.entity.ApiDO;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * this is api view to web front.
 */
public class ApiVO implements Serializable {

    private static final long serialVersionUID = 7944745026885343719L;

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
     * tags.
     */
    private List<TagVO> tags;

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
     * getTags.
     *
     * @return tags
     */
    public List<TagVO> getTags() {
        return tags;
    }

    /**
     * setTags.
     *
     * @param tags tags
     */
    public void setTags(final List<TagVO> tags) {
        this.tags = tags;
    }

    /**
     * builder.
     *
     * @return ApiVOBuilder
     */
    public static ApiVOBuilder builder() {
        return new ApiVOBuilder();
    }

    /**
     * buildApiVO.
     *
     * @param apiDO apiDO.
     * @param tags  tags.
     * @return ApiVO.
     */
    public static ApiVO buildApiVO(final ApiDO apiDO, final List<TagVO> tags) {
        return ApiVO.builder()
                .id(apiDO.getId())
                .contextPath(apiDO.getContextPath())
                .apiPath(apiDO.getApiPath())
                .httpMethod(apiDO.getHttpMethod())
                .consume(apiDO.getConsume())
                .produce(apiDO.getProduce())
                .version(apiDO.getVersion())
                .rpcType(apiDO.getRpcType())
                .state(apiDO.getState())
                .ext(apiDO.getExt())
                .apiOwner(apiDO.getApiOwner())
                .apiDesc(apiDO.getApiDesc())
                .apiSource(apiDO.getApiSource())
                .document(apiDO.getDocument())
                .documentMd5(apiDO.getDocumentMd5())
                .dateCreated(apiDO.getDateCreated())
                .dateUpdated(apiDO.getDateUpdated())
                .tags(tags)
                .build();
    }

    public static final class ApiVOBuilder {

        /**
         * id.
         */
        private String id;

        /**
         * contextPath.
         */
        private String contextPath;

        /**
         * apiPath.
         */
        private String apiPath;

        /**
         * httpMethod.
         */
        private Integer httpMethod;

        /**
         * consume.
         */
        private String consume;

        /**
         * produce.
         */
        private String produce;

        /**
         * version.
         */
        private String version;

        /**
         * rpcType.
         */
        private String rpcType;

        /**
         * status.
         */
        private Integer state;

        /**
         * ext.
         */
        private String ext;

        /**
         * apiOwner.
         */
        private String apiOwner;

        /**
         * apiDesc.
         */
        private String apiDesc;

        /**
         * apiSource.
         */
        private Integer apiSource;

        /**
         * document.
         */
        private String document;

        /**
         * documentMd5.
         */
        private String documentMd5;

        /**
         * dateCreated.
         */
        private Date dateCreated;

        /**
         * dateUpdated.
         */
        private Date dateUpdated;

        /**
         * tags.
         */
        private List<TagVO> tags;

        private ApiVOBuilder() {

        }

        /**
         * builder id.
         * @param id id
         * @return ApiVOBuilder
         */
        public ApiVOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * builder contextPath.
         * @param contextPath contextPath
         * @return ApiVOBuilder
         */
        public ApiVOBuilder contextPath(final String contextPath) {
            this.contextPath = contextPath;
            return this;
        }

        /**
         * build apiPath.
         * @param apiPath apiPath
         * @return ApiVOBuilder
         */
        public ApiVOBuilder apiPath(final String apiPath) {
            this.apiPath = apiPath;
            return this;
        }

        /**
         * build httpMethod.
         * @param httpMethod httpMethod
         * @return ApiVOBuilder
         */
        public ApiVOBuilder httpMethod(final Integer httpMethod) {
            this.httpMethod = httpMethod;
            return this;
        }

        /**
         * build consume.
         * @param consume consume
         * @return ApiVOBuilder
         */
        public ApiVOBuilder consume(final String consume) {
            this.consume = consume;
            return this;
        }

        /**
         * build produce.
         * @param produce produce
         * @return ApiVOBuilder
         */
        public ApiVOBuilder produce(final String produce) {
            this.produce = produce;
            return this;
        }

        /**
         * build version.
         * @param version version
         * @return ApiVOBuilder
         */
        public ApiVOBuilder version(final String version) {
            this.version = version;
            return this;
        }

        /**
         * build rpcType.
         * @param rpcType rpcType
         * @return ApiVOBuilder
         */
        public ApiVOBuilder rpcType(final String rpcType) {
            this.rpcType = rpcType;
            return this;
        }

        /**
         * build state.
         * @param state state
         * @return ApiVOBuilder
         */
        public ApiVOBuilder state(final Integer state) {
            this.state = state;
            return this;
        }

        /**
         * build ext.
         * @param ext ext
         * @return ApiVOBuilder
         */
        public ApiVOBuilder ext(final String ext) {
            this.ext = ext;
            return this;
        }

        /**
         * build apiOwner.
         * @param apiOwner apiOwner
         * @return ApiVOBuilder
         */
        public ApiVOBuilder apiOwner(final String apiOwner) {
            this.apiOwner = apiOwner;
            return this;
        }

        /**
         * build apiDesc.
         * @param apiDesc apiDesc
         * @return ApiVOBuilder
         */
        public ApiVOBuilder apiDesc(final String apiDesc) {
            this.apiDesc = apiDesc;
            return this;
        }

        /**
         * build apiSource.
         * @param apiSource apiSource
         * @return ApiVOBuilder
         */
        public ApiVOBuilder apiSource(final Integer apiSource) {
            this.apiSource = apiSource;
            return this;
        }

        /**
         * build document.
         * @param document document
         * @return ApiVOBuilder
         */
        public ApiVOBuilder document(final String document) {
            this.document = document;
            return this;
        }

        /**
         * build documentMd5.
         * @param documentMd5 documentMd5
         * @return ApiVOBuilder
         */
        public ApiVOBuilder documentMd5(final String documentMd5) {
            this.documentMd5 = documentMd5;
            return this;
        }

        /**
         * build dateCreated.
         * @param dateCreated dateCreated
         * @return ApiVOBuilder
         */
        public ApiVOBuilder dateCreated(final Date dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * build dateUpdated.
         * @param dateUpdated dateUpdated
         * @return ApiVOBuilder
         */
        public ApiVOBuilder dateUpdated(final Date dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build tags.
         * @param tags tags
         * @return ApiVOBuilder
         */
        public ApiVOBuilder tags(final List<TagVO> tags) {
            this.tags = tags;
            return this;
        }

        /**
         * build.
         * @return ApiVO
         */
        public ApiVO build() {
            ApiVO apiVO = new ApiVO();
            apiVO.setId(id);
            apiVO.setContextPath(contextPath);
            apiVO.setApiPath(apiPath);
            apiVO.setHttpMethod(httpMethod);
            apiVO.setConsume(consume);
            apiVO.setProduce(produce);
            apiVO.setVersion(version);
            apiVO.setRpcType(rpcType);
            apiVO.setState(state);
            apiVO.setExt(ext);
            apiVO.setApiOwner(apiOwner);
            apiVO.setApiDesc(apiDesc);
            apiVO.setApiSource(apiSource);
            apiVO.setDocument(document);
            apiVO.setDocumentMd5(documentMd5);
            apiVO.setDateCreated(dateCreated);
            apiVO.setDateUpdated(dateUpdated);
            apiVO.setTags(tags);
            return apiVO;
        }
    }
}
