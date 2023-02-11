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

import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;

import java.util.List;
import java.util.Objects;

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

    /**
     * event type.
     */
    private EventType eventType;

    /**
     * tags.
     */
    private List<String> tags;

    /**
     * getTags.
     * @return tags
     */
    public List<String> getTags() {
        return tags;
    }

    /**
     * setTags.
     * @param tags tags
     */
    public void setTags(final List<String> tags) {
        this.tags = tags;
    }

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

    /**
     * getEventType.
     * @return eventType
     */
    public EventType getEventType() {
        return eventType;
    }

    /**
     * setEventType.
     * @param eventType eventType
     */
    public void setEventType(final EventType eventType) {
        this.eventType = eventType;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        ApiDocRegisterDTO that = (ApiDocRegisterDTO) o;
        return Objects.equals(contextPath, that.contextPath) && Objects.equals(apiPath, that.apiPath) && Objects.equals(httpMethod, that.httpMethod) && Objects.equals(consume, that.consume)
                && Objects.equals(produce, that.produce) && Objects.equals(version, that.version) && Objects.equals(rpcType, that.rpcType) && Objects.equals(state, that.state)
                && Objects.equals(ext, that.ext) && Objects.equals(apiOwner, that.apiOwner) && Objects.equals(apiDesc, that.apiDesc) && Objects.equals(apiSource, that.apiSource)
                && Objects.equals(document, that.document) && eventType == that.eventType && Objects.equals(tags, that.tags);
    }

    @Override
    public int hashCode() {
        return Objects.hash(contextPath, apiPath, httpMethod, consume, produce, version, rpcType, state, ext, apiOwner, apiDesc, apiSource, document, eventType, tags);
    }

    @Override
    public String toString() {
        return "ApiDocRegisterDTO{"
                + "contextPath='"
                + contextPath
                + ", apiPath='"
                + apiPath
                + ", httpMethod="
                + httpMethod
                + ", consume='"
                + consume
                + ", produce='"
                + produce
                + ", version='"
                + version
                + ", rpcType='"
                + rpcType
                + ", state="
                + state
                + ", ext='"
                + ext
                + ", apiOwner='"
                + apiOwner
                + ", apiDesc='"
                + apiDesc
                + ", apiSource="
                + apiSource
                + ", document='"
                + document
                + ", eventType='"
                + eventType
                + ", tags='"
                + tags
                + '}';
    }

    /**
     * builder.
     * @return ApiDocRegisterDTOBuilder
     */
    public static ApiDocRegisterDTOBuilder builder() {
        return new ApiDocRegisterDTOBuilder();
    }

    public static final class ApiDocRegisterDTOBuilder {

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

        private EventType eventType;

        private List<String> tags;

        private ApiDocRegisterDTOBuilder() {
        }

        /**
         * build contextPath.
         * @param contextPath contextPath
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder contextPath(final String contextPath) {
            this.contextPath = contextPath;
            return this;
        }

        /**
         * build apiPath.
         * @param apiPath apiPath
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder apiPath(final String apiPath) {
            this.apiPath = apiPath;
            return this;
        }

        /**
         * build httpMethod.
         * @param httpMethod httpMethod
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder httpMethod(final Integer httpMethod) {
            this.httpMethod = httpMethod;
            return this;
        }

        /**
         * build consume.
         * @param consume consume
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder consume(final String consume) {
            this.consume = consume;
            return this;
        }

        /**
         * build produce.
         * @param produce produce
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder produce(final String produce) {
            this.produce = produce;
            return this;
        }

        /**
         * build version.
         * @param version version
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder version(final String version) {
            this.version = version;
            return this;
        }

        /**
         * build rpcType.
         * @param rpcType rpcType
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder rpcType(final String rpcType) {
            this.rpcType = rpcType;
            return this;
        }

        /**
         * build state.
         * @param state state
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder state(final Integer state) {
            this.state = state;
            return this;
        }

        /**
         * build ext.
         * @param ext ext
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder ext(final String ext) {
            this.ext = ext;
            return this;
        }

        /**
         * build apiOwner.
         * @param apiOwner apiOwner
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder apiOwner(final String apiOwner) {
            this.apiOwner = apiOwner;
            return this;
        }

        /**
         * build apiDesc.
         * @param apiDesc apiDesc
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder apiDesc(final String apiDesc) {
            this.apiDesc = apiDesc;
            return this;
        }

        /**
         * build apiSource.
         * @param apiSource apiSource
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder apiSource(final Integer apiSource) {
            this.apiSource = apiSource;
            return this;
        }

        /**
         * build document.
         * @param document document
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder document(final String document) {
            this.document = document;
            return this;
        }

        /**
         * build eventType.
         * @param eventType eventType
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder eventType(final EventType eventType) {
            this.eventType = eventType;
            return this;
        }

        /**
         * build tags.
         * @param tags tags
         * @return ApiDocRegisterDTOBuilder
         */
        public ApiDocRegisterDTOBuilder tags(final List<String> tags) {
            this.tags = tags;
            return this;
        }

        /**
         * build.
         * @return ApiDocRegisterDTO
         */
        public ApiDocRegisterDTO build() {
            ApiDocRegisterDTO apiDocRegisterDTO = new ApiDocRegisterDTO();
            apiDocRegisterDTO.setContextPath(contextPath);
            apiDocRegisterDTO.setApiPath(apiPath);
            apiDocRegisterDTO.setHttpMethod(httpMethod);
            apiDocRegisterDTO.setConsume(consume);
            apiDocRegisterDTO.setProduce(produce);
            apiDocRegisterDTO.setVersion(version);
            apiDocRegisterDTO.setRpcType(rpcType);
            apiDocRegisterDTO.setState(state);
            apiDocRegisterDTO.setExt(ext);
            apiDocRegisterDTO.setApiOwner(apiOwner);
            apiDocRegisterDTO.setApiDesc(apiDesc);
            apiDocRegisterDTO.setApiSource(apiSource);
            apiDocRegisterDTO.setDocument(document);
            apiDocRegisterDTO.setEventType(eventType);
            apiDocRegisterDTO.setTags(tags);
            return apiDocRegisterDTO;
        }
    }
}
