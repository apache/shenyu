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

import java.sql.Timestamp;
import java.util.Objects;

/**
 * parameter.
 */
public final class ParameterDO extends BaseDO {

    /**
     * the api_id.
     */
    private String apiId;

    /**
     * the model_id.
     */
    private String modelId;

    /**
     * the type, 0-requestPathVariable,1-requestUrlParam,2-requestHeader,3-requestBody,
     * 4-responseHeader,5-responseBody.
     */
    private Integer type;

    /**
     * the parameter name.
     */
    private String name;

    /**
     * the description of parameter.
     */
    private String paramDesc;

    /**
     * whether to require (0 not required, 1 required).
     */
    private Boolean required;

    /**
     * extended fields.
     */
    private String ext;

    /**
     * get api_id.
     *
     * @return api_Id
     */
    public String getApiId() {
        return apiId;
    }

    /**
     * set api_id.
     *
     * @param apiId the api_id
     */
    public void setApiId(final String apiId) {
        this.apiId = apiId;
    }

    /**
     * get model_id.
     *
     * @return model_id
     */
    public String getModelId() {
        return modelId;
    }

    /**
     * set model_id.
     *
     * @param modelId the model_id
     */
    public void setModelId(final String modelId) {
        this.modelId = modelId;
    }

    /**
     * get the type.
     *
     * @return type
     */
    public Integer getType() {
        return type;
    }

    /**
     * set the type.
     *
     * @param type the type
     */
    public void setType(final Integer type) {
        this.type = type;
    }

    /**
     * get the parameter name.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set the parameter name.
     *
     * @param name the parameter name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * get the description of parameter.
     *
     * @return paramDesc
     */
    public String getParamDesc() {
        return paramDesc;
    }

    /**
     * set the description of parameter.
     *
     * @param paramDesc the param_desc
     */
    public void setParamDesc(final String paramDesc) {
        this.paramDesc = paramDesc;
    }

    /**
     * get required.
     *
     * @return required
     */
    public Boolean getRequired() {
        return required;
    }

    /**
     * set required.
     *
     * @param required whether of require
     */
    public void setRequired(final Boolean required) {
        this.required = required;
    }

    /**
     * get extend fields.
     *
     * @return ext
     */
    public String getExt() {
        return ext;
    }

    /**
     * set ext-fields.
     *
     * @param ext the ext-fields
     */
    public void setExt(final String ext) {
        this.ext = ext;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (!super.equals(o)) {
            return false;
        }
        ParameterDO parameterDO = (ParameterDO) o;
        return Objects.equals(apiId, parameterDO.apiId)
                && Objects.equals(modelId, parameterDO.modelId)
                && Objects.equals(type, parameterDO.type)
                && Objects.equals(name, parameterDO.name)
                && Objects.equals(paramDesc, parameterDO.paramDesc)
                && Objects.equals(required, parameterDO.required)
                && Objects.equals(ext, parameterDO.ext);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), apiId, modelId, type, name, paramDesc, required, ext);
    }

    /**
     * return ParameterDOBuilder.
     *
     * @return ParameterDOBuilder
     */
    public static ParameterDOBuilder builder() {
        return new ParameterDOBuilder();
    }

    public static final class ParameterDOBuilder {

        private String id;

        private String apiId;

        private String modelId;

        private Integer type;

        private String name;

        private String paramDesc;

        private Boolean required;

        private String ext;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        /**
         * id.
         *
         * @param id the primary key
         * @return ParameterDOBuilder
         */
        public ParameterDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * apiId.
         *
         * @param apiId the api_id
         * @return ParameterDOBuilder
         */
        public ParameterDOBuilder apiId(final String apiId) {
            this.apiId = apiId;
            return this;
        }

        /**
         * modelId.
         *
         * @param modelId the model_id
         * @return ParameterDOBuilder
         */
        public ParameterDOBuilder modelId(final String modelId) {
            this.modelId = modelId;
            return this;
        }

        /**
         * type.
         *
         * @param type the type
         * @return ParameterDOBuilder
         */
        public ParameterDOBuilder type(final Integer type) {
            this.type = type;
            return this;
        }

        /**
         * name.
         *
         * @param name the parameter name
         * @return ParameterDOBuilder
         */
        public ParameterDOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * paramDesc.
         *
         * @param paramDesc the param_desc
         * @return ParameterDOBuilder
         */
        public ParameterDOBuilder paramDesc(final String paramDesc) {
            this.paramDesc = paramDesc;
            return this;
        }

        /**
         * required.
         *
         * @param required whether of require
         * @return ParameterDOBuilder
         */
        public ParameterDOBuilder required(final Boolean required) {
            this.required = required;
            return this;
        }

        /**
         * ext.
         *
         * @param ext the ext_fields
         * @return ParameterDOBuilder
         */
        public ParameterDOBuilder ext(final String ext) {
            this.ext = ext;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the created date
         * @return ParameterDOBuilder
         */
        public ParameterDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the updated date
         * @return ParameterDOBuilder
         */
        public ParameterDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build a ParameterDO.
         *
         * @return ParameterDO
         */
        public ParameterDO build() {
            ParameterDO parameterDO = new ParameterDO();
            parameterDO.setId(this.id);
            parameterDO.setApiId(this.apiId);
            parameterDO.setModelId(this.modelId);
            parameterDO.setType(this.type);
            parameterDO.setName(this.name);
            parameterDO.setParamDesc(this.paramDesc);
            parameterDO.setRequired(this.required);
            parameterDO.setExt(this.ext);
            parameterDO.setDateCreated(this.dateCreated);
            parameterDO.setDateUpdated(this.dateUpdated);
            return parameterDO;
        }
    }
}
