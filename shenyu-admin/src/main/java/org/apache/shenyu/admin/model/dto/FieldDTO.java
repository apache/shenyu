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

import org.apache.shenyu.admin.mapper.FieldMapper;
import org.apache.shenyu.admin.validation.annotation.Existed;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Objects;

/**
 * field.
 */
public class FieldDTO implements Serializable {

    private static final long serialVersionUID = 7476131236434536366L;

    @Existed(provider = FieldMapper.class, nullOfIgnore = true, message = "filed is not existed")
    private String id;

    /**
     * the model id.
     */
    @NotBlank
    private String modelId;

    /**
     * the self model id.
     */
    @NotBlank
    private String selfModelId;

    /**
     * the field name.
     */
    @NotBlank
    private String name;

    /**
     * the field desc.
     */
    @NotBlank
    private String fieldDesc;

    /**
     * the required whether to require (0 not required, 1 required).
     */
    @NotNull
    private Boolean required;

    /**
     * the ext.
     */
    @NotBlank
    private String ext;

    /**
     * created time.
     */
    private Timestamp dateCreated;

    /**
     * updated time.
     */
    private Timestamp dateUpdated;


    /**
     * getModelId.
     *
     * @return modelId
     */
    public String getModelId() {
        return modelId;
    }

    /**
     * set model id.
     *
     * @param modelId modelId
     */
    public void setModelId(final String modelId) {
        this.modelId = modelId;
    }

    /**
     * getSelfModelId.
     *
     * @return selfModeId
     */
    public String getSelfModelId() {
        return selfModelId;
    }

    /**
     * set self model id.
     *
     * @param selfModelId selfModelId
     */
    public void setSelfModelId(final String selfModelId) {
        this.selfModelId = selfModelId;
    }

    /**
     * getName.
     *
     * @return name
     */
    public String getName() {
        return name;
    }

    /**
     * set name.
     *
     * @param name name.
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * getFieldDesc.
     *
     * @return fieldDesc.
     */
    public String getFieldDesc() {
        return fieldDesc;
    }

    /**
     * set field desc.
     *
     * @param fieldDesc fieldDesc
     */
    public void setFieldDesc(final String fieldDesc) {
        this.fieldDesc = fieldDesc;
    }

    /**
     * getRequired.
     *
     * @return required
     */
    public Boolean getRequired() {
        return required;
    }

    /**
     * set required.
     *
     * @param required required
     */
    public void setRequired(final Boolean required) {
        this.required = required;
    }

    /**
     * getExt.
     *
     * @return ext
     */
    public String getExt() {
        return ext;
    }

    /**
     * getId.
     *
     * @return String
     */
    public String getId() {
        return id;
    }

    /**
     * setId.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * getDateCreated.
     *
     * @return Timestamp
     */
    public Timestamp getDateCreated() {
        return dateCreated;
    }

    /**
     * setDateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Timestamp dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * getDateUpdated.
     *
     * @return Timestamp
     */
    public Timestamp getDateUpdated() {
        return dateUpdated;
    }

    /**
     * setDateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Timestamp dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    /**
     * set ext.
     *
     * @param ext ext
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
        FieldDTO fieldDO = (FieldDTO) o;
        return Objects.equals(modelId, fieldDO.modelId)
                && Objects.equals(selfModelId, fieldDO.selfModelId)
                && Objects.equals(name, fieldDO.name)
                && Objects.equals(fieldDesc, fieldDO.fieldDesc)
                && Objects.equals(required, fieldDO.required)
                && Objects.equals(ext, fieldDO.ext);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), modelId, selfModelId, name, fieldDesc, required, ext);
    }

    /**
     * builder.
     *
     * @return fieldDOBuilder
     */
    public static FieldDTO.FieldDTOBuilder builder() {
        return new FieldDTO.FieldDTOBuilder();
    }

    public static final class FieldDTOBuilder {

        private String id;

        private String modelId;

        private String selfModelId;

        private String name;

        private String fieldDesc;

        private Boolean required;

        private String ext;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private FieldDTOBuilder() {

        }

        /**
         * id.
         *
         * @param id id
         * @return FieldDOBuilder
         */
        public FieldDTO.FieldDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * modelId.
         *
         * @param modelId modelId
         * @return FieldDOBuilder
         */
        public FieldDTO.FieldDTOBuilder modelId(final String modelId) {
            this.modelId = modelId;
            return this;
        }

        /**
         * selfModelId.
         *
         * @param selfModelId selfModelId
         * @return FieldDOBuilder
         */
        public FieldDTO.FieldDTOBuilder selfModelId(final String selfModelId) {
            this.selfModelId = selfModelId;
            return this;
        }

        /**
         * name.
         *
         * @param name name
         * @return FieldDOBuilder
         */
        public FieldDTO.FieldDTOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * fieldDesc.
         *
         * @param fieldDesc fieldDesc
         * @return FieldDOBuilder
         */
        public FieldDTO.FieldDTOBuilder fieldDesc(final String fieldDesc) {
            this.fieldDesc = fieldDesc;
            return this;
        }

        /**
         * required.
         *
         * @param required required
         * @return FieldDOBuilder
         */
        public FieldDTO.FieldDTOBuilder required(final Boolean required) {
            this.required = required;
            return this;
        }

        /**
         * ext.
         *
         * @param ext ext
         * @return FieldDOBuilder
         */
        public FieldDTO.FieldDTOBuilder ext(final String ext) {
            this.ext = ext;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated dateCreated
         * @return FieldDOBuilders
         */
        public FieldDTO.FieldDTOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated dateUpdated
         * @return FieldDOBuilder
         */
        public FieldDTO.FieldDTOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build.
         *
         * @return FieldDO
         */
        public FieldDTO build() {
            FieldDTO fieldDO = new FieldDTO();
            fieldDO.setId(this.id);
            fieldDO.setModelId(this.modelId);
            fieldDO.setSelfModelId(this.selfModelId);
            fieldDO.setName(this.name);
            fieldDO.setFieldDesc(this.fieldDesc);
            fieldDO.setRequired(this.required);
            fieldDO.setExt(this.ext);
            fieldDO.setDateCreated(this.dateCreated);
            fieldDO.setDateUpdated(this.dateUpdated);
            return fieldDO;
        }
    }

}
