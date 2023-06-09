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

import java.util.Objects;
import java.sql.Timestamp;

/**
 * field.
 */
public class FieldDO extends BaseDO {

    /**
     * the model id.
     */
    private String modelId;

    /**
     * the self model id.
     */
    private String selfModelId;

    /**
     * the field name.
     */
    private String name;

    /**
     * the field desc.
     */
    private String fieldDesc;

    /**
     * the required whether to require (0 not required, 1 required).
     */
    private Boolean required;

    /**
     * the ext.
     */
    private String ext;

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
        FieldDO fieldDO = (FieldDO) o;
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
    public static FieldDOBuilder builder() {
        return new FieldDOBuilder();
    }

    public static final class FieldDOBuilder {

        private String id;

        private String modelId;

        private String selfModelId;

        private String name;

        private String fieldDesc;

        private Boolean required;

        private String ext;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private FieldDOBuilder() {

        }

        /**
         * id.
         *
         * @param id id
         * @return FieldDOBuilder
         */
        public FieldDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * modelId.
         *
         * @param modelId modelId
         * @return FieldDOBuilder
         */
        public FieldDOBuilder modelId(final String modelId) {
            this.modelId = modelId;
            return this;
        }

        /**
         * selfModelId.
         *
         * @param selfModelId selfModelId
         * @return FieldDOBuilder
         */
        public FieldDOBuilder selfModelId(final String selfModelId) {
            this.selfModelId = selfModelId;
            return this;
        }

        /**
         * name.
         *
         * @param name name
         * @return FieldDOBuilder
         */
        public FieldDOBuilder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * fieldDesc.
         *
         * @param fieldDesc fieldDesc
         * @return FieldDOBuilder
         */
        public FieldDOBuilder fieldDesc(final String fieldDesc) {
            this.fieldDesc = fieldDesc;
            return this;
        }

        /**
         * required.
         *
         * @param required required
         * @return FieldDOBuilder
         */
        public FieldDOBuilder required(final Boolean required) {
            this.required = required;
            return this;
        }

        /**
         * ext.
         *
         * @param ext ext
         * @return FieldDOBuilder
         */
        public FieldDOBuilder ext(final String ext) {
            this.ext = ext;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated dateCreated
         * @return FieldDOBuilders
         */
        public FieldDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated dateUpdated
         * @return FieldDOBuilder
         */
        public FieldDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build.
         *
         * @return FieldDO
         */
        public FieldDO build() {
            FieldDO fieldDO = new FieldDO();
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
