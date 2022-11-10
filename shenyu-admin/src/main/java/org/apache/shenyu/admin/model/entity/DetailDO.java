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
 * DetailDO.
 */
public class DetailDO extends BaseDO {

    /**
     * the field id.
     */
    private String fieldId;

    /**
     * is example.
     */
    private Boolean example;

    /**
     * field value.
     */
    private String fieldValue;

    /**
     * value desc.
     */
    private String valueDesc;

    /**
     *  getFieldId.
     *
     * @return fieldId
     */
    public String getFieldId() {
        return fieldId;
    }

    /**
     * set fieldId.
     *
     * @param fieldId fieldId
     */
    public void setFieldId(final String fieldId) {
        this.fieldId = fieldId;
    }

    /**
     * get Example.
     *
     * @return example
     */
    public Boolean getExample() {
        return example;
    }

    /**
     * set example.
     *
     * @param example example.
     */
    public void setExample(final Boolean example) {
        this.example = example;
    }

    /**
     * get fieldValue.
     *
     * @return fieldValue
     */
    public String getFieldValue() {
        return fieldValue;
    }

    /**
     * set fieldValue.
     *
     * @param fieldValue fieldValue
     */
    public void setFieldValue(final String fieldValue) {
        this.fieldValue = fieldValue;
    }

    /**
     * get valueDesc.
     *
     * @return valueDesc
     */
    public String getValueDesc() {
        return valueDesc;
    }

    /**
     * set valueDesc.
     *
     * @param valueDesc valueDesc
     */
    public void setValueDesc(final String valueDesc) {
        this.valueDesc = valueDesc;
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
        DetailDO detailDO = (DetailDO) o;
        return Objects.equals(fieldId, detailDO.fieldId)
                && Objects.equals(example, detailDO.example)
                && Objects.equals(fieldValue, detailDO.fieldValue)
                && Objects.equals(valueDesc, detailDO.valueDesc);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), fieldId, example, fieldValue, valueDesc);
    }


    /**
     * builder.
     *
     * @return fieldDOBuilder
     */
    public static DetailDOBuilder builder() {
        return new DetailDOBuilder();
    }

    public static final class DetailDOBuilder {

        private String id;

        private String fieldId;

        private Boolean example;

        private String fieldValue;

        private String valueDesc;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private DetailDOBuilder() {

        }

        /**
         * id.
         *
         * @param id id
         * @return DetailDOBuilder
         */
        public DetailDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * field id.
         *
         * @param fieldId fieldId
         * @return DetailDOBuilder
         */
        public DetailDOBuilder fieldId(final String fieldId) {
            this.fieldId = fieldId;
            return this;
        }

        /**
         * example.
         *
         * @param example fieldId
         * @return DetailDOBuilder
         */
        public DetailDOBuilder example(final Boolean example) {
            this.example = example;
            return this;
        }

        /**
         * fieldValue.
         *
         * @param fieldValue fieldValue
         * @return DetailDOBuilder
         */
        public DetailDOBuilder fieldValue(final String fieldValue) {
            this.fieldValue = fieldValue;
            return this;
        }

        /**
         * valueDesc.
         *
         * @param valueDesc valueDesc
         * @return DetailDOBuilder
         */
        public DetailDOBuilder valueDesc(final String valueDesc) {
            this.valueDesc = valueDesc;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated dateCreated
         * @return DetailDOBuilder
         */
        public DetailDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated dateUpdated
         * @return DetailDOBuilder
         */
        public DetailDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * build.
         *
         * @return detailDO
         */
        public DetailDO build() {
            DetailDO detailDO = new DetailDO();
            detailDO.setId(this.id);
            detailDO.setFieldId(this.fieldId);
            detailDO.setExample(this.example);
            detailDO.setFieldValue(this.fieldValue);
            detailDO.setValueDesc(this.valueDesc);
            detailDO.setDateCreated(this.dateCreated);
            detailDO.setDateUpdated(this.dateUpdated);
            return detailDO;
        }

    }
}
