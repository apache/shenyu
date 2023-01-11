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

import org.apache.shenyu.admin.mapper.DetailMapper;
import org.apache.shenyu.admin.model.entity.DetailDO;
import org.apache.shenyu.admin.validation.annotation.Existed;

import java.sql.Timestamp;
import java.util.Objects;

public class DetailDTO {

    private static final long serialVersionUID = 7247613164345326366L;

    @Existed(provider = DetailMapper.class, nullOfIgnore = true, message = "detail is not existed")
    private String id;

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
     * created time.
     */
    private Timestamp dateCreated;

    /**
     * updated time.
     */
    private Timestamp dateUpdated;

    /**
     * getId.
     *
     * @return id
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
     * getFieldId.
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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        DetailDTO detailDTO = (DetailDTO) o;
        return Objects.equals(id, detailDTO.id)
                && Objects.equals(fieldId, detailDTO.fieldId)
                && Objects.equals(example, detailDTO.example)
                && Objects.equals(fieldValue, detailDTO.fieldValue)
                && Objects.equals(valueDesc, detailDTO.valueDesc)
                && Objects.equals(dateCreated, detailDTO.dateCreated)
                && Objects.equals(dateUpdated, detailDTO.dateUpdated);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, fieldId, example, fieldValue, valueDesc, dateCreated, dateUpdated);
    }

    /**
     * builder.
     *
     * @return fieldDOBuilder
     */
    public static DetailDTO.DetailDTOBuilder builder() {
        return new DetailDTO.DetailDTOBuilder();
    }

    public static final class DetailDTOBuilder {

        private String id;

        private String fieldId;

        private Boolean example;

        private String fieldValue;

        private String valueDesc;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private DetailDTOBuilder() {

        }

        /**
         * id.
         *
         * @param id id
         * @return DetailDOBuilder
         */
        public DetailDTO.DetailDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * field id.
         *
         * @param fieldId fieldId
         * @return DetailDOBuilder
         */
        public DetailDTO.DetailDTOBuilder fieldId(final String fieldId) {
            this.fieldId = fieldId;
            return this;
        }

        /**
         * example.
         *
         * @param example fieldId
         * @return DetailDOBuilder
         */
        public DetailDTO.DetailDTOBuilder example(final Boolean example) {
            this.example = example;
            return this;
        }

        /**
         * fieldValue.
         *
         * @param fieldValue fieldValue
         * @return DetailDOBuilder
         */
        public DetailDTO.DetailDTOBuilder fieldValue(final String fieldValue) {
            this.fieldValue = fieldValue;
            return this;
        }

        /**
         * valueDesc.
         *
         * @param valueDesc valueDesc
         * @return DetailDOBuilder
         */
        public DetailDTO.DetailDTOBuilder valueDesc(final String valueDesc) {
            this.valueDesc = valueDesc;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated dateCreated
         * @return DetailDOBuilder
         */
        public DetailDTO.DetailDTOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated dateUpdated
         * @return DetailDOBuilder
         */
        public DetailDTO.DetailDTOBuilder dateUpdated(final Timestamp dateUpdated) {
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
