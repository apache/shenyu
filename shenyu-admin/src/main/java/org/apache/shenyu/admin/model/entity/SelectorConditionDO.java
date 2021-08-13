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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * SelectorConditionDO.
 */
public final class SelectorConditionDO extends BaseDO {

    private static final long serialVersionUID = 756287802698140201L;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * parameter type.
     */
    private String paramType;

    /**
     * match operator.
     */
    private String operator;

    /**
     * parameter name.
     */
    private String paramName;

    /**
     * parameter value.
     */
    private String paramValue;

    public SelectorConditionDO() {
    }

    public SelectorConditionDO(final String selectorId, final String paramType, final String operator, final String paramName, final String paramValue) {
        this.selectorId = selectorId;
        this.paramType = paramType;
        this.operator = operator;
        this.paramName = paramName;
        this.paramValue = paramValue;
    }

    /**
     * Gets the value of selectorId.
     *
     * @return the value of selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * Sets the selectorId.
     *
     * @param selectorId selectorId
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }

    /**
     * Gets the value of paramType.
     *
     * @return the value of paramType
     */
    public String getParamType() {
        return paramType;
    }

    /**
     * Sets the paramType.
     *
     * @param paramType paramType
     */
    public void setParamType(final String paramType) {
        this.paramType = paramType;
    }

    /**
     * Gets the value of operator.
     *
     * @return the value of operator
     */
    public String getOperator() {
        return operator;
    }

    /**
     * Sets the operator.
     *
     * @param operator operator
     */
    public void setOperator(final String operator) {
        this.operator = operator;
    }

    /**
     * Gets the value of paramName.
     *
     * @return the value of paramName
     */
    public String getParamName() {
        return paramName;
    }

    /**
     * Sets the paramName.
     *
     * @param paramName paramName
     */
    public void setParamName(final String paramName) {
        this.paramName = paramName;
    }

    /**
     * Gets the value of paramValue.
     *
     * @return the value of paramValue
     */
    public String getParamValue() {
        return paramValue;
    }

    /**
     * Sets the paramValue.
     *
     * @param paramValue paramValue
     */
    public void setParamValue(final String paramValue) {
        this.paramValue = paramValue;
    }

    /**
     * builder method.
     *
     * @return builder object.
     */
    public static SelectorConditionDO.SelectorConditionDOBuilder builder() {
        return new SelectorConditionDO.SelectorConditionDOBuilder();
    }

    /**
     * build selectorConditionDO.
     *
     * @param selectorConditionDTO {@linkplain SelectorConditionDTO}
     * @return {@linkplain SelectorConditionDO}
     */
    public static SelectorConditionDO buildSelectorConditionDO(final SelectorConditionDTO selectorConditionDTO) {
        return Optional.ofNullable(selectorConditionDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            SelectorConditionDO selectorConditionDO = SelectorConditionDO.builder()
                    .paramType(item.getParamType())
                    .selectorId(item.getSelectorId())
                    .operator(item.getOperator())
                    .paramName(item.getParamName())
                    .paramValue(item.getParamValue())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                selectorConditionDO.setId(UUIDUtils.getInstance().generateShortUuid());
                selectorConditionDO.setDateCreated(currentTime);
            } else {
                selectorConditionDO.setId(item.getId());
            }
            return selectorConditionDO;
        }).orElse(null);
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
        SelectorConditionDO that = (SelectorConditionDO) o;
        return Objects.equals(selectorId, that.selectorId)
                && Objects.equals(paramType, that.paramType)
                && Objects.equals(operator, that.operator)
                && Objects.equals(paramName, that.paramName)
                && Objects.equals(paramValue, that.paramValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), selectorId, paramType, operator, paramName, paramValue);
    }

    public static final class SelectorConditionDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String selectorId;

        private String paramType;

        private String operator;

        private String paramName;

        private String paramValue;

        private SelectorConditionDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return SelectorConditionDOBuilder.
         */
        public SelectorConditionDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return SelectorConditionDOBuilder.
         */
        public SelectorConditionDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return SelectorConditionDOBuilder.
         */
        public SelectorConditionDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * selectorId.
         *
         * @param selectorId the selectorId.
         * @return SelectorConditionDOBuilder.
         */
        public SelectorConditionDOBuilder selectorId(final String selectorId) {
            this.selectorId = selectorId;
            return this;
        }

        /**
         * paramType.
         *
         * @param paramType the paramType.
         * @return SelectorConditionDOBuilder.
         */
        public SelectorConditionDOBuilder paramType(final String paramType) {
            this.paramType = paramType;
            return this;
        }

        /**
         * operator.
         *
         * @param operator the operator.
         * @return SelectorConditionDOBuilder.
         */
        public SelectorConditionDOBuilder operator(final String operator) {
            this.operator = operator;
            return this;
        }

        /**
         * paramName.
         *
         * @param paramName the paramName.
         * @return SelectorConditionDOBuilder.
         */
        public SelectorConditionDOBuilder paramName(final String paramName) {
            this.paramName = paramName;
            return this;
        }

        /**
         * paramValue.
         *
         * @param paramValue the paramValue.
         * @return SelectorConditionDOBuilder.
         */
        public SelectorConditionDOBuilder paramValue(final String paramValue) {
            this.paramValue = paramValue;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public SelectorConditionDO build() {
            SelectorConditionDO selectorConditionDO = new SelectorConditionDO();
            selectorConditionDO.setId(id);
            selectorConditionDO.setDateCreated(dateCreated);
            selectorConditionDO.setDateUpdated(dateUpdated);
            selectorConditionDO.setSelectorId(selectorId);
            selectorConditionDO.setParamType(paramType);
            selectorConditionDO.setOperator(operator);
            selectorConditionDO.setParamName(paramName);
            selectorConditionDO.setParamValue(paramValue);
            return selectorConditionDO;
        }
    }
}
