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
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Objects;
import java.util.Optional;

/**
 * RuleConditionDO.
 */
public final class RuleConditionDO extends BaseDO {

    private static final long serialVersionUID = -5652026882314490873L;

    /**
     * rule id.
     */
    private String ruleId;

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

    public RuleConditionDO() {
    }

    public RuleConditionDO(final String ruleId, final String paramType, final String operator, final String paramName, final String paramValue) {
        this.ruleId = ruleId;
        this.paramType = paramType;
        this.operator = operator;
        this.paramName = paramName;
        this.paramValue = paramValue;
    }

    /**
     * Gets the value of ruleId.
     *
     * @return the value of ruleId
     */
    public String getRuleId() {
        return ruleId;
    }

    /**
     * Sets the ruleId.
     *
     * @param ruleId ruleId
     */
    public void setRuleId(final String ruleId) {
        this.ruleId = ruleId;
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
    public static RuleConditionDO.RuleConditionDOBuilder builder() {
        return new RuleConditionDO.RuleConditionDOBuilder();
    }

    /**
     * build ruleConditionDO.
     *
     * @param ruleConditionDTO {@linkplain RuleConditionDTO}
     * @return {@linkplain RuleConditionDO}
     */
    public static RuleConditionDO buildRuleConditionDO(final RuleConditionDTO ruleConditionDTO) {
        return Optional.ofNullable(ruleConditionDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            RuleConditionDO ruleConditionDO = RuleConditionDO.builder()
                    .paramType(item.getParamType())
                    .ruleId(item.getRuleId())
                    .operator(item.getOperator())
                    .paramName(item.getParamName())
                    .paramValue(StringUtils.defaultIfBlank(item.getParamValue(), "").trim())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                ruleConditionDO.setId(UUIDUtils.getInstance().generateShortUuid());
                ruleConditionDO.setDateCreated(currentTime);
            } else {
                ruleConditionDO.setId(item.getId());
            }
            return ruleConditionDO;
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
        RuleConditionDO that = (RuleConditionDO) o;
        return Objects.equals(ruleId, that.ruleId)
                && Objects.equals(paramType, that.paramType)
                && Objects.equals(operator, that.operator)
                && Objects.equals(paramName, that.paramName)
                && Objects.equals(paramValue, that.paramValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(super.hashCode(), ruleId, paramType, operator, paramName, paramValue);
    }

    public static final class RuleConditionDOBuilder {

        private String id;

        private Timestamp dateCreated;

        private Timestamp dateUpdated;

        private String ruleId;

        private String paramType;

        private String operator;

        private String paramName;

        private String paramValue;

        private RuleConditionDOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return RuleConditionDOBuilder.
         */
        public RuleConditionDOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * dateCreated.
         *
         * @param dateCreated the dateCreated.
         * @return RuleConditionDOBuilder.
         */
        public RuleConditionDOBuilder dateCreated(final Timestamp dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * dateUpdated.
         *
         * @param dateUpdated the dateUpdated.
         * @return RuleConditionDOBuilder.
         */
        public RuleConditionDOBuilder dateUpdated(final Timestamp dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }

        /**
         * ruleId.
         *
         * @param ruleId the ruleId.
         * @return RuleConditionDOBuilder.
         */
        public RuleConditionDOBuilder ruleId(final String ruleId) {
            this.ruleId = ruleId;
            return this;
        }

        /**
         * paramType.
         *
         * @param paramType the paramType.
         * @return RuleConditionDOBuilder.
         */
        public RuleConditionDOBuilder paramType(final String paramType) {
            this.paramType = paramType;
            return this;
        }

        /**
         * operator.
         *
         * @param operator the operator.
         * @return RuleConditionDOBuilder.
         */
        public RuleConditionDOBuilder operator(final String operator) {
            this.operator = operator;
            return this;
        }

        /**
         * paramName.
         *
         * @param paramName the paramName.
         * @return RuleConditionDOBuilder.
         */
        public RuleConditionDOBuilder paramName(final String paramName) {
            this.paramName = paramName;
            return this;
        }

        /**
         * paramValue.
         *
         * @param paramValue the paramValue.
         * @return RuleConditionDOBuilder.
         */
        public RuleConditionDOBuilder paramValue(final String paramValue) {
            this.paramValue = paramValue;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public RuleConditionDO build() {
            RuleConditionDO ruleConditionDO = new RuleConditionDO(ruleId, paramType, operator, paramName, paramValue);
            ruleConditionDO.setId(id);
            ruleConditionDO.setDateCreated(dateCreated);
            ruleConditionDO.setDateUpdated(dateUpdated);
            return ruleConditionDO;
        }
    }
}
