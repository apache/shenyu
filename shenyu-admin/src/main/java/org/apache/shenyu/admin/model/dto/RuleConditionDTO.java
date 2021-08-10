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

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.Objects;

/**
 * this is rule condition from by web front.
 */
public final class RuleConditionDTO implements Serializable {

    private static final long serialVersionUID = -1883819174316303659L;

    /**
     * primary key.
     */
    private String id;

    /**
     * rule id.
     */
    private String ruleId;

    /**
     * parameter type.
     */
    @NotNull
    private String paramType;

    /**
     * match operator.
     */
    @NotNull
    private String operator;

    /**
     * parameter name.
     */
    @NotNull
    private String paramName;

    /**
     * parameter value.
     */
    @NotNull
    private String paramValue;

    public RuleConditionDTO() {
    }

    public RuleConditionDTO(final String id,
                            final String ruleId,
                            @NotNull final String paramType,
                            @NotNull final String operator,
                            @NotNull final String paramName,
                            @NotNull final String paramValue) {
        this.id = id;
        this.ruleId = ruleId;
        this.paramType = paramType;
        this.operator = operator;
        this.paramName = paramName;
        this.paramValue = paramValue;
    }

    /**
     * Gets the value of id.
     *
     * @return the value of id
     */
    public String getId() {
        return id;
    }

    /**
     * Sets the id.
     *
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
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
    public static RuleConditionDTO.RuleConditionDTOBuilder builder() {
        return new RuleConditionDTO.RuleConditionDTOBuilder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof RuleConditionDTO)) {
            return false;
        }
        RuleConditionDTO that = (RuleConditionDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(ruleId, that.ruleId)
                && Objects.equals(paramType, that.paramType)
                && Objects.equals(operator, that.operator)
                && Objects.equals(paramName, that.paramName)
                && Objects.equals(paramValue, that.paramValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, ruleId, paramType, operator, paramName, paramValue);
    }

    public static final class RuleConditionDTOBuilder {

        private String id;

        private String ruleId;

        private String paramType;

        private String operator;

        private String paramName;

        private String paramValue;

        private RuleConditionDTOBuilder() {
        }

        /**
         * id.
         *
         * @param id the id.
         * @return RuleConditionDTOBuilder.
         */
        public RuleConditionDTOBuilder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * ruleId.
         *
         * @param ruleId the ruleId.
         * @return RuleConditionDTOBuilder.
         */
        public RuleConditionDTOBuilder ruleId(final String ruleId) {
            this.ruleId = ruleId;
            return this;
        }

        /**
         * paramType.
         *
         * @param paramType the paramType.
         * @return RuleConditionDTOBuilder.
         */
        public RuleConditionDTOBuilder paramType(final String paramType) {
            this.paramType = paramType;
            return this;
        }

        /**
         * operator.
         *
         * @param operator the operator.
         * @return RuleConditionDTOBuilder.
         */
        public RuleConditionDTOBuilder operator(final String operator) {
            this.operator = operator;
            return this;
        }

        /**
         * paramName.
         *
         * @param paramName the paramName.
         * @return RuleConditionDTOBuilder.
         */
        public RuleConditionDTOBuilder paramName(final String paramName) {
            this.paramName = paramName;
            return this;
        }

        /**
         * paramValue.
         *
         * @param paramValue paramValue.
         * @return RuleConditionDTOBuilder.
         */
        public RuleConditionDTOBuilder paramValue(final String paramValue) {
            this.paramValue = paramValue;
            return this;
        }

        /**
         * build method.
         *
         * @return build object.
         */
        public RuleConditionDTO build() {
            return new RuleConditionDTO(id, ruleId, paramType, operator, paramName, paramValue);
        }
    }
}
