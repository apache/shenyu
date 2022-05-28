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
 * this is selector condition from by web front.
 */
public class SelectorConditionDTO implements Serializable {

    private static final long serialVersionUID = -7096949265173497354L;

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

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

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof SelectorConditionDTO)) {
            return false;
        }
        SelectorConditionDTO that = (SelectorConditionDTO) o;
        return Objects.equals(id, that.id)
                && Objects.equals(selectorId, that.selectorId)
                && Objects.equals(paramType, that.paramType)
                && Objects.equals(operator, that.operator)
                && Objects.equals(paramName, that.paramName)
                && Objects.equals(paramValue, that.paramValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, selectorId, paramType, operator, paramName, paramValue);
    }
}
