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

package org.apache.shenyu.common.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;

import java.util.Objects;

/**
 * ConditionDTO.
 *
 * @since 2.0.0
 */
public class ConditionData {

    /**
     * {@linkplain org.apache.shenyu.common.enums.ParamTypeEnum}.
     */
    private String paramType;

    /**
     * {@linkplain org.apache.shenyu.common.enums.OperatorEnum}.
     */
    private String operator;

    /**
     * param name.
     */
    private String paramName;

    /**
     * param value.
     */
    private String paramValue;

    @JsonIgnore
    private transient boolean frozen;

    /**
     * No args constructor.
     */
    public ConditionData() {
    }

    /**
     * Copy the values without copying the frozen state.
     *
     * @param source source data
     */
    ConditionData(final ConditionData source) {
        this.paramType = source.paramType;
        this.operator = source.operator;
        this.paramName = source.paramName;
        this.paramValue = source.paramValue;
    }

    /**
     * Whether this data is a read-only snapshot.
     *
     * @return whether mutations are rejected
     */
    public final boolean frozen() {
        return frozen;
    }

    /**
     * Seal a fully initialized snapshot for publication.
     */
    final void markFrozen() {
        frozen = true;
    }

    /**
     * Reject mutations of a published snapshot.
     */
    final void checkMutable() {
        if (frozen) {
            throw new UnsupportedOperationException("DTO snapshot is read-only");
        }
    }

    /**
     * get paramType.
     *
     * @return paramType
     */
    public String getParamType() {
        return paramType;
    }

    /**
     * set paramType.
     *
     * @param paramType paramType
     */
    public void setParamType(final String paramType) {
        checkMutable();
        this.paramType = paramType;
    }

    /**
     * get operator.
     *
     * @return operator
     */
    public String getOperator() {
        return operator;
    }

    /**
     * set operator.
     *
     * @param operator operator
     */
    public void setOperator(final String operator) {
        checkMutable();
        this.operator = operator;
    }

    /**
     * get paramName.
     *
     * @return paramName
     */
    public String getParamName() {
        return paramName;
    }

    /**
     * set paramName.
     *
     * @param paramName paramName
     */
    public void setParamName(final String paramName) {
        checkMutable();
        this.paramName = paramName;
    }

    /**
     * get paramValue.
     *
     * @return paramValue
     */
    public String getParamValue() {
        return paramValue;
    }

    /**
     * set paramValue.
     *
     * @param paramValue paramValue
     */
    public void setParamValue(final String paramValue) {
        checkMutable();
        this.paramValue = paramValue;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        ConditionData that = (ConditionData) o;
        return Objects.equals(paramType, that.paramType) && Objects.equals(operator, that.operator)
                && Objects.equals(paramName, that.paramName) && Objects.equals(paramValue, that.paramValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(paramType, operator, paramName, paramValue);
    }

    @Override
    public String toString() {
        return "ConditionData{"
                + "paramType='"
                + paramType
                + '\''
                + ", operator='"
                + operator
                + '\''
                + ", paramName='"
                + paramName
                + '\''
                + ", paramValue='"
                + paramValue
                + '\''
                + '}';
    }
}
