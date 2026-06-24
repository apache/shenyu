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

package org.apache.shenyu.common.dto.convert.rule;

import java.util.Objects;

/**
 * Gray release condition.
 */
public class GrayCondition {

    /**
     * parameter type: "header", "cookie", "query", "ip".
     */
    private String paramType;

    /**
     * parameter name, e.g. "X-Canary", "version".
     * Ignored when paramType is "ip".
     */
    private String paramName;

    /**
     * match operator: "=", "match", "regex", "contains", "exclude",
     * "startsWith", "endsWith", "isBlank".
     */
    private String operator;

    /**
     * expected value, e.g. "true", "v2", "192\\.168\\..*".
     */
    private String paramValue;

    /**
     * Instantiates a new Gray condition.
     */
    public GrayCondition() {
    }

    /**
     * Instantiates a new Gray condition.
     *
     * @param paramType  the param type
     * @param paramName  the param name
     * @param operator   the operator
     * @param paramValue the param value
     */
    public GrayCondition(final String paramType, final String paramName, final String operator, final String paramValue) {
        this.paramType = paramType;
        this.paramName = paramName;
        this.operator = operator;
        this.paramValue = paramValue;
    }

    /**
     * Gets param type.
     *
     * @return the param type
     */
    public String getParamType() {
        return paramType;
    }

    /**
     * Sets param type.
     *
     * @param paramType the param type
     */
    public void setParamType(final String paramType) {
        this.paramType = paramType;
    }

    /**
     * Gets param name.
     *
     * @return the param name
     */
    public String getParamName() {
        return paramName;
    }

    /**
     * Sets param name.
     *
     * @param paramName the param name
     */
    public void setParamName(final String paramName) {
        this.paramName = paramName;
    }

    /**
     * Gets operator.
     *
     * @return the operator
     */
    public String getOperator() {
        return operator;
    }

    /**
     * Sets operator.
     *
     * @param operator the operator
     */
    public void setOperator(final String operator) {
        this.operator = operator;
    }

    /**
     * Gets param value.
     *
     * @return the param value
     */
    public String getParamValue() {
        return paramValue;
    }

    /**
     * Sets param value.
     *
     * @param paramValue the param value
     */
    public void setParamValue(final String paramValue) {
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
        GrayCondition that = (GrayCondition) o;
        return Objects.equals(paramType, that.paramType)
                && Objects.equals(paramName, that.paramName)
                && Objects.equals(operator, that.operator)
                && Objects.equals(paramValue, that.paramValue);
    }

    @Override
    public int hashCode() {
        return Objects.hash(paramType, paramName, operator, paramValue);
    }

    @Override
    public String toString() {
        return "GrayCondition{"
                + "paramType='" + paramType + '\''
                + ", paramName='" + paramName + '\''
                + ", operator='" + operator + '\''
                + ", paramValue='" + paramValue + '\''
                + '}';
    }
}
