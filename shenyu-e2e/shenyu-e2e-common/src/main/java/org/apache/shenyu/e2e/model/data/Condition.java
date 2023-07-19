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

package org.apache.shenyu.e2e.model.data;

import com.fasterxml.jackson.annotation.JsonValue;
import org.jetbrains.annotations.NotNull;

/**
 * The request condition.
 */
public final class Condition {

    @NotNull
    private ParamType paramType;
    
    @NotNull
    private Operator operator;
    
    private String paramName;
    
    private String paramValue;

    /**
     * The type of param.
     */
    public enum ParamType {

        POST("post"),

        /**
         * request method.
         */
        METHOD("req_method"),

        URI("uri"),

        QUERY("query"),

        HEADER("header"),

        COOKIE("cookie"),
    
        IP("ip"),

        HOST("host"),

        DOMAIN("domain");
    
        private final String alias;

        /**
         * set alias.
         *
         * @param alias alias
         */
        ParamType(final String alias) {
            this.alias = alias;
        }

        /**
         * get alias.
         *
         * @return alias
         */
        @JsonValue
        public String getAlias() {
            return alias;
        }
    }
    
    public enum Operator {

        MATCH("match"),

        EQUAL("="),

        REGEX("regex"),

        CONTAINS("contains"),

        TIME_BEFORE("TimeBefore"),

        TIME_AFTER("TimeAfter"),

        EXCLUDE("exclude"),

        STARTS_WITH("startsWith"),

        ENDS_WITH("endsWith"),

        PATH_PATTERN("pathPattern");

        private final String alias;

        /**
         * set alias.
         *
         * @param alias alias
         */
        Operator(final String alias) {
            this.alias = alias;
        }

        /**
         * get alias.
         *
         * @return alias
         */
        @JsonValue
        public String getAlias() {
            return alias;
        }
    }

    /**
     * builder constructor.
     *
     * @param builder builder
     */
    private Condition(final Builder builder) {
        this.paramType = builder.paramType;
        this.operator = builder.operator;
        this.paramName = builder.paramName;
        this.paramValue = builder.paramValue;
    }

    /**
     * class builder.
     *
     * @return Builder
     */
    public static Builder builder() {
        return new Builder();
    }

    /**
     * set paramType.
     *
     * @param paramType paramType
     */
    public void setParamType(final @NotNull ParamType paramType) {
        this.paramType = paramType;
    }

    /**
     * get paramType.
     *
     * @return paramType
     */
    public @NotNull ParamType getParamType() {
        return paramType;
    }

    /**
     * set operator.
     *
     * @param operator operator
     */
    public void setOperator(final @NotNull Operator operator) {
        this.operator = operator;
    }

    /**
     * get operator.
     *
     * @return operator
     */
    public @NotNull Operator getOperator() {
        return operator;
    }

    /**
     * set paramName.
     *
     * @param paramName paramName
     */
    public void setParamName(final String paramName) {
        this.paramName = paramName;
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
     * set paramValue.
     *
     * @param paramValue paramValue
     */
    public void setParamValue(final String paramValue) {
        this.paramValue = paramValue;
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
     * class builder.
     */
    public static final class Builder {

        @NotNull
        private ParamType paramType;

        @NotNull
        private Operator operator;

        private String paramName;

        private String paramValue;

        /**
         * no args constructor.
         */
        private Builder() {
        }

        /**
         * build new Object.
         *
         * @return Condition
         */
        public Condition build() {
            return new Condition(this);
        }

        /**
         * build paramType.
         *
         * @param paramType paramType
         * @return this
         */
        public Builder paramType(final ParamType paramType) {
            this.paramType = paramType;
            return this;
        }

        /**
         * build operator.
         *
         * @param operator operator
         * @return this
         */
        public Builder operator(final Operator operator) {
            this.operator = operator;
            return this;
        }

        /**
         * build paramName.
         *
         * @param paramName paramName
         * @return this
         */
        public Builder paramName(final String paramName) {
            this.paramName = paramName;
            return this;
        }

        /**
         * build paramValue.
         *
         * @param paramValue paramValue
         * @return paramValue
         */
        public Builder paramValue(final String paramValue) {
            this.paramValue = paramValue;
            return this;
        }
    }
}
