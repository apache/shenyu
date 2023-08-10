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

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonValue;
import org.jetbrains.annotations.NotNull;

import java.util.Date;

/**
 * The request condition.
 */
public final class Condition {

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * The type of param.
     */
    @NotNull
    private ParamType paramType;

    /**
     * The type of param.
     */
    private String paramTypeName;

    /**
     * The type of operator.
     */
    @NotNull
    private Operator operator;

    /**
     * The name of param.
     */
    private String paramName;

    /**
     * The value of param.
     */
    private String paramValue;

    /**
     * The name of operator.
     */
    private String operatorName;

    /**
     * dateCreated.
     */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date dateCreated;

    /**
     * dateUpdated.
     */
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date dateUpdated;

    /**
     * no args constructor.
     */
    public Condition() {
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
        this.id = builder.id;
        this.selectorId = builder.selectorId;
        this.paramTypeName = builder.paramTypeName;
        this.operatorName = builder.operatorName;
        this.dateCreated = builder.dateCreated;
        this.dateUpdated = builder.dateUpdated;
    }

    /**
     * get paramType.
     *
     * @return paramType
     */
    public Date getDateCreated() {
        return dateCreated;
    }

    /**
     * set paramType.
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final Date dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * get paramType.
     * @return paramType
     */
    public Date getDateUpdated() {
        return dateUpdated;
    }

    /**
     * set paramType.
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final Date dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

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
     * get id.
     * @return id
     */
    public String getId() {
        return id;
    }

    /**
     * set id.
     * @param id id
     */
    public void setId(final String id) {
        this.id = id;
    }

    /**
     * get selectorId.
     * @return selectorId
     */
    public String getSelectorId() {
        return selectorId;
    }

    /**
     * set selectorId.
     * @param selectorId selectorId
     */
    public void setSelectorId(final String selectorId) {
        this.selectorId = selectorId;
    }

    /**
     * get paramTypeName.
     * @return paramTypeName
     */
    public String getParamTypeName() {
        return paramTypeName;
    }

    /**
     * set paramTypeName.
     * @param paramTypeName paramTypeName
     */
    public void setParamTypeName(final String paramTypeName) {
        this.paramTypeName = paramTypeName;
    }

    /**
     * get operatorName.
     * @return operatorName
     */
    public String getOperatorName() {
        return operatorName;
    }

    /**
     * set operatorName.
     * @param operatorName operatorName
     */
    public void setOperatorName(final String operatorName) {
        this.operatorName = operatorName;
    }

    /**
     * class builder.
     */
    public static final class Builder {


        /**
         * primary key.
         */
        private String id;

        /**
         * selector id.
         */
        private String selectorId;

        /**
         * The type of param.
         */
        private String paramTypeName;

        /**
         * The type of param.
         */
        @NotNull
        private ParamType paramType;

        /**
         * The type of operator.
         */
        @NotNull
        private Operator operator;

        /**
         * The name of param.
         */
        private String paramName;

        /**
         * The value of param.
         */
        private String paramValue;

        /**
         * The name of operator.
         */
        private String operatorName;

        /**
         * dateCreated.
         */
        @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss.SSS")
        private Date dateCreated;

        /**
         * dateUpdated.
         */
        @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss.SSS")
        private Date dateUpdated;

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
         * build paramName.
         *
         * @param paramValue paramValue
         * @return this
         */
        public Builder paramValue(final String paramValue) {
            this.paramValue = paramValue;
            return this;
        }

        /**
         * build paramValue.
         *
         * @param id id
         * @return id
         */
        public Builder id(final String id) {
            this.id = id;
            return this;
        }

        /**
         * build selectorId.
         *
         * @param selectorId selectorId
         * @return selectorId
         */
        public Builder selectorId(final String selectorId) {
            this.selectorId = selectorId;
            return this;
        }

        /**
         * build paramTypeName.
         *
         * @param paramTypeName paramTypeName
         * @return paramTypeName
         */
        public Builder paramTypeName(final String paramTypeName) {
            this.paramTypeName = paramTypeName;
            return this;
        }

        /**
         * build operatorName.
         *
         * @param operatorName operatorName
         * @return operatorName
         */
        public Builder operatorName(final String operatorName) {
            this.operatorName = operatorName;
            return this;
        }

        /**
         * build operatorName.
         *
         * @param dateCreated dateCreated
         * @return dateCreated
         */
        public Builder dateCreated(final Date dateCreated) {
            this.dateCreated = dateCreated;
            return this;
        }

        /**
         * build operatorName.
         *
         * @param dateUpdated dateUpdated
         * @return dateUpdated
         */
        public Builder dateUpdated(final Date dateUpdated) {
            this.dateUpdated = dateUpdated;
            return this;
        }
    }
}
