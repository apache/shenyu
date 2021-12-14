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

package org.apache.shenyu.admin.model.vo;

import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Optional;

/**
 * this is rule condition view to web front.
 */
public class RuleConditionVO implements Serializable {

    private static final long serialVersionUID = -951311268188074239L;

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
    private String paramType;

    /**
     * parameter type name.
     */
    private String paramTypeName;

    /**
     * match operator.
     */
    private String operator;

    /**
     * match operator name.
     */
    private String operatorName;

    /**
     * parameter name.
     */
    private String paramName;

    /**
     * parameter value.
     */
    private String paramValue;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    public RuleConditionVO() {
    }

    public RuleConditionVO(final String id,
                           final String ruleId,
                           final String paramType,
                           final String paramTypeName,
                           final String operator,
                           final String operatorName,
                           final String paramName,
                           final String paramValue,
                           final String dateCreated,
                           final String dateUpdated) {
        this.id = id;
        this.ruleId = ruleId;
        this.paramType = paramType;
        this.paramTypeName = paramTypeName;
        this.operator = operator;
        this.operatorName = operatorName;
        this.paramName = paramName;
        this.paramValue = paramValue;
        this.dateCreated = dateCreated;
        this.dateUpdated = dateUpdated;
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
     * Gets the value of paramTypeName.
     *
     * @return the value of paramTypeName
     */
    public String getParamTypeName() {
        return paramTypeName;
    }

    /**
     * Sets the paramTypeName.
     *
     * @param paramTypeName paramTypeName
     */
    public void setParamTypeName(final String paramTypeName) {
        this.paramTypeName = paramTypeName;
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
     * Gets the value of operatorName.
     *
     * @return the value of operatorName
     */
    public String getOperatorName() {
        return operatorName;
    }

    /**
     * Sets the operatorName.
     *
     * @param operatorName operatorName
     */
    public void setOperatorName(final String operatorName) {
        this.operatorName = operatorName;
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
     * Gets the value of dateCreated.
     *
     * @return the value of dateCreated
     */
    public String getDateCreated() {
        return dateCreated;
    }

    /**
     * Sets the dateCreated.
     *
     * @param dateCreated dateCreated
     */
    public void setDateCreated(final String dateCreated) {
        this.dateCreated = dateCreated;
    }

    /**
     * Gets the value of dateUpdated.
     *
     * @return the value of dateUpdated
     */
    public String getDateUpdated() {
        return dateUpdated;
    }

    /**
     * Sets the dateUpdated.
     *
     * @param dateUpdated dateUpdated
     */
    public void setDateUpdated(final String dateUpdated) {
        this.dateUpdated = dateUpdated;
    }

    /**
     * build ruleConditionVO.
     *
     * @param ruleConditionDO {@linkplain RuleConditionDO}
     * @return {@linkplain RuleConditionVO}
     */
    public static RuleConditionVO buildRuleConditionVO(final RuleConditionDO ruleConditionDO) {
        ParamTypeEnum paramTypeEnum = ParamTypeEnum.getParamTypeEnumByName(ruleConditionDO.getParamType());
        OperatorEnum operatorEnum = OperatorEnum.getOperatorEnumByAlias(ruleConditionDO.getOperator());
        return new RuleConditionVO(ruleConditionDO.getId(), ruleConditionDO.getRuleId(), ruleConditionDO.getParamType(), 
                Optional.ofNullable(paramTypeEnum).map(ParamTypeEnum::getName).orElse(ruleConditionDO.getParamType()),
                ruleConditionDO.getOperator(), Optional.ofNullable(operatorEnum).map(OperatorEnum::getAlias).orElse(ruleConditionDO.getOperator()), 
                ruleConditionDO.getParamName(), ruleConditionDO.getParamValue(),
                DateUtils.localDateTimeToString(ruleConditionDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(ruleConditionDO.getDateUpdated().toLocalDateTime()));
    }
}
