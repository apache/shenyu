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

import org.apache.shenyu.admin.model.entity.SelectorConditionDO;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.Optional;

/**
 * this is selector condition view to web front.
 */
public class SelectorConditionVO implements Serializable {

    private static final long serialVersionUID = -9194910149899688578L;

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * parameter type code.
     */
    private String paramType;

    /**
     * parameter type name.
     */
    private String paramTypeName;

    /**
     * match operator code.
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
    
    public SelectorConditionVO() {
    }

    public SelectorConditionVO(final String id,
                               final String selectorId,
                               final String paramType,
                               final String paramTypeName,
                               final String operator,
                               final String operatorName,
                               final String paramName,
                               final String paramValue,
                               final String dateCreated,
                               final String dateUpdated) {
        this.id = id;
        this.selectorId = selectorId;
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
     * build selectorConditionVO.
     *
     * @param selectorConditionDO {@linkplain SelectorConditionDO}
     * @return {@linkplain SelectorConditionVO}
     */
    public static SelectorConditionVO buildSelectorConditionVO(final SelectorConditionDO selectorConditionDO) {
        ParamTypeEnum paramTypeEnum = ParamTypeEnum.getParamTypeEnumByName(selectorConditionDO.getParamType());
        OperatorEnum operatorEnum = OperatorEnum.getOperatorEnumByAlias(selectorConditionDO.getOperator());
        return new SelectorConditionVO(selectorConditionDO.getId(), selectorConditionDO.getSelectorId(), selectorConditionDO.getParamType(),
                Optional.ofNullable(paramTypeEnum).map(ParamTypeEnum::getName).orElse(selectorConditionDO.getParamType()), selectorConditionDO.getOperator(),
                Optional.ofNullable(operatorEnum).map(OperatorEnum::getAlias).orElse(selectorConditionDO.getOperator()), 
                selectorConditionDO.getParamName(), selectorConditionDO.getParamValue(),
                DateUtils.localDateTimeToString(selectorConditionDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(selectorConditionDO.getDateUpdated().toLocalDateTime()));
    }
}
