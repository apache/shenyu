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

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;

/**
 * this is rule condition view to web front.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
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

    /**
     * build ruleConditionVO.
     *
     * @param ruleConditionDO {@linkplain RuleConditionDO}
     * @return {@linkplain RuleConditionVO}
     */
    public static RuleConditionVO buildRuleConditionVO(final RuleConditionDO ruleConditionDO) {
        ParamTypeEnum paramTypeEnum = ParamTypeEnum.getParamTypeEnumByName(ruleConditionDO.getParamType());
        OperatorEnum operatorEnum = OperatorEnum.getOperatorEnumByAlias(ruleConditionDO.getOperator());
        return new RuleConditionVO(ruleConditionDO.getId(), ruleConditionDO.getRuleId(), ruleConditionDO.getParamType(), paramTypeEnum == null ? null : paramTypeEnum.getName(),
                ruleConditionDO.getOperator(), operatorEnum == null ? null : operatorEnum.name(), ruleConditionDO.getParamName(), ruleConditionDO.getParamValue(),
                DateUtils.localDateTimeToString(ruleConditionDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(ruleConditionDO.getDateUpdated().toLocalDateTime()));
    }
}
