/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.entity;

import lombok.Data;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.RuleConditionDTO;
import org.dromara.soul.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.Date;

/**
 * RuleConditionDO.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Data
public class RuleConditionDO extends BaseDO {

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

    /**
     * build ruleConditionDO.
     *
     * @param ruleConditionDTO {@linkplain RuleConditionDTO}
     * @return {@linkplain RuleConditionDO}
     */
    public static RuleConditionDO buildRuleConditionDO(final RuleConditionDTO ruleConditionDTO) {
        if (ruleConditionDTO != null) {
            RuleConditionDO ruleConditionDO = new RuleConditionDO();
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            if (StringUtils.isEmpty(ruleConditionDTO.getId())) {
                ruleConditionDO.setId(UUIDUtils.getInstance().generateShortUuid());
                ruleConditionDO.setDateCreated(currentTime);
            } else {
                ruleConditionDO.setId(ruleConditionDTO.getId());
            }

            ruleConditionDO.setParamType(ruleConditionDTO.getParamType());
            ruleConditionDO.setRuleId(ruleConditionDTO.getRuleId());
            ruleConditionDO.setOperator(ruleConditionDTO.getOperator());
            ruleConditionDO.setParamName(ruleConditionDTO.getParamName());
            ruleConditionDO.setParamValue(ruleConditionDTO.getParamValue());
            ruleConditionDO.setDateUpdated(currentTime);
            return ruleConditionDO;
        }
        return null;
    }
}
