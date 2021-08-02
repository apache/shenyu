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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.admin.model.entity.SelectorConditionDO;
import org.apache.shenyu.common.dto.ConditionData;

import java.util.ArrayList;
import java.util.List;

/**
 * The interface Condition transfer.
 */
public enum ConditionTransfer {

    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    /**
     * Map to selector data condition data.
     *
     * @param selectorConditionDO the selector condition do
     * @return the condition data
     */
    public ConditionData mapToSelectorDO(final SelectorConditionDO selectorConditionDO) {
        if (selectorConditionDO == null) {
            return null;
        }

        ConditionData conditionData = new ConditionData();

        conditionData.setParamType(selectorConditionDO.getParamType());
        conditionData.setOperator(selectorConditionDO.getOperator());
        conditionData.setParamName(selectorConditionDO.getParamName());
        conditionData.setParamValue(selectorConditionDO.getParamValue());

        return conditionData;
    }

    /**
     * Map to selector data condition data list.
     *
     * @param selectorConditionDOS the selector condition do list
     * @return the condition data list
     */
    public List<ConditionData> mapToSelectorDOS(final List<SelectorConditionDO> selectorConditionDOS) {
        if (selectorConditionDOS == null) {
            return null;
        }

        List<ConditionData> list = new ArrayList<ConditionData>(selectorConditionDOS.size());
        for (SelectorConditionDO selectorConditionDO : selectorConditionDOS) {
            list.add(mapToSelectorDO(selectorConditionDO));
        }

        return list;
    }

    /**
     * Map to selector data dto condition data.
     *
     * @param selectorConditionDTO the selector condition dto
     * @return the condition data
     */
    public ConditionData mapToSelectorDTO(final SelectorConditionDTO selectorConditionDTO) {
        if (selectorConditionDTO == null) {
            return null;
        }

        ConditionData conditionData = new ConditionData();

        conditionData.setParamType(selectorConditionDTO.getParamType());
        conditionData.setOperator(selectorConditionDTO.getOperator());
        conditionData.setParamName(selectorConditionDTO.getParamName());
        conditionData.setParamValue(selectorConditionDTO.getParamValue());

        return conditionData;
    }

    /**
     * Map to rule data condition data.
     *
     * @param ruleConditionDO the rule condition do
     * @return the condition data
     */
    public ConditionData mapToRuleDO(final RuleConditionDO ruleConditionDO) {
        if (ruleConditionDO == null) {
            return null;
        }

        ConditionData conditionData = new ConditionData();

        conditionData.setParamType(ruleConditionDO.getParamType());
        conditionData.setOperator(ruleConditionDO.getOperator());
        conditionData.setParamName(ruleConditionDO.getParamName());
        conditionData.setParamValue(ruleConditionDO.getParamValue());

        return conditionData;
    }

    /**
     * Map to rule data condition data.
     *
     * @param ruleConditionDTO the rule condition dto
     * @return the condition data
     */
    public ConditionData mapToRuleDTO(final RuleConditionDTO ruleConditionDTO) {
        if (ruleConditionDTO == null) {
            return null;
        }

        ConditionData conditionData = new ConditionData();

        conditionData.setParamType(ruleConditionDTO.getParamType());
        conditionData.setOperator(ruleConditionDTO.getOperator());
        conditionData.setParamName(ruleConditionDTO.getParamName());
        conditionData.setParamValue(ruleConditionDTO.getParamValue());

        return conditionData;
    }

}
