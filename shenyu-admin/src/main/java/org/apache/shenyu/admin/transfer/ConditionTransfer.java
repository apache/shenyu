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

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

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
        return Optional.ofNullable(selectorConditionDO)
                .map(v -> {
                    ConditionData conditionData = new ConditionData();
                    conditionData.setParamType(v.getParamType());
                    conditionData.setOperator(v.getOperator());
                    conditionData.setParamName(v.getParamName());
                    conditionData.setParamValue(v.getParamValue());
                    return conditionData;
                })
                .orElse(null);
    }

    /**
     * Map to selector data condition data list.
     *
     * @param selectorConditionDOS the selector condition do list
     * @return the condition data list
     */
    public List<ConditionData> mapToSelectorDOS(final List<SelectorConditionDO> selectorConditionDOS) {
        return Optional.ofNullable(selectorConditionDOS)
                .map(v -> v.stream().map(this::mapToSelectorDO).collect(Collectors.toList()))
                .orElse(null);
    }

    /**
     * Map to selector data dto condition data.
     *
     * @param selectorConditionDTO the selector condition dto
     * @return the condition data
     */
    public ConditionData mapToSelectorDTO(final SelectorConditionDTO selectorConditionDTO) {
        return Optional.ofNullable(selectorConditionDTO)
                .map(v -> {
                    ConditionData conditionData = new ConditionData();
                    conditionData.setParamType(v.getParamType());
                    conditionData.setOperator(v.getOperator());
                    conditionData.setParamName(v.getParamName());
                    conditionData.setParamValue(v.getParamValue());
                    return conditionData;
                })
                .orElse(null);
    }

    /**
     * Map to rule data condition data.
     *
     * @param ruleConditionDO the rule condition do
     * @return the condition data
     */
    public ConditionData mapToRuleDO(final RuleConditionDO ruleConditionDO) {
        return Optional.ofNullable(ruleConditionDO)
                .map(v -> {
                    ConditionData conditionData = new ConditionData();
                    conditionData.setParamType(v.getParamType());
                    conditionData.setOperator(v.getOperator());
                    conditionData.setParamName(v.getParamName());
                    conditionData.setParamValue(v.getParamValue());
                    return conditionData;
                })
                .orElse(null);
    }

    /**
     * Map to rule data condition data.
     *
     * @param ruleConditionDTO the rule condition dto
     * @return the condition data
     */
    public ConditionData mapToRuleDTO(final RuleConditionDTO ruleConditionDTO) {
        return Optional.ofNullable(ruleConditionDTO)
                .map(v -> {
                    ConditionData conditionData = new ConditionData();
                    conditionData.setParamType(v.getParamType());
                    conditionData.setOperator(v.getOperator());
                    conditionData.setParamName(v.getParamName());
                    conditionData.setParamValue(v.getParamValue());
                    return conditionData;
                })
                .orElse(null);
    }

}
