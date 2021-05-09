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

import java.util.List;

import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.SelectorConditionDTO;
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.admin.model.entity.SelectorConditionDO;
import org.apache.shenyu.common.dto.ConditionData;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

/**
 * The interface Condition transfer.
 */
@Mapper
public interface ConditionTransfer {

    /**
     * The constant INSTANCE.
     */
    ConditionTransfer INSTANCE = Mappers.getMapper(ConditionTransfer.class);

    /**
     * Map to selector data condition data.
     *
     * @param selectorConditionDO the selector condition do
     * @return the condition data
     */
    ConditionData mapToSelectorDO(SelectorConditionDO selectorConditionDO);

    /**
     * Map to selector data condition data list.
     *
     * @param selectorConditionDOS the selector condition do list
     * @return the condition data list
     */
    List<ConditionData> mapToSelectorDOS(List<SelectorConditionDO> selectorConditionDOS);

    /**
     * Map to selector data dto condition data.
     *
     * @param selectorConditionDTO the selector condition dto
     * @return the condition data
     */
    ConditionData mapToSelectorDTO(SelectorConditionDTO selectorConditionDTO);

    /**
     * Map to rule data condition data.
     *
     * @param ruleConditionDO the rule condition do
     * @return the condition data
     */
    ConditionData mapToRuleDO(RuleConditionDO ruleConditionDO);

    /**
     * Map to rule data condition data.
     *
     * @param ruleConditionDTO the rule condition dto
     * @return the condition data
     */
    ConditionData mapToRuleDTO(RuleConditionDTO ruleConditionDTO);

}
