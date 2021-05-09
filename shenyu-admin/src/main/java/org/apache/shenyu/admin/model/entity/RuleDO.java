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

package org.apache.shenyu.admin.model.entity;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.SuperBuilder;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

/**
 * RuleDO.
 */
@Data
@SuperBuilder
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public final class RuleDO extends BaseDO {

    private static final long serialVersionUID = 8050178277098166539L;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * rule name.
     */
    private String name;

    /**
     * whether enabled.
     */
    private Boolean enabled;

    /**
     * whether loged.
     */
    private Boolean loged;

    /**
     * sort type.
     */
    private Integer sort;

    /**
     * process logic.
     */
    private String handle;

    /**
     * build ruleDO.
     *
     * @param ruleDTO {@linkplain RuleDTO}
     * @return {@linkplain RuleDO}
     */
    public static RuleDO buildRuleDO(final RuleDTO ruleDTO) {
        return Optional.ofNullable(ruleDTO).map(item -> {
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            RuleDO ruleDO = RuleDO.builder()
                    .selectorId(item.getSelectorId())
                    .matchMode(item.getMatchMode())
                    .name(item.getName())
                    .enabled(item.getEnabled())
                    .loged(item.getLoged())
                    .sort(item.getSort())
                    .handle(item.getHandle())
                    .dateUpdated(currentTime)
                    .build();
            if (StringUtils.isEmpty(item.getId())) {
                ruleDO.setId(UUIDUtils.getInstance().generateShortUuid());
                ruleDO.setDateCreated(currentTime);
            } else {
                ruleDO.setId(item.getId());
            }
            return ruleDO;
        }).orElse(null);
    }

    /**
     * Trans from rule data.
     *
     * @param ruleDO            the rule do
     * @param pluginName        the plugin name
     * @param conditionDataList the condition data list
     * @return the rule data
     */
    public static RuleData transFrom(final RuleDO ruleDO, final String pluginName, final List<ConditionData> conditionDataList) {
        return RuleData.builder()
                .id(ruleDO.getId())
                .name(ruleDO.getName())
                .pluginName(pluginName)
                .selectorId(ruleDO.getSelectorId())
                .matchMode(ruleDO.getMatchMode())
                .sort(ruleDO.getSort())
                .enabled(ruleDO.getEnabled())
                .loged(ruleDO.getLoged())
                .handle(ruleDO.getHandle())
                .conditionDataList(conditionDataList)
                .build();
    }
}
