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
import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.dto.RuleData;
import org.dromara.soul.common.utils.UUIDUtils;

import java.sql.Timestamp;
import java.util.List;

/**
 * RuleDO.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Data
public class RuleDO extends BaseDO {

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
        if (ruleDTO != null) {
            RuleDO ruleDO = new RuleDO();
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            if (StringUtils.isEmpty(ruleDTO.getId())) {
                ruleDO.setId(UUIDUtils.getInstance().generateShortUuid());
                ruleDO.setDateCreated(currentTime);
            } else {
                ruleDO.setId(ruleDTO.getId());
            }

            ruleDO.setSelectorId(ruleDTO.getSelectorId());
            ruleDO.setMatchMode(ruleDTO.getMatchMode());
            ruleDO.setName(ruleDTO.getName());
            ruleDO.setEnabled(ruleDTO.getEnabled());
            ruleDO.setLoged(ruleDTO.getLoged());
            ruleDO.setSort(ruleDTO.getSort());
            ruleDO.setHandle(ruleDTO.getHandle());
            ruleDO.setDateUpdated(currentTime);
            return ruleDO;
        }
        return null;
    }

    public static RuleData transFrom(final RuleDO ruleDO, final String pluginName, final List<ConditionData> conditionDataList) {
        return new RuleData(ruleDO.getId(),
                ruleDO.getName(),
                pluginName,
                ruleDO.getSelectorId(),
                ruleDO.getMatchMode(),
                ruleDO.getSort(),
                ruleDO.getEnabled(),
                ruleDO.getLoged(),
                ruleDO.getHandle(),
                conditionDataList);
    }
}
