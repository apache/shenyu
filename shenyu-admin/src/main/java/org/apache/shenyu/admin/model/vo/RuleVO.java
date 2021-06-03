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
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.List;

/**
 * this is rule view to web front.
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class RuleVO implements Serializable {

    private static final long serialVersionUID = -672321762440577372L;

    /**
     * primary key.
     */
    private String id;

    /**
     * selector id.
     */
    private String selectorId;

    /**
     * match mode.
     */
    private Integer matchMode;

    /**
     * match mode name.
     */
    private String matchModeName;

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
     * rule conditions.
     */
    private List<RuleConditionVO> ruleConditions;

    /**
     * created time.
     */
    private String dateCreated;

    /**
     * updated time.
     */
    private String dateUpdated;

    /**
     * build ruleVO.
     *
     * @param ruleDO {@linkplain RuleDO}
     * @return {@linkplain RuleVO}
     */
    public static RuleVO buildRuleVO(final RuleDO ruleDO) {
        return buildRuleVO(ruleDO, null);
    }

    /**
     * build ruleVO.
     *
     * @param ruleDO         {@linkplain RuleDO}
     * @param ruleConditions {@linkplain List}
     * @return {@linkplain RuleVO}
     */
    public static RuleVO buildRuleVO(final RuleDO ruleDO, final List<RuleConditionVO> ruleConditions) {
        return new RuleVO(ruleDO.getId(), ruleDO.getSelectorId(), ruleDO.getMatchMode(), MatchModeEnum.getMatchModeByCode(ruleDO.getMatchMode()),
                ruleDO.getName(), ruleDO.getEnabled(), ruleDO.getLoged(), ruleDO.getSort(), ruleDO.getHandle(), ruleConditions,
                DateUtils.localDateTimeToString(ruleDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(ruleDO.getDateUpdated().toLocalDateTime()));
    }
}
