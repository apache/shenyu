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

import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.common.enums.MatchModeEnum;
import org.apache.shenyu.common.utils.DateUtils;

import java.io.Serializable;
import java.util.List;

/**
 * this is rule view to web front.
 */
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
     * match restful.
     */
    private Boolean matchRestful;
    
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

    public RuleVO() {
    }

    public RuleVO(final String id,
                  final String selectorId,
                  final Integer matchMode,
                  final String matchModeName,
                  final String name,
                  final Boolean enabled,
                  final Boolean loged,
                  final Integer sort,
                  final String handle,
                  final Boolean matchRestful,
                  final List<RuleConditionVO> ruleConditions,
                  final String dateCreated,
                  final String dateUpdated) {
        this.id = id;
        this.selectorId = selectorId;
        this.matchMode = matchMode;
        this.matchModeName = matchModeName;
        this.name = name;
        this.enabled = enabled;
        this.loged = loged;
        this.sort = sort;
        this.handle = handle;
        this.matchRestful = matchRestful;
        this.ruleConditions = ruleConditions;
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
     * Gets the value of matchMode.
     *
     * @return the value of matchMode
     */
    public Integer getMatchMode() {
        return matchMode;
    }

    /**
     * Sets the matchMode.
     *
     * @param matchMode matchMode
     */
    public void setMatchMode(final Integer matchMode) {
        this.matchMode = matchMode;
    }

    /**
     * Gets the value of matchModeName.
     *
     * @return the value of matchModeName
     */
    public String getMatchModeName() {
        return matchModeName;
    }

    /**
     * Sets the matchModeName.
     *
     * @param matchModeName matchModeName
     */
    public void setMatchModeName(final String matchModeName) {
        this.matchModeName = matchModeName;
    }

    /**
     * Gets the value of name.
     *
     * @return the value of name
     */
    public String getName() {
        return name;
    }

    /**
     * Sets the name.
     *
     * @param name name
     */
    public void setName(final String name) {
        this.name = name;
    }

    /**
     * Gets the value of enabled.
     *
     * @return the value of enabled
     */
    public Boolean getEnabled() {
        return enabled;
    }

    /**
     * Sets the enabled.
     *
     * @param enabled enabled
     */
    public void setEnabled(final Boolean enabled) {
        this.enabled = enabled;
    }

    /**
     * Gets the value of loged.
     *
     * @return the value of loged
     */
    public Boolean getLoged() {
        return loged;
    }

    /**
     * Sets the loged.
     *
     * @param loged loged
     */
    public void setLoged(final Boolean loged) {
        this.loged = loged;
    }

    /**
     * Gets the value of sort.
     *
     * @return the value of sort
     */
    public Integer getSort() {
        return sort;
    }

    /**
     * Sets the sort.
     *
     * @param sort sort
     */
    public void setSort(final Integer sort) {
        this.sort = sort;
    }

    /**
     * Gets the value of handle.
     *
     * @return the value of handle
     */
    public String getHandle() {
        return handle;
    }

    /**
     * Sets the handle.
     *
     * @param handle handle
     */
    public void setHandle(final String handle) {
        this.handle = handle;
    }
    
    /**
     * get match restful.
     *
     * @return matchRestful
     */
    public Boolean getMatchRestful() {
        return matchRestful;
    }
    
    /**
     * set match restful.
     *
     * @param matchRestful matchRestful
     */
    public void setMatchRestful(final Boolean matchRestful) {
        this.matchRestful = matchRestful;
    }
    
    /**
     * Gets the value of ruleConditions.
     *
     * @return the value of ruleConditions
     */
    public List<RuleConditionVO> getRuleConditions() {
        return ruleConditions;
    }

    /**
     * Sets the ruleConditions.
     *
     * @param ruleConditions ruleConditions
     */
    public void setRuleConditions(final List<RuleConditionVO> ruleConditions) {
        this.ruleConditions = ruleConditions;
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
                ruleDO.getName(), ruleDO.getEnabled(), ruleDO.getLoged(), ruleDO.getSort(), ruleDO.getHandle(),
                ruleDO.getMatchRestful(), ruleConditions,
                DateUtils.localDateTimeToString(ruleDO.getDateCreated().toLocalDateTime()),
                DateUtils.localDateTimeToString(ruleDO.getDateUpdated().toLocalDateTime()));
    }
}
