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

package org.apache.shenyu.examples.sdk.http.dto;

import org.apache.shenyu.common.dto.ConditionData;

import java.util.List;

/**
 * SelectorRulesData.
 */
public class SelectorRulesData {

    private String pluginName;

    private String selectorName;

    private Integer matchMode;

    private String selectorHandler;

    private List<ConditionData> conditionDataList;

    private List<RuleLocalData> ruleDataList;

    /**
     * Gets plugin name.
     *
     * @return the plugin name
     */
    public String getPluginName() {
        return pluginName;
    }

    /**
     * Sets plugin name.
     *
     * @param pluginName the plugin name
     */
    public void setPluginName(final String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * Gets selector name.
     *
     * @return the selector name
     */
    public String getSelectorName() {
        return selectorName;
    }

    /**
     * Sets selector name.
     *
     * @param selectorName the selector name
     */
    public void setSelectorName(final String selectorName) {
        this.selectorName = selectorName;
    }

    /**
     * Gets selector handler.
     *
     * @return the selector handler
     */
    public String getSelectorHandler() {
        return selectorHandler;
    }

    /**
     * Sets selector handler.
     *
     * @param selectorHandler the selector handler
     */
    public void setSelectorHandler(final String selectorHandler) {
        this.selectorHandler = selectorHandler;
    }

    /**
     * Gets match mode.
     *
     * @return the match mode
     */
    public Integer getMatchMode() {
        return matchMode;
    }

    /**
     * Sets match mode.
     *
     * @param matchMode the match mode
     */
    public void setMatchMode(final Integer matchMode) {
        this.matchMode = matchMode;
    }

    /**
     * Gets condition data list.
     *
     * @return the condition data list
     */
    public List<ConditionData> getConditionDataList() {
        return conditionDataList;
    }

    /**
     * Sets condition data list.
     *
     * @param conditionDataList the condition data list
     */
    public void setConditionDataList(final List<ConditionData> conditionDataList) {
        this.conditionDataList = conditionDataList;
    }

    /**
     * Gets rule data list.
     *
     * @return the rule data list
     */
    public List<RuleLocalData> getRuleDataList() {
        return ruleDataList;
    }

    /**
     * Sets rule data list.
     *
     * @param ruleDataList the rule data list
     */
    public void setRuleDataList(final List<RuleLocalData> ruleDataList) {
        this.ruleDataList = ruleDataList;
    }

}
