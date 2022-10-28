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
 * RuleLocalData.
 */
public class RuleLocalData {

    private String ruleName;

    private String ruleHandler;

    private Integer matchMode;

    private List<ConditionData> conditionDataList;

    /**
     * Gets rule name.
     *
     * @return the rule name
     */
    public String getRuleName() {
        return ruleName;
    }

    /**
     * Sets rule name.
     *
     * @param ruleName the rule name
     */
    public void setRuleName(final String ruleName) {
        this.ruleName = ruleName;
    }

    /**
     * Gets rule handler.
     *
     * @return the rule handler
     */
    public String getRuleHandler() {
        return ruleHandler;
    }

    /**
     * Sets rule handler.
     *
     * @param ruleHandler the rule handler
     */
    public void setRuleHandler(final String ruleHandler) {
        this.ruleHandler = ruleHandler;
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
}
