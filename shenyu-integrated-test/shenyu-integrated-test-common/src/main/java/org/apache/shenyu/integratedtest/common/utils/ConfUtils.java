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

package org.apache.shenyu.integratedtest.common.utils;

import com.google.common.collect.Lists;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.convert.rule.RuleHandle;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;

import java.util.Collections;
import java.util.List;
import java.util.Objects;

public class ConfUtils {

    /**
     * generate singleton Condition List.
     *
     * @param paramType    paramType
     * @param operatorType operatorType
     * @param paramValue   paramValue
     * @return ConditionList
     */
    public static List<ConditionData> singletonConditionList(final ParamTypeEnum paramType,
                                                             final OperatorEnum operatorType,
                                                             final String paramValue) {
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType(paramType.getName());
        conditionData.setOperator(operatorType.getAlias());
        conditionData.setParamValue(paramValue);
        return Collections.singletonList(conditionData);
    }

    /**
     * generate singleton Condition List that has a URI-EQ Condition.
     *
     * @param path path
     * @return ConditionList
     */
    public static List<ConditionData> singletonURIEqConditionList(final String path) {
        return singletonConditionList(ParamTypeEnum.URI, OperatorEnum.EQ, path);
    }

    /**
     * generate singleton Condition List that has a URI-MATCH Condition.
     *
     * @param path path
     * @return ConditionList
     */
    public static List<ConditionData> singletonURIMatchConditionList(final String path) {
        return singletonConditionList(ParamTypeEnum.URI, OperatorEnum.MATCH, path);
    }

    /**
     * generate singleton ruleLocalData List.
     *
     * @param ruleLocalData ruleLocalData
     * @return RuleLocalDataList
     */
    public static List<RuleLocalData> singletonRuleLocalDataList(final RuleLocalData ruleLocalData) {
        return Lists.newArrayList(ruleLocalData);
    }

    /**
     * generate singleton ruleLocalData List.
     *
     * @param ruleHandle        ruleHandle
     * @param conditionDataList conditionDataList
     * @return RuleLocalDataList
     */
    public static List<RuleLocalData> singletonRuleLocalDataList(final RuleHandle ruleHandle,
                                                                 final List<ConditionData> conditionDataList) {
        return singletonRuleLocalDataList(ruleLocalData(ruleHandle, conditionDataList));
    }

    /**
     * generate ruleLocalData.
     *
     * @param ruleHandle        ruleHandle
     * @param conditionDataList ConditionDataList
     * @return ruleLocalData
     */
    public static RuleLocalData ruleLocalData(final RuleHandle ruleHandle,
                                              final List<ConditionData> conditionDataList) {
        return ruleLocalData(Objects.isNull(ruleHandle) ? null : JsonUtils.toJson(ruleHandle),
                conditionDataList);
    }

    /**
     * generate ruleLocalData.
     *
     * @param ruleHandle        ruleHandle
     * @param conditionDataList ConditionDataList
     * @return ruleLocalData
     */
    public static RuleLocalData ruleLocalData(final String ruleHandle,
                                              final List<ConditionData> conditionDataList) {
        final RuleLocalData ruleLocalData = new RuleLocalData();
        ruleLocalData.setRuleHandler(ruleHandle);
        ruleLocalData.setConditionDataList(conditionDataList);
        return ruleLocalData;
    }
}
