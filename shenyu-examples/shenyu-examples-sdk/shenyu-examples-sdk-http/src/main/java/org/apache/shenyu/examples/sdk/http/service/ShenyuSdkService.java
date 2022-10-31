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

package org.apache.shenyu.examples.sdk.http.service;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.examples.sdk.http.api.ShenyuSdkApi;
import org.apache.shenyu.examples.sdk.http.dto.RuleLocalData;
import org.apache.shenyu.examples.sdk.http.dto.SelectorRulesData;
import org.apache.shenyu.examples.sdk.http.dto.ShenyuServerResponse;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Collections;

/**
 * ShenyuSdkService.
 */
@Service
public class ShenyuSdkService {
    @Resource
    private ShenyuSdkApi sdkApi;

    /**
     * call shenyu server.
     *
     * @return response
     */
    public ShenyuServerResponse callShenyu() {
        SelectorRulesData request = buildRequest();
        return sdkApi.selectorAndRules(request);
    }

    /**
     * buildRequest.
     *
     * @return SelectorRulesData
     */
    private static SelectorRulesData buildRequest() {
        SelectorRulesData selectorRulesData = new SelectorRulesData();
        selectorRulesData.setSelectorName("selector");
        selectorRulesData.setSelectorHandler("[{\"upstreamUrl\":\"127.0.0.1:8080\"}]");
        selectorRulesData.setPluginName("divide");
        ConditionData conditionData = new ConditionData();
        conditionData.setParamType("uri");
        conditionData.setOperator("match");
        conditionData.setParamValue("/**");
        selectorRulesData.setConditionDataList(Collections.singletonList(conditionData));
        RuleLocalData ruleLocalData = new RuleLocalData();
        ruleLocalData.setRuleHandler("{\"loadBalance\":\"random\"}");
        ruleLocalData.setConditionDataList(Collections.singletonList(conditionData));
        selectorRulesData.setRuleDataList(Collections.singletonList(ruleLocalData));
        return selectorRulesData;
    }
}
