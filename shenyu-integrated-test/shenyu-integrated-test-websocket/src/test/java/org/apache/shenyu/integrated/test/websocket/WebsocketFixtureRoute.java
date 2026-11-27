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

package org.apache.shenyu.integrated.test.websocket;

import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;

import java.io.IOException;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

final class WebsocketFixtureRoute {

    private WebsocketFixtureRoute() {
    }

    static void install(final PluginEnum plugin, final String route, final int port, final String protocol) throws IOException {
        assertEquals("success", AbstractPluginDataInit.initPlugin(plugin.getName(), ""));
        ConditionData path = new ConditionData();
        path.setParamType(ParamTypeEnum.URI.getName());
        path.setOperator(OperatorEnum.EQ.getAlias());
        path.setParamValue(route);
        List<ConditionData> conditions = List.of(path);

        RuleLocalData rule = new RuleLocalData();
        rule.setRuleName("websocket-fixture" + route);
        rule.setMatchMode(0);
        rule.setConditionDataList(conditions);
        rule.setRuleHandler("{\"loadBalance\":\"roundRobin\",\"retry\":0,\"timeout\":5000}");

        String upstream = "[{\"upstreamUrl\":\"websocket-fixture:" + port + "\",\"protocol\":\"" + protocol + "\","
                + "\"weight\":50,\"warmup\":0,\"status\":true}]";
        assertEquals("success", AbstractPluginDataInit.initSelectorAndRules(plugin.getName(), upstream, conditions, List.of(rule)));
    }
}
