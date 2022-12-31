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

package org.apache.shenyu.integrated.test.http.combination;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.apache.shenyu.common.enums.OperatorEnum.MATCH;
import static org.apache.shenyu.common.enums.ParamTypeEnum.URI;
import static org.apache.shenyu.integratedtest.common.utils.ConfUtils.singletonConditionList;
import static org.apache.shenyu.integratedtest.common.utils.ConfUtils.singletonRuleLocalDataList;
import static org.apache.shenyu.integratedtest.common.utils.ConfUtils.singletonURIEqConditionList;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public final class JwtPluginTest extends AbstractPluginDataInit {

    private static final List<ConditionData> SELECTOR_CONDITION_LIST = singletonConditionList(URI, MATCH, "/http/**");

    private static final List<RuleLocalData> RULE_LOCAL_DATA_LIST = singletonRuleLocalDataList(null, singletonURIEqConditionList("/http/test/findByUserId"));

    @BeforeAll
    public static void setup() throws IOException {
        String pluginResult = initPlugin(PluginEnum.JWT.getName(), "{\"secretKey\":\"shenyu-test-shenyu-test-shenyu-test\"}");
        assertThat(pluginResult, is("success"));
        String selectorAndRulesResult = initSelectorAndRules(PluginEnum.JWT.getName(), "", SELECTOR_CONDITION_LIST, RULE_LOCAL_DATA_LIST);
        assertThat(selectorAndRulesResult, is("success"));
    }

    @Test
    public void testJwt() throws IOException {
        final String key = "shenyu-test-shenyu-test-shenyu-test";
        final String testPath = "/http/test/findByUserId?userId=1001";
        final String token = Jwts.builder().setId("1001").signWith(Keys.hmacShaKeyFor(key.getBytes(StandardCharsets.UTF_8)), SignatureAlgorithm.HS256).compact();
        Map<String, Object> headers = new HashMap<>();

        // send request with fake jwt
        headers.put("token", "fake.token.me");
        Map<String, Object> errorResponse = HttpHelper.INSTANCE.getFromGateway(testPath, headers, Map.class);
        assertThat(errorResponse.get("message"), is("Illegal authorization"));

        // send request with jwt
        headers.put("token", token);
        Map<String, Object> correctResponse = HttpHelper.INSTANCE.getFromGateway(testPath, headers, Map.class);
        assertThat(correctResponse.get("userId"), is("1001"));
    }

    @AfterAll
    public static void clean() throws IOException {
        cleanPluginData(PluginEnum.JWT.getName());
    }
}
