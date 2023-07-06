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

package org.apache.shenyu.integrated.test.http;

import com.google.gson.JsonObject;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.integratedtest.common.AbstractPluginDataInit;
import org.apache.shenyu.integratedtest.common.dto.OrderDTO;
import org.apache.shenyu.integratedtest.common.helper.HttpHelper;
import org.apache.shenyu.web.controller.LocalPluginController.RuleLocalData;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class DividePluginTest extends AbstractPluginDataInit {

    @Test
    public void testHelloWorld() throws Exception {
        OrderDTO user = new OrderDTO("123", "Tom");
        user = HttpHelper.INSTANCE.postGateway("/http/order/save", user, OrderDTO.class);
        assertEquals("hello world save order", user.getName());
    }
    
    @Test
    public void testDomain() throws IOException, InterruptedException {
        String selectorHandle = "[{\"upstreamHost\":\"localhost\",\"upstreamUrl\":\"jsonplaceholder.typicode.com\","
                + "\"protocol\":\"http://\",\"timestamp\":\"0\",\"weight\":50,\"warmup\":\"0\",\"status\":true}]";
        List<ConditionData> conditionData = Stream.of(1).map(weight -> {
            ConditionData data = new ConditionData();
            data.setParamType(ParamTypeEnum.URI.getName());
            data.setOperator(OperatorEnum.MATCH.getAlias());
            data.setParamValue("/posts");
            return data;
        }).collect(Collectors.toList());
        
        List<RuleLocalData> ruleLocalDataList = Stream.of(1).map(rule -> {
            RuleLocalData ruleLocalData = new RuleLocalData();
            ruleLocalData.setRuleName("test-domain");
            ruleLocalData.setMatchMode(0);
            ruleLocalData.setConditionDataList(conditionData);
            ruleLocalData.setRuleHandler("{\"loadBalance\":\"hash\",\"retryStrategy\":\"current\","
                    + "\"retry\":\"3\",\"timeout\":3000,\"headerMaxSize\":10240,\"requestMaxSize\":102400}");
            return ruleLocalData;
        }).collect(Collectors.toList());
        
        String message = initSelectorAndRules(PluginEnum.DIVIDE.getName(), selectorHandle, conditionData, ruleLocalDataList);
        assertThat(message, is("success"));
        TimeUnit.SECONDS.sleep(10);
        JsonObject request = new JsonObject();
        request.addProperty("userId", 1);
        JsonPlaceHolderUser user = HttpHelper.INSTANCE.postGateway("/posts", request, JsonPlaceHolderUser.class);
        assertEquals("1", user.getUserId());
    }
    
    public static class JsonPlaceHolderUser {
        private String id;
        
        private String userId;
        
        public JsonPlaceHolderUser() {
        }
        
        public JsonPlaceHolderUser(final String id, final String userId) {
            this.id = id;
            this.userId = userId;
        }
        
        /**
         * get id.
         *
         * @return id
         */
        public String getId() {
            return id;
        }
        
        /**
         * set id.
         *
         * @param id id
         */
        public void setId(final String id) {
            this.id = id;
        }
        
        /**
         * get user id.
         *
         * @return user id
         */
        public String getUserId() {
            return userId;
        }
        
        /**
         * set user id.
         *
         * @param userId user id
         */
        public void setUserId(final String userId) {
            this.userId = userId;
        }
    }
}
