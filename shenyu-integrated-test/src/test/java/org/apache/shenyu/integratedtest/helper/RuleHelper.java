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
package org.apache.shenyu.integratedtest.helper;

import com.google.common.collect.Lists;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import org.apache.shenyu.integratedtest.dto.AdminResponse;
import org.apache.shenyu.integratedtest.dto.RuleConditionDTO;
import org.apache.shenyu.integratedtest.dto.RuleDTO;

import java.util.concurrent.atomic.AtomicInteger;

public class RuleHelper {

    public static final RuleHelper INSTANCE = new RuleHelper();

    private final AtomicInteger sort = new AtomicInteger(0);

    private RuleHelper() {
    }

    public AdminResponse<Object> createRateLimitRule(String selectorId, String name, String algorithm, int rate, String uri) throws Exception {
        RuleDTO ruleDTO = RuleDTO.builder()
                .enabled(true)
                .handle(String.format("{\"algorithmName\":\"%s\",\"replenishRate\":\"%d\",\"burstCapacity\":\"%d\"}", algorithm, rate, rate))
                .loged(true)
                .selectorId(selectorId)
                .sort(sort.incrementAndGet())
                .name(name)
                .ruleConditions(Lists.newArrayList(RuleConditionDTO.builder()
                        .operator("=")
                        .paramName("/")
                        .paramValue(uri)
                        .paramType("uri")
                        .build()))
                .matchMode(0)
                .build();
        return HttpHelper.INSTANCE.postAdmin("/rule", ruleDTO, new TypeToken<AdminResponse<Object>>() {
        }.getType());
    }
}
