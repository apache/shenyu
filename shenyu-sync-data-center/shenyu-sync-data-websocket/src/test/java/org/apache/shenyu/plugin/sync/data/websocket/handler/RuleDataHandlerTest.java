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

package org.apache.shenyu.plugin.sync.data.websocket.handler;

import com.google.gson.Gson;
import org.apache.shenyu.common.dto.ConditionData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.core.Is.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

public final class RuleDataHandlerTest {

    private final PluginDataSubscriber subscriber;

    private final RuleDataHandler ruleDataHandler;

    public RuleDataHandlerTest() {
        subscriber = mock(PluginDataSubscriber.class);
        ruleDataHandler = new RuleDataHandler(subscriber);
    }

    @Test
    public void testConvert() {
        List<RuleData> ruleDataList = new LinkedList<>();
        ConditionData conditionData = new ConditionData();
        conditionData.setParamName("conditionName-" + 0);
        List<ConditionData> conditionDataList = Collections.singletonList(conditionData);
        ruleDataList.add(RuleData.builder().name("name1").enabled(true).conditionDataList(conditionDataList).build());
        ruleDataList.add(RuleData.builder().name("name2").selectorId("0").build());
        Gson gson = new Gson();
        String json = gson.toJson(ruleDataList);
        List<RuleData> convertedList = ruleDataHandler.convert(json);
        assertThat(convertedList, is(ruleDataList));
    }

    @Test
    public void testDoRefresh() {
        List<RuleData> ruleDataList = createFakeRuleDateObjects(3);
        ruleDataHandler.doRefresh(ruleDataList);
        verify(subscriber).refreshRuleDataSelf(ruleDataList);
        ruleDataList.forEach(verify(subscriber)::onRuleSubscribe);
    }

    @Test
    public void testDoUpdate() {
        List<RuleData> ruleDataList = createFakeRuleDateObjects(4);
        ruleDataHandler.doUpdate(ruleDataList);
        ruleDataList.forEach(verify(subscriber)::onRuleSubscribe);
    }

    @Test
    public void testDoDelete() {
        List<RuleData> ruleDataList = createFakeRuleDateObjects(3);
        ruleDataHandler.doDelete(ruleDataList);
        ruleDataList.forEach(verify(subscriber)::unRuleSubscribe);
    }

    private List<RuleData> createFakeRuleDateObjects(final int count) {
        List<RuleData> result = new LinkedList<>();
        for (int i = 1; i <= count; i++) {
            result.add(RuleData.builder().name("name-" + i).build());
        }
        return result;
    }
}
