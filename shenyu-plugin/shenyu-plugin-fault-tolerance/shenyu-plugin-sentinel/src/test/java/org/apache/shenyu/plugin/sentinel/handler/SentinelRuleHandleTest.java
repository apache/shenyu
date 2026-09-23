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

package org.apache.shenyu.plugin.sentinel.handler;

import com.alibaba.csp.sentinel.slots.block.degrade.DegradeRule;
import com.alibaba.csp.sentinel.slots.block.degrade.DegradeRuleManager;
import com.alibaba.csp.sentinel.slots.block.flow.FlowRule;
import com.alibaba.csp.sentinel.slots.block.flow.FlowRuleManager;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.SentinelHandle;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

@ExtendWith(MockitoExtension.class)
public final class SentinelRuleHandleTest {

    private SentinelRuleHandle sentinelRuleHandle;

    @BeforeEach
    public void setUp() {
        FlowRuleManager.loadRules(Collections.emptyList());
        DegradeRuleManager.loadRules(Collections.emptyList());
        sentinelRuleHandle = new SentinelRuleHandle();
    }

    @Test
    public void pluginNamedTest() {
        assertEquals(PluginEnum.SENTINEL.getName(), sentinelRuleHandle.pluginNamed());
    }

    @Test
    public void removeRule() {
        RuleData data = createRuleData("removeRule");
        sentinelRuleHandle.handlerRule(data);
        FlowRule flowRule = FlowRuleManager.getRules().get(0);
        assertThat(flowRule.getCount(), is(10.0));
        assertThat(flowRule.getResource(), is("sentinel_removeRule"));
        DegradeRule degradeRule = DegradeRuleManager.getRules().get(0);
        assertThat(degradeRule.getCount(), is(1.0));
        assertThat(degradeRule.getResource(), is("sentinel_removeRule"));
        sentinelRuleHandle.removeRule(data);
        assertTrue(FlowRuleManager.getRules().isEmpty());
        assertTrue(DegradeRuleManager.getRules().isEmpty());
    }

    @Test
    public void handlerRuleShouldNotLoseConcurrentUpdates() throws Exception {
        int ruleCount = 16;
        ExecutorService executor = Executors.newFixedThreadPool(ruleCount);
        CountDownLatch ready = new CountDownLatch(ruleCount);
        CountDownLatch start = new CountDownLatch(1);
        List<Future<?>> futures = new ArrayList<>();
        try {
            for (int i = 0; i < ruleCount; i++) {
                RuleData ruleData = createRuleData("concurrent-" + i);
                futures.add(executor.submit(() -> {
                    ready.countDown();
                    start.await();
                    sentinelRuleHandle.handlerRule(ruleData);
                    return null;
                }));
            }
            assertTrue(ready.await(5, TimeUnit.SECONDS));
            start.countDown();
            for (Future<?> future : futures) {
                future.get(10, TimeUnit.SECONDS);
            }

            assertEquals(ruleCount, FlowRuleManager.getRules().size());
            assertEquals(ruleCount, DegradeRuleManager.getRules().size());
        } finally {
            executor.shutdownNow();
        }
    }

    private RuleData createRuleData(final String ruleId) {
        RuleData data = new RuleData();
        data.setSelectorId("sentinel");
        data.setId(ruleId);
        SentinelHandle sentinelHandle = new SentinelHandle();
        sentinelHandle.setFlowRuleCount(10);
        sentinelHandle.setFlowRuleGrade(0);
        sentinelHandle.setFlowRuleControlBehavior(0);
        sentinelHandle.setDegradeRuleCount(1d);
        sentinelHandle.setDegradeRuleGrade(0);
        sentinelHandle.setDegradeRuleTimeWindow(5);
        sentinelHandle.setDegradeRuleMinRequestAmount(5);
        sentinelHandle.setDegradeRuleStatIntervals(10);
        sentinelHandle.setDegradeRuleSlowRatioThreshold(0.5d);
        data.setHandle(GsonUtils.getGson().toJson(sentinelHandle));
        return data;
    }
}
