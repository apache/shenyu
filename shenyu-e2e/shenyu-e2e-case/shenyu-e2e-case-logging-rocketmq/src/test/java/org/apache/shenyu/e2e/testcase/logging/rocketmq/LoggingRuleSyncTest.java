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

package org.apache.shenyu.e2e.testcase.logging.rocketmq;

import org.apache.shenyu.e2e.client.gateway.GatewayClient;
import org.apache.shenyu.e2e.model.data.RuleCacheData;
import org.apache.shenyu.e2e.model.data.SelectorCacheData;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Properties;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertTrue;

class LoggingRuleSyncTest {

    @Test
    void waitsForNewIdsEvenWhenCacheSizesMatch() {
        AtomicInteger selectorReads = new AtomicInteger();
        AtomicInteger ruleReads = new AtomicInteger();
        GatewayClient gateway = new GatewayClient("sync-test", "gateway", "http://localhost", new Properties()) {
            @Override
            public List<SelectorCacheData> getSelectorCache() {
                SelectorCacheData selector = new SelectorCacheData();
                selector.setId(selectorReads.incrementAndGet() < 3 ? "old-selector" : "new-selector");
                return List.of(selector);
            }

            @Override
            public List<RuleCacheData> getRuleCache() {
                RuleCacheData rule = new RuleCacheData();
                rule.setId(ruleReads.incrementAndGet() < 3 ? "old-rule" : "new-rule");
                return List.of(rule);
            }
        };

        DividePluginTest.waitForLoggingRules(gateway, List.of("new-selector"), List.of("new-rule"));

        assertTrue(selectorReads.get() >= 3);
        assertTrue(ruleReads.get() >= 3);
    }

    @Test
    void acceptsAlreadySynchronizedRulesAlongsideUnrelatedData() {
        GatewayClient gateway = new GatewayClient("sync-test", "gateway", "http://localhost", new Properties()) {
            @Override
            public List<SelectorCacheData> getSelectorCache() {
                SelectorCacheData selector = new SelectorCacheData();
                selector.setId("new-selector");
                return List.of(selector, new SelectorCacheData());
            }

            @Override
            public List<RuleCacheData> getRuleCache() {
                RuleCacheData rule = new RuleCacheData();
                rule.setId("new-rule");
                return List.of(rule, new RuleCacheData());
            }
        };

        DividePluginTest.waitForLoggingRules(gateway, List.of("new-selector"), List.of("new-rule"));
    }
}
