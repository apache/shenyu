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

package org.apache.shenyu.sync.data.api;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.junit.jupiter.api.Test;
import org.mockito.InOrder;

import java.util.List;

import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.mock;

/**
 * Existing subscribers keep their refresh and subscription callbacks.
 */
class PluginDataSubscriberTest {

    @Test
    void pluginRefreshRetainsLegacyOrder() {
        PluginDataSubscriber subscriber = mock(PluginDataSubscriber.class, CALLS_REAL_METHODS);
        PluginData first = PluginData.builder().name("first").build();
        PluginData second = PluginData.builder().name("second").build();
        List<PluginData> batch = List.of(first, second);
        subscriber.onPluginRefresh(batch);
        InOrder order = inOrder(subscriber);
        order.verify(subscriber).onPluginRefresh(batch);
        order.verify(subscriber).refreshPluginDataSelf(batch);
        order.verify(subscriber).onSubscribe(first);
        order.verify(subscriber).onSubscribe(second);
        order.verifyNoMoreInteractions();
    }

    @Test
    void selectorRefreshRetainsLegacyOrder() {
        PluginDataSubscriber subscriber = mock(PluginDataSubscriber.class, CALLS_REAL_METHODS);
        SelectorData first = SelectorData.builder().name("first").build();
        SelectorData second = SelectorData.builder().name("second").build();
        List<SelectorData> batch = List.of(first, second);
        subscriber.onSelectorRefresh(batch);
        InOrder order = inOrder(subscriber);
        order.verify(subscriber).onSelectorRefresh(batch);
        order.verify(subscriber).refreshSelectorDataSelf(batch);
        order.verify(subscriber).onSelectorSubscribe(first);
        order.verify(subscriber).onSelectorSubscribe(second);
        order.verifyNoMoreInteractions();
    }

    @Test
    void ruleRefreshRetainsLegacyOrder() {
        PluginDataSubscriber subscriber = mock(PluginDataSubscriber.class, CALLS_REAL_METHODS);
        RuleData first = RuleData.builder().name("first").build();
        RuleData second = RuleData.builder().name("second").build();
        List<RuleData> batch = List.of(first, second);
        subscriber.onRuleRefresh(batch);
        InOrder order = inOrder(subscriber);
        order.verify(subscriber).onRuleRefresh(batch);
        order.verify(subscriber).refreshRuleDataSelf(batch);
        order.verify(subscriber).onRuleSubscribe(first);
        order.verify(subscriber).onRuleSubscribe(second);
        order.verifyNoMoreInteractions();
    }
}
