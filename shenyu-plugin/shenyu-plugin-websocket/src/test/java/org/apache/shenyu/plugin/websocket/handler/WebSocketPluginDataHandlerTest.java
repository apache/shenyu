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

package org.apache.shenyu.plugin.websocket.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.WebSocketRuleHandle;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The Test Case For WebSocketPluginDataHandler.
 */
public class WebSocketPluginDataHandlerTest {

    private WebSocketPluginDataHandler webSocketPluginDataHandler;

    private SelectorData selectorData = new SelectorData();

    private RuleData ruleData = new RuleData();

    private List<Upstream> upstreamList;

    @BeforeEach
    public void setUp() {
        this.webSocketPluginDataHandler = new WebSocketPluginDataHandler();
        this.upstreamList = Stream.of(3)
                .map(weight -> Upstream.builder()
                        .url("mock-" + weight)
                        .build())
                .collect(Collectors.toList());
        selectorData.setId("1");
        ruleData.setSelectorId("1");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        ruleData.setId("test");
    }

    @Test
    public void testRemoveSelector() throws NoSuchFieldException, IllegalAccessException {
        UpstreamCacheManager instance = UpstreamCacheManager.getInstance();
        instance.submit("1", upstreamList);
        Field field = instance.getClass().getDeclaredField("UPSTREAM_MAP");
        field.setAccessible(true);
        Map<String, List<Upstream>> map = (Map<String, List<Upstream>>) field.get(instance);
        Assertions.assertNotEquals(map.get("1"), null);
        webSocketPluginDataHandler.removeSelector(selectorData);
        Assertions.assertEquals(map.get("1"), null);
    }

    @Test
    public void testRemoveRule() {
        Supplier<CommonHandleCache<String, WebSocketRuleHandle>> cache = WebSocketPluginDataHandler.CACHED_HANDLE;
        cache.get().cachedHandle("1_test", new WebSocketRuleHandle());
        Assertions.assertNotEquals(cache.get().obtainHandle("1_test"), null);
        webSocketPluginDataHandler.removeRule(ruleData);
        Assertions.assertEquals(cache.get().obtainHandle("1_test"), null);
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(webSocketPluginDataHandler.pluginNamed(), "websocket");
    }
}

