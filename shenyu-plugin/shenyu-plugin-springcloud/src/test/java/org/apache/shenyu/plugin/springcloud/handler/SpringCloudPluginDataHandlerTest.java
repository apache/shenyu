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

package org.apache.shenyu.plugin.springcloud.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.rule.impl.SpringCloudRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DivideUpstream;
import org.apache.shenyu.common.dto.convert.selector.SpringCloudSelectorHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.function.Supplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * The Test Case For SpringCloudPluginDataHandler.
 */
public final class SpringCloudPluginDataHandlerTest {

    private SpringCloudPluginDataHandler springCloudPluginDataHandler;

    private SelectorData selectorData = new SelectorData();

    private RuleData ruleData = new RuleData();

    @BeforeEach
    public void setUp() {
        this.springCloudPluginDataHandler = new SpringCloudPluginDataHandler();
        this.selectorData = new SelectorData();
    }

    @Test
    public void testHandlerSelector() throws NoSuchFieldException, IllegalAccessException {
        List<DivideUpstream> divideUpstreams = new ArrayList<>();
        divideUpstreams.add(new DivideUpstream());
        final SpringCloudSelectorHandle springCloudSelectorHandle = SpringCloudSelectorHandle.builder()
                .serviceId("serviceId")
                .divideUpstreams(divideUpstreams)
                .build();
        selectorData = SelectorData.builder()
                .handle(GsonUtils.getInstance().toJson(springCloudSelectorHandle))
                .id("1")
                .build();
        springCloudPluginDataHandler.handlerSelector(selectorData);
        UpstreamCacheManager instance = UpstreamCacheManager.getInstance();
        Field field = instance.getClass().getDeclaredField("UPSTREAM_MAP");
        field.setAccessible(true);
        Map<String, List<Upstream>> map = (Map<String, List<Upstream>>) field.get(instance);
        Assertions.assertNotNull(map.get("1"));
    }

    @Test
    public void testRemoveSelector() throws NoSuchFieldException, IllegalAccessException {
        selectorData.setId("1");
        List<Upstream> upstreamList = Stream.of(3)
                .map(weight -> Upstream.builder()
                        .url("mock-" + weight)
                        .build())
                .collect(Collectors.toList());
        UpstreamCacheManager instance = UpstreamCacheManager.getInstance();
        instance.submit("1", upstreamList);
        Field field = instance.getClass().getDeclaredField("UPSTREAM_MAP");
        field.setAccessible(true);
        Map<String, List<Upstream>> map = (Map<String, List<Upstream>>) field.get(instance);
        Assertions.assertNotNull(map.get("1"));
        springCloudPluginDataHandler.removeSelector(selectorData);
        Assertions.assertNull(map.get("1"));
    }

    @Test
    public void testHandlerRule() {
        ruleData.setSelectorId("1");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        ruleData.setName("test");
        springCloudPluginDataHandler.handlerRule(ruleData);
        Supplier<CommonHandleCache<String, SpringCloudRuleHandle>> cache = SpringCloudPluginDataHandler.RULE_CACHED;
        Assertions.assertNotEquals(cache.get().obtainHandle("1_test"), null);
    }

    @Test
    public void testRemoveRule() {
        ruleData.setSelectorId("1");
        ruleData.setHandle("{\"urlPath\":\"test\"}");
        ruleData.setName("test");
        Supplier<CommonHandleCache<String, SpringCloudRuleHandle>> cache = SpringCloudPluginDataHandler.RULE_CACHED;
        cache.get().cachedHandle("1_test", new SpringCloudRuleHandle());
        Assertions.assertNotEquals(cache.get().obtainHandle("1_test"), null);
        springCloudPluginDataHandler.removeRule(ruleData);
        Assertions.assertEquals(cache.get().obtainHandle("1_test"), null);
    }

    @Test
    public void testPluginNamed() {
        Assertions.assertEquals(springCloudPluginDataHandler.pluginNamed(), "springCloud");
    }
}
