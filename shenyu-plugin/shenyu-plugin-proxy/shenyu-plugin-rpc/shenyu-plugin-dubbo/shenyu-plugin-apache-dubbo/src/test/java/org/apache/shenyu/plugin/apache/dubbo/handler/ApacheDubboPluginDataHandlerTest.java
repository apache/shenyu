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

package org.apache.shenyu.plugin.apache.dubbo.handler;

import com.google.common.cache.LoadingCache;
import org.apache.dubbo.common.URL;
import org.apache.dubbo.config.ConsumerConfig;
import org.apache.dubbo.config.ReferenceConfig;
import org.apache.dubbo.config.RegistryConfig;
import org.apache.dubbo.rpc.Invoker;
import org.apache.dubbo.rpc.RpcInvocation;
import org.apache.dubbo.rpc.service.GenericService;
import org.apache.dubbo.rpc.support.MockInvoker;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.plugin.DubboRegisterConfig;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.apache.dubbo.cache.ApacheDubboConfigCache;
import org.apache.shenyu.plugin.apache.dubbo.proxy.ApacheDubboGrayLoadBalance;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;

/**
 * The Test Case For ApacheDubboPluginDataHandler.
 */
public final class ApacheDubboPluginDataHandlerTest {

    private ApacheDubboPluginDataHandler apacheDubboPluginDataHandler;

    @BeforeEach
    public void setUp() {
        apacheDubboPluginDataHandler = new ApacheDubboPluginDataHandler();
    }

    @Test
    public void handlerPlugin() {
        DubboRegisterConfig dubboRegisterConfig = mock(DubboRegisterConfig.class);
        PluginData pluginData = PluginData.builder()
                .enabled(true)
                .config(GsonUtils.getInstance().toJson(dubboRegisterConfig))
                .build();
        apacheDubboPluginDataHandler.handlerPlugin(pluginData);
        pluginData.setConfig(null);
        apacheDubboPluginDataHandler.handlerPlugin(pluginData);
    }

    @Test
    public void pluginNamed() {
        assertThat(apacheDubboPluginDataHandler.pluginNamed(), is(PluginEnum.DUBBO.getName()));
    }

    @Test
    public void grayRoutingShouldPreserveSameAddressGroupsVersionsAndWeights() {
        SelectorData selectorData = new SelectorData();
        selectorData.setId("gray-logical-identity-selector");
        selectorData.setContinued(true);
        selectorData.setHandle("[{\"protocol\":\"dubbo://\",\"upstreamUrl\":\"127.0.0.1:37438\",\"group\":\"blue\",\"version\":\"v1\","
                + "\"weight\":1,\"gray\":true,\"status\":true,\"healthCheckEnabled\":false,\"warmup\":0},"
                + "{\"protocol\":\"dubbo://\",\"upstreamUrl\":\"127.0.0.1:37438\",\"group\":\"blue\",\"version\":\"v2\","
                + "\"weight\":2,\"gray\":true,\"status\":true,\"healthCheckEnabled\":false,\"warmup\":0},"
                + "{\"protocol\":\"dubbo://\",\"upstreamUrl\":\"127.0.0.1:37438\",\"group\":\"green\",\"version\":\"v2\","
                + "\"weight\":3,\"gray\":true,\"status\":true,\"healthCheckEnabled\":false,\"warmup\":0}]");
        RuleData ruleData = new RuleData();
        ruleData.setId("gray-logical-identity-rule");
        ruleData.setSelectorId(selectorData.getId());
        ruleData.setHandle("{\"loadBalance\":\"roundRobin\"}");
        try {
            apacheDubboPluginDataHandler.handlerSelector(selectorData);
            apacheDubboPluginDataHandler.handlerRule(ruleData);
            List<Upstream> cached = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorData.getId());
            assertEquals(3, cached.size());
            assertEquals(Map.of("blue:v1", 1, "blue:v2", 2, "green:v2", 3), cached.stream()
                    .collect(Collectors.toMap(upstream -> upstream.getGroup() + ":" + upstream.getVersion(), Upstream::getWeight)));

            MockInvoker<Object> blueV1 = new MockInvoker<>(URL.valueOf("dubbo://127.0.0.1:37438/TestService?group=blue&version=v1"), Object.class);
            MockInvoker<Object> blueV2 = new MockInvoker<>(URL.valueOf("dubbo://127.0.0.1:37438/TestService?group=blue&version=v2"), Object.class);
            MockInvoker<Object> greenV2 = new MockInvoker<>(URL.valueOf("dubbo://127.0.0.1:37438/TestService?group=green&version=v2"), Object.class);
            List<Invoker<Object>> invokers = List.of(blueV1, blueV2, greenV2);
            RpcInvocation invocation = new RpcInvocation();
            invocation.setMethodName("test");
            invocation.setAttachment(Constants.DUBBO_SELECTOR_ID, selectorData.getId());
            invocation.setAttachment(Constants.DUBBO_RULE_ID, ruleData.getId());
            invocation.setAttachment(Constants.DUBBO_REMOTE_ADDRESS, "127.0.0.1");
            ApacheDubboGrayLoadBalance loadBalance = new ApacheDubboGrayLoadBalance();
            // Attachments identify the configuration; each call advances weighted round robin's internal state.
            for (Invoker<Object> expected : List.of(greenV2, blueV2, blueV1, greenV2, blueV2, greenV2)) {
                assertSame(expected, loadBalance.select(invokers, blueV1.getUrl(), invocation));
            }
        } finally {
            apacheDubboPluginDataHandler.removeRule(ruleData);
            apacheDubboPluginDataHandler.removeSelector(selectorData);
        }
    }

    @Test
    @SuppressWarnings("unchecked")
    public void configUpdateShouldApplyBeforeDestroyingReferences() {
        final ApacheDubboConfigCache configCache = new ApacheDubboConfigCache();
        DubboRegisterConfig config = new DubboRegisterConfig();
        config.setProtocol("zookeeper");
        config.setRegister("zookeeper://127.0.0.1:2181");
        config.setGroup("old-group");
        config.setThreads(32);
        configCache.init(config);
        final LoadingCache<String, ReferenceConfig<GenericService>> references =
                (LoadingCache<String, ReferenceConfig<GenericService>>) ReflectionTestUtils.getField(configCache, "cache");
        ReferenceConfig<GenericService> reference = mock(ReferenceConfig.class);
        AtomicReference<String> groupAtRemoval = new AtomicReference<>();
        AtomicReference<Integer> threadsAtRemoval = new AtomicReference<>();
        doAnswer(invocation -> {
            groupAtRemoval.set(((RegistryConfig) ReflectionTestUtils.getField(configCache, "registryConfig")).getGroup());
            threadsAtRemoval.set(((ConsumerConfig) ReflectionTestUtils.getField(configCache, "consumerConfig")).getThreads());
            return null;
        }).when(reference).destroy();
        references.put("old-reference", reference);

        config.setGroup("new-group");
        config.setThreads(64);
        try (MockedStatic<ApacheDubboConfigCache> mocked = mockStatic(ApacheDubboConfigCache.class)) {
            mocked.when(ApacheDubboConfigCache::getInstance).thenReturn(configCache);
            apacheDubboPluginDataHandler.initConfigCache(config);
        }

        verify(reference).destroy();
        assertEquals("new-group", groupAtRemoval.get());
        assertEquals(64, threadsAtRemoval.get());
        assertTrue(references.asMap().isEmpty());
    }
}
