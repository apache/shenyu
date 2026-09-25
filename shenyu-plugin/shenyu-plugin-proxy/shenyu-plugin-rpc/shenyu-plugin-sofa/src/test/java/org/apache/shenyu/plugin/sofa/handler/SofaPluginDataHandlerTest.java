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

package org.apache.shenyu.plugin.sofa.handler;

import com.alipay.sofa.rpc.config.ConsumerConfig;
import com.google.common.cache.LoadingCache;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.SofaRegisterConfig;
import org.apache.shenyu.common.dto.convert.selector.SofaUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.Singleton;
import org.apache.shenyu.plugin.sofa.cache.ApplicationConfigCache;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.MethodOrderer;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestMethodOrder;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * SofaPluginDataHandlerTest.
 */
@ExtendWith(MockitoExtension.class)
@TestMethodOrder(MethodOrderer.Alphanumeric.class)
public final class SofaPluginDataHandlerTest {

    private final String registryConfig = "{\"protocol\":\"zookeeper\",\"register\":\"127.0.0.1:2181\"}";

    private SofaPluginDataHandler sofaPluginDataHandler;

    @BeforeEach
    public void setUp() {
        sofaPluginDataHandler = new SofaPluginDataHandler();
        ApplicationConfigCache.getInstance().invalidateAll();
    }

    @Test
    public void testPluginEnable() {
        PluginData pluginData = new PluginData("", "", registryConfig, "1", true, null);
        sofaPluginDataHandler.handlerPlugin(pluginData);
        assertEquals("127.0.0.1:2181", Singleton.INST.get(SofaRegisterConfig.class).getRegister());
    }

    @Test
    public void testPluginDisable() {
        PluginData pluginData = new PluginData("", "", registryConfig, "1", false, null);
        sofaPluginDataHandler.handlerPlugin(pluginData);
        assertNull(Singleton.INST.get(SofaRegisterConfig.class));
    }

    @Test
    public void testPluginNamed() {
        assertEquals(sofaPluginDataHandler.pluginNamed(), PluginEnum.SOFA.getName());
    }

    @Test
    public void testHandlerSelectorSkipsInvalidationWhenUpstreamUnchanged() throws Exception {
        String selectorId = "1532088187335";
        String handle = "{\"register\":\"zookeeper://127.0.0.1:2181\",\"protocol\":\"bolt\"}";
        seedReference(selectorId, handle);

        // first event: nothing recorded yet, invalidates (cache rebuilt lazily anyway)
        sofaPluginDataHandler.handlerSelector(selector(selectorId, handle));
        assertFalse(referenceCache().asMap().containsKey(referenceKey(selectorId, handle)));

        // re-seed and send the same handle again: must NOT invalidate a second time
        seedReference(selectorId, handle);
        sofaPluginDataHandler.handlerSelector(selector(selectorId, handle));
        assertTrue(referenceCache().asMap().containsKey(referenceKey(selectorId, handle)),
                "an unchanged selector handle must not invalidate live references");
    }

    @Test
    public void testHandlerSelectorInvalidatesWhenUpstreamChanged() throws Exception {
        String selectorId = "1532088187336";
        String oldHandle = "{\"register\":\"zookeeper://127.0.0.1:2181\",\"protocol\":\"bolt\"}";
        sofaPluginDataHandler.handlerSelector(selector(selectorId, oldHandle));
        seedReference(selectorId, oldHandle);

        sofaPluginDataHandler.handlerSelector(selector(selectorId,
                "{\"register\":\"zookeeper://127.0.0.2:2181\",\"protocol\":\"bolt\"}"));
        assertFalse(referenceCache().asMap().containsKey(referenceKey(selectorId, oldHandle)),
                "a changed selector handle must invalidate live references");
    }

    @Test
    public void testHandlerSelectorInvalidatesWhenHandleRemoved() throws Exception {
        String selectorId = "1532088187337";
        String handle = "{\"register\":\"zookeeper://127.0.0.1:2181\",\"protocol\":\"bolt\"}";
        sofaPluginDataHandler.handlerSelector(selector(selectorId, handle));
        seedReference(selectorId, handle);

        // removing the handle must invalidate references built from the old handle
        sofaPluginDataHandler.handlerSelector(selector(selectorId, null));
        assertFalse(referenceCache().asMap().containsKey(referenceKey(selectorId, handle)),
                "a removed selector handle must invalidate live references");
    }

    private void seedReference(final String selectorId, final String handle) throws Exception {
        referenceCache().put(referenceKey(selectorId, handle), new ConsumerConfig<>());
    }

    private String referenceKey(final String selectorId, final String handle) {
        SofaUpstream upstream = GsonUtils.getInstance().fromJson(handle, SofaUpstream.class);
        return ApplicationConfigCache.getInstance().generateUpstreamCacheKey(selectorId, "/sofa/findAll", upstream);
    }

    private SelectorData selector(final String id, final String handle) {
        return SelectorData.builder().id(id).handle(handle).build();
    }

    @SuppressWarnings("unchecked")
    private LoadingCache<String, ConsumerConfig<com.alipay.sofa.rpc.api.GenericService>> referenceCache() throws Exception {
        Field field = ApplicationConfigCache.class.getDeclaredField("cache");
        field.setAccessible(true);
        return (LoadingCache<String, ConsumerConfig<com.alipay.sofa.rpc.api.GenericService>>) field.get(ApplicationConfigCache.getInstance());
    }
}
