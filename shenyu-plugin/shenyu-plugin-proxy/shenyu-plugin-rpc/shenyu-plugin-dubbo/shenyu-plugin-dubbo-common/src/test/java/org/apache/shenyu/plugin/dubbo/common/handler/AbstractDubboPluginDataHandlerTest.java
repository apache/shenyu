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

package org.apache.shenyu.plugin.dubbo.common.handler;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.plugin.DubboRegisterConfig;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * AbstractDubboPluginDataHandler test.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class AbstractDubboPluginDataHandlerTest {

    private AbstractDubboPluginDataHandler handler;

    @BeforeEach
    public void setUp() {
        handler = new AbstractDubboPluginDataHandler() {
            @Override
            protected void initConfigCache(final DubboRegisterConfig dubboRegisterConfig) {
            }

            @Override
            protected void invalidateReferenceBySelector(final SelectorData selectorData) {
            }

            @Override
            protected void invalidateReferenceByRule(final RuleData ruleData) {
            }
        };
    }

    @Test
    public void handlerPluginTest() {
        PluginData pluginData = new PluginData();
        pluginData.setEnabled(Boolean.TRUE);
        handler.handlerPlugin(pluginData);

        pluginData.setConfig("{}");
        handler.handlerPlugin(pluginData);
    }

    @Test
    public void selectorTest() {
        SelectorData selectorData = new SelectorData();
        handler.handlerSelector(selectorData);
        selectorData.setId("1");
        selectorData.setHandle("[{\"appName\": \"name\", \"upstreamUrl\": \"http://192.168.55.113/dubbo\", \"gray\":true}]");
        handler.handlerSelector(selectorData);
        assertEquals(AbstractDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get().obtainHandle("1").size(), 1);

        selectorData.setHandle("[{\"appName\": \"name\", \"upstreamUrl\": \"http://192.168.55.113/dubbo\", \"gray\":false}]");
        handler.handlerSelector(selectorData);
        assertNull(AbstractDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get().obtainHandle("1"));
        // when gray update false
        selectorData.setHandle("[{\"appName\": \"name\", \"upstreamUrl\": \"http://192.168.55.113/dubbo\", \"gray\":true},{\"appName\": \"name\", \"upstreamUrl\": \"http://192.168.55.114/dubbo\", \"gray\":true}]");
        handler.handlerSelector(selectorData);
        assertEquals(AbstractDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get().obtainHandle("1").size(), 2);

        selectorData.setHandle("[{\"appName\": \"name\", \"upstreamUrl\": \"http://192.168.55.113/dubbo\", \"gray\":true},{\"appName\": \"name\", \"upstreamUrl\": \"http://192.168.55.114/dubbo\", \"gray\":false}]");
        handler.handlerSelector(selectorData);
        assertEquals(AbstractDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get().obtainHandle("1").size(), 1);

        handler.removeSelector(selectorData);
        assertNull(AbstractDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get().obtainHandle("1"));
    }

    @Test
    public void emptyHandleClearsGrayRoutingCaches() {
        final String selectorId = "gray-empty-selector";
        UpstreamCacheManager.getInstance().removeByKey(selectorId);
        SelectorData selectorData = new SelectorData();
        selectorData.setId(selectorId);
        selectorData.setContinued(true);
        selectorData.setHandle("[{\"protocol\":\"dubbo://\",\"upstreamUrl\":\"127.0.0.1:20880\",\"gray\":true,"
                + "\"weight\":1,\"status\":true,\"healthCheckEnabled\":false}]");
        handler.handlerSelector(selectorData);
        assertEquals(1, AbstractDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get().obtainHandle(selectorId).size());
        assertEquals(1, UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorId).size());

        // a full update to an empty handle is a valid replacement and must clear gray routing
        selectorData.setHandle("[]");
        handler.handlerSelector(selectorData);
        assertNull(AbstractDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get().obtainHandle(selectorId));
        assertNull(UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorId));
        UpstreamCacheManager.getInstance().removeByKey(selectorId);
    }

    @Test
    public void replacingGrayTargetRemovesOmittedUpstream() {
        final String selectorId = "gray-replace-selector";
        UpstreamCacheManager.getInstance().removeByKey(selectorId);
        SelectorData selectorData = new SelectorData();
        selectorData.setId(selectorId);
        selectorData.setContinued(true);
        selectorData.setHandle("[{\"protocol\":\"dubbo://\",\"upstreamUrl\":\"127.0.0.1:20880\",\"gray\":true,"
                + "\"weight\":1,\"status\":true,\"healthCheckEnabled\":false}]");
        handler.handlerSelector(selectorData);
        assertEquals(1, UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorId).size());

        // replace gray target A (20880) with B (20881): A must no longer remain a routing candidate
        selectorData.setHandle("[{\"protocol\":\"dubbo://\",\"upstreamUrl\":\"127.0.0.1:20881\",\"gray\":true,"
                + "\"weight\":1,\"status\":true,\"healthCheckEnabled\":false}]");
        handler.handlerSelector(selectorData);
        List<Upstream> candidates = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorId);
        assertEquals(1, candidates.size());
        assertEquals("127.0.0.1:20881", candidates.get(0).getUrl());
        assertEquals(1, AbstractDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get().obtainHandle(selectorId).size());
        UpstreamCacheManager.getInstance().removeByKey(selectorId);
    }

    @Test
    public void ruleTest() {
        RuleData ruleData = new RuleData();
        handler.handlerRule(ruleData);
        handler.removeRule(ruleData);
    }

    @Test
    public void pluginNamed() {
        assertThat(handler.pluginNamed(), is(PluginEnum.DUBBO.getName()));
    }
}
