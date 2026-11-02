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

package org.apache.shenyu.web.endpoint;

import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MatchDataCache;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Test cases for {@link ShenyuControllerEndpoint}.
 *
 * <p>The endpoints are opt-in and publish only bounded digest representations; these tests
 * seed the caches with clearly marked fake secrets and assert the raw material never leaves
 * the process.</p>
 */
public final class ShenyuControllerEndpointTest {

    private static final String FAKE_SECRET = "apiKey=my-super-secret-key-12345";

    private MockMvc mockMvc;

    @BeforeEach
    public void setUp() {
        ShenyuControllerEndpoint shenyuControllerEndpoint = new ShenyuControllerEndpoint(mock(ShenyuWebHandler.class));
        this.mockMvc = MockMvcBuilders.standaloneSetup(shenyuControllerEndpoint).build();
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        BaseDataCache baseDataCache = BaseDataCache.getInstance();
        baseDataCache.cachePluginData(PluginData.builder().id("p1").name("divide").config(FAKE_SECRET).role("1").enabled(true).build());
        baseDataCache.cacheSelectData(SelectorData.builder().id("s1").name("sel").pluginName("divide").handle(FAKE_SECRET).enabled(true).build());
        baseDataCache.cacheRuleData(RuleData.builder().id("r1").name("rule").pluginName("divide").selectorId("s1").handle(FAKE_SECRET).build());
        MetaDataCache.getInstance().cache(MetaData.builder().id("m1").path("/rpc/demo").rpcType("dubbo")
                .rpcExt("{\"timeout\":3000,\"password\":\"plain-secret\"}").build());
        MatchDataCache.getInstance().cacheSelectorData("/api/demo", SelectorData.builder().id("sm1").name("selMatch")
                .pluginName("divide").handle(FAKE_SECRET).enabled(true).build(), 16, 16);
        MatchDataCache.getInstance().cacheRuleData("/api/demo", RuleData.builder().id("rm1").name("ruleMatch")
                .pluginName("divide").selectorId("sm1").handle(FAKE_SECRET).build(), 16, 16);
    }

    @AfterEach
    public void cleanUp() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
        MetaDataCache.getInstance().clean();
        MatchDataCache.getInstance().cleanSelectorData();
        MatchDataCache.getInstance().cleanRuleDataData();
    }

    @Test
    public void plugins() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/plugins");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
    }

    @Test
    public void pluginDatas() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/pluginData");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void selectorData() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/selectorData");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void ruleData() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/ruleData");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void getSelectorMatchCache() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/selectorMatchCache");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void getRuleMatchCache() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/ruleMatchCache");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void getMetadata() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/metadata");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void getMetaDataCache() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/metadataCache");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    // Fluent Flux bodies stream through the async dispatch; collect the final content
    private MockHttpServletResponse performGet(final String path) throws Exception {
        final MvcResult result = this.mockMvc.perform(MockMvcRequestBuilders.get(path)).andReturn();
        return this.mockMvc.perform(MockMvcRequestBuilders.asyncDispatch(result)).andReturn().getResponse();
    }

    /**
     * The body must carry the bounded digest structure (a 64-hex config digest) and none of
     * the raw sensitive payloads seeded in the cache.
     */
    private void assertDigestOnly(final String body) {
        assertFalse(body.contains("my-super-secret-key-12345"), "raw config must not be published");
        assertFalse(body.contains("plain-secret"), "raw metadata extensions must not be published");
        assertTrue(body.contains("Digest") || body.matches(".*[0-9a-f]{64}.*"),
                "expected SHA-256 digest representation, got: " + body);
    }
}
