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
 * <p>The digest view is always available and publishes non-secret identity fields plus
 * SHA-256 digests of the sensitive payload fields; these tests seed the caches with clearly
 * marked fake secrets and assert the raw material never leaves the process while the
 * identity fields the digest view exists for do.</p>
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
        MetaDataCache.getInstance().cache(MetaData.builder().id("m1").path("/rpc/demo").rpcType("dubbo").methodName("demoMethod")
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
    public void pluginDigest() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/pluginDigest");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void selectorDigest() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/selectorDigest");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void ruleDigest() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/ruleDigest");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void getSelectorMatchDigest() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/selectorMatchDigest");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void getRuleMatchDigest() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/ruleMatchDigest");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void getMetadataDigest() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/metadataDigest");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void getMetaDataCacheDigest() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/metadataCacheDigest");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        assertDigestOnly(response.getContentAsString());
    }

    @Test
    public void digestKeepsNonSecretIdentityFields() throws Exception {
        final String selector = performGet("/actuator/selectorDigest").getContentAsString();
        assertTrue(selector.contains("\"name\":\"sel\""), "selector name is identity, not a credential: " + selector);
        final String rule = performGet("/actuator/ruleDigest").getContentAsString();
        assertTrue(rule.contains("\"name\":\"rule\""), "rule name is identity, not a credential: " + rule);
        final String metadata = performGet("/actuator/metadataDigest").getContentAsString();
        assertTrue(metadata.contains("\"path\":\"/rpc/demo\""), "metadata path is identity, not a credential: " + metadata);
        assertTrue(metadata.contains("\"rpcType\":\"dubbo\""), "rpcType is identity, not a credential: " + metadata);
    }

    // Fluent Flux bodies stream through the async dispatch; collect the final content
    private MockHttpServletResponse performGet(final String path) throws Exception {
        final MvcResult result = this.mockMvc.perform(MockMvcRequestBuilders.get(path)).andReturn();
        return this.mockMvc.perform(MockMvcRequestBuilders.asyncDispatch(result)).andReturn().getResponse();
    }

    /**
     * The body must carry the bounded digest structure (a 64-hex digest) and none of
     * the raw sensitive payloads seeded in the cache.
     */
    private void assertDigestOnly(final String body) {
        assertFalse(body.contains("my-super-secret-key-12345"), "raw config must not be published");
        assertFalse(body.contains("plain-secret"), "raw metadata extensions must not be published");
        assertTrue(body.matches("(?s).*[0-9a-f]{64}.*"),
                "expected SHA-256 digest representation, got: " + body);
    }
}
