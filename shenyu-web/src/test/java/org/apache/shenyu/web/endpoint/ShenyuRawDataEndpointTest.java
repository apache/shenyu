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

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import org.apache.shenyu.plugin.base.cache.MetaDataCache;
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

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;

/**
 * Test cases for {@link ShenyuRawDataEndpoint}.
 *
 * <p>When {@code shenyu.endpoint.raw-data.enabled=true} the original paths serve the raw
 * caches exactly as before the digest view was introduced. This pins the contract the e2e
 * client relies on ({@code shenyu-e2e-client} deserializes these payloads into
 * {@code PluginData}/{@code SelectorCacheData}/{@code RuleCacheData}/{@code MetaData}),
 * so a digest-shaped body must fail these tests loudly instead of degrading assertions
 * into null-vs-null comparisons.</p>
 */
public final class ShenyuRawDataEndpointTest {

    private static final String FAKE_SECRET = "apiKey=my-super-secret-key-12345";

    private MockMvc mockMvc;

    private final ObjectMapper mapper = new ObjectMapper();

    @BeforeEach
    public void setUp() {
        ShenyuRawDataEndpoint rawDataEndpoint = new ShenyuRawDataEndpoint();
        this.mockMvc = MockMvcBuilders.standaloneSetup(rawDataEndpoint).build();
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        BaseDataCache.getInstance().cachePluginData(PluginData.builder().id("p1").name("divide").config(FAKE_SECRET).role("1").enabled(true).build());
        MetaDataCache.getInstance().cache(MetaData.builder().id("m1").path("/rpc/demo").rpcType("dubbo")
                .rpcExt("{\"timeout\":3000}").build());
    }

    @AfterEach
    public void cleanUp() {
        BaseDataCache.getInstance().cleanPluginData();
        BaseDataCache.getInstance().cleanSelectorData();
        BaseDataCache.getInstance().cleanRuleData();
        MetaDataCache.getInstance().clean();
    }

    @Test
    public void pluginDataServesTheRawCacheDump() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/pluginData");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        final String body = response.getContentAsString();
        assertTrue(body.contains("divide"), "the raw payload must carry the plugin name: " + body);
        assertTrue(body.contains(FAKE_SECRET), "the raw payload must carry the full config, that is what opting in means");
        // the endpoint returns Flux<Map<...>>: a single-element array, exactly what the
        // e2e client's List<T> deserialization expects
        final List<Map<String, PluginData>> deserialized = mapper.readValue(body, mapper.getTypeFactory()
                .constructParametricType(List.class, mapper.getTypeFactory().constructMapType(Map.class, String.class, PluginData.class)));
        assertFalse(deserialized.isEmpty());
        PluginData plugin = deserialized.get(0).get("divide");
        assertNotNull(plugin, "the payload must deserialize into PluginData like the e2e client does");
        assertEquals("p1", plugin.getId());
        assertEquals(Boolean.TRUE, plugin.getEnabled());
    }

    @Test
    public void metadataServesTheRawCacheDump() throws Exception {
        final MockHttpServletResponse response = performGet("/actuator/metadata");
        assertEquals(HttpStatus.OK.value(), response.getStatus());
        final String body = response.getContentAsString();
        assertTrue(body.contains("timeout") && body.contains("rpcExt"), "the raw payload must carry the full rpcExt");
        final List<Map<String, MetaData>> deserialized = mapper.readValue(body, mapper.getTypeFactory()
                .constructParametricType(List.class, mapper.getTypeFactory().constructMapType(Map.class, String.class, MetaData.class)));
        assertFalse(deserialized.isEmpty());
        MetaData data = deserialized.get(0).get("m1");
        assertNotNull(data, "the payload must deserialize into MetaData like the e2e client does");
        assertEquals("/rpc/demo", data.getPath());
        assertEquals("dubbo", data.getRpcType());
    }

    private MockHttpServletResponse performGet(final String path) throws Exception {
        final MvcResult result = this.mockMvc.perform(MockMvcRequestBuilders.get(path)).andReturn();
        return this.mockMvc.perform(MockMvcRequestBuilders.asyncDispatch(result)).andReturn().getResponse();
    }
}
