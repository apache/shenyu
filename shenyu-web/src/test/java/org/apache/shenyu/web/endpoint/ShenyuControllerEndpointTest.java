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

import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.apache.shenyu.web.handler.ShenyuWebHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.http.HttpStatus;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.Mockito.mock;

/**
 * Test cases for {@link ShenyuControllerEndpoint}.
 */
public final class ShenyuControllerEndpointTest {

    private MockMvc mockMvc;

    @BeforeEach
    public void setUp() {
        ShenyuControllerEndpoint shenyuControllerEndpoint = new ShenyuControllerEndpoint(mock(ShenyuWebHandler.class));
        this.mockMvc = MockMvcBuilders.standaloneSetup(shenyuControllerEndpoint).build();
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
    }

    @Test
    public void plugins() throws Exception {
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/actuator/plugins"))
            .andReturn().getResponse();
        assertEquals(response.getStatus(), HttpStatus.OK.value());
    }

    @Test
    public void pluginDatas() throws Exception {
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/actuator/pluginData"))
            .andReturn().getResponse();
        assertEquals(response.getStatus(), HttpStatus.OK.value());
    }

    @Test
    public void selectorData() throws Exception {
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/actuator/selectorData"))
            .andReturn().getResponse();
        assertEquals(response.getStatus(), HttpStatus.OK.value());
    }

    @Test
    public void ruleData() throws Exception {
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/actuator/ruleData"))
            .andReturn().getResponse();
        assertEquals(response.getStatus(), HttpStatus.OK.value());
    }

    @Test
    public void getSelectorMatchCache() throws Exception {
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/actuator/selectorMatchCache"))
            .andReturn().getResponse();
        assertEquals(response.getStatus(), HttpStatus.OK.value());
    }

    @Test
    public void getRuleMatchCache() throws Exception {
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/actuator/ruleMatchCache"))
            .andReturn().getResponse();
        assertEquals(response.getStatus(), HttpStatus.OK.value());
    }

    @Test
    public void getMetadata() throws Exception {
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/actuator/metadata"))
            .andReturn().getResponse();
        assertEquals(response.getStatus(), HttpStatus.OK.value());
    }

    @Test
    public void getMetaDataCache() throws Exception {
        final MockHttpServletResponse response = this.mockMvc.perform(MockMvcRequestBuilders.get("/actuator/metadataCache"))
            .andReturn().getResponse();
        assertEquals(response.getStatus(), HttpStatus.OK.value());
    }
}
