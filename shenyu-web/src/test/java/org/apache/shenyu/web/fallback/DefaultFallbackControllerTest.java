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

package org.apache.shenyu.web.fallback;

import org.apache.shenyu.plugin.api.result.DefaultShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResult;
import org.apache.shenyu.plugin.api.result.ShenyuResultEnum;
import org.apache.shenyu.plugin.api.utils.SpringBeanUtils;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.core.Is.is;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test case for DefaultFallbackController.
 */
@RunWith(MockitoJUnitRunner.class)
public final class DefaultFallbackControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private DefaultFallbackController defaultFallbackController;

    @Before
    public void setUp() {
        ConfigurableApplicationContext context = mock(ConfigurableApplicationContext.class);
        SpringBeanUtils.getInstance().setApplicationContext(context);
        when(context.getBean(ShenyuResult.class)).thenReturn(new DefaultShenyuResult() { });

        this.mockMvc = MockMvcBuilders.standaloneSetup(defaultFallbackController).build();
    }

    @Test
    public void testFallback() throws Exception {
        this.mockMvc.perform(MockMvcRequestBuilders.get("/fallback/hystrix"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(ShenyuResultEnum.HYSTRIX_PLUGIN_FALLBACK.getCode())))
                .andExpect(jsonPath("$.message", is(ShenyuResultEnum.HYSTRIX_PLUGIN_FALLBACK.getMsg())))
                .andReturn();
    }

    @Test
    public void testResilience4jFallback() throws Exception {
        this.mockMvc.perform(MockMvcRequestBuilders.get("/fallback/resilience4j"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(ShenyuResultEnum.RESILIENCE4J_PLUGIN_FALLBACK.getCode())))
                .andExpect(jsonPath("$.message", is(ShenyuResultEnum.RESILIENCE4J_PLUGIN_FALLBACK.getMsg())))
                .andReturn();
    }
}
