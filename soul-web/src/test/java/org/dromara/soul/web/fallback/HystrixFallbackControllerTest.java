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

package org.dromara.soul.web.fallback;

import org.dromara.soul.plugin.api.result.SoulResultEnum;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.core.Is.is;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * The default fallback for hystrix.
 *
 * @author xiaoshen11
 */

@RunWith(MockitoJUnitRunner.class)
public class HystrixFallbackControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private HystrixFallbackController hystrixFallbackController;

    @Before
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(hystrixFallbackController).build();
    }

    @Test
    public void testFallback() throws Exception {
        this.mockMvc.perform(MockMvcRequestBuilders.get("/hystrixFallback/fallback"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(SoulResultEnum.TOO_MANY_REQUESTS.getCode())))
                .andExpect(jsonPath("$.message", is(SoulResultEnum.TOO_MANY_REQUESTS.getMsg())))
                .andReturn();
    }
}
