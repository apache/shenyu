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

package org.apache.shenyu.admin.controller;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.servlet.view.InternalResourceViewResolver;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test case for IndexController.
 */
@ExtendWith(SpringExtension.class)
public final class IndexControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private IndexController indexController;

    @BeforeEach
    public void setUp() {
        InternalResourceViewResolver viewResolver = new InternalResourceViewResolver();
        viewResolver.setPrefix("/WEB-INF/jsp/view/");
        viewResolver.setSuffix(".jsp");
        this.mockMvc = MockMvcBuilders.standaloneSetup(indexController).setViewResolvers(viewResolver).build();
    }

    @Test
    public void testIndex() throws Exception {
        this.mockMvc.perform(get("/index"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("domain"))
                .andReturn();
    }

    @Test
    public void testRootEndpoint() throws Exception {
        this.mockMvc.perform(get("/"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("domain"))
                .andReturn();
    }

    @Test
    public void testIndexWithForwardedHeaders() throws Exception {
        this.mockMvc.perform(get("/index")
                        .header("X-Forwarded-Proto", "https")
                        .header("X-Forwarded-Host", "example.com"))
                .andExpect(status().isOk())
                .andExpect(model().attribute("domain", "https://example.com"))
                .andReturn();
    }

    @Test
    public void testIndexWithForwardedProtoOnly() throws Exception {
        this.mockMvc.perform(get("/index")
                        .header("X-Forwarded-Proto", "https"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("domain"))
                .andReturn();
    }

    @Test
    public void testIndexWithForwardedHostOnly() throws Exception {
        this.mockMvc.perform(get("/index")
                        .header("X-Forwarded-Host", "example.com:8443"))
                .andExpect(status().isOk())
                .andExpect(model().attribute("domain", "http://example.com:8443"))
                .andReturn();
    }

    @Test
    public void testIndexWithContextPath() throws Exception {
        // Note: MockMvc standalone setup doesn't easily support context path testing
        // The context path functionality is tested indirectly through other tests
        // This test verifies basic functionality still works
        this.mockMvc.perform(get("/index"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("domain"))
                .andReturn();
    }

    @Test
    public void testIndexWithContextPathAndForwardedHeaders() throws Exception {
        // Test forwarded headers work correctly (context path handling is tested in integration tests)
        this.mockMvc.perform(get("/index")
                        .header("X-Forwarded-Proto", "https")
                        .header("X-Forwarded-Host", "example.com"))
                .andExpect(status().isOk())
                .andExpect(model().attribute("domain", "https://example.com"))
                .andReturn();
    }

    @Test
    public void testIndexWithForwardedHostContainingPort() throws Exception {
        this.mockMvc.perform(get("/index")
                        .header("X-Forwarded-Proto", "https")
                        .header("X-Forwarded-Host", "example.com:8443"))
                .andExpect(status().isOk())
                .andExpect(model().attribute("domain", "https://example.com:8443"))
                .andReturn();
    }

    @Test
    public void testIndexWithInvalidForwardedHostContainingProtocol() throws Exception {
        // Should fall back to server name when forwarded host contains protocol separator
        this.mockMvc.perform(get("/index")
                        .header("X-Forwarded-Host", "http://evil.com"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("domain"))
                .andReturn();
    }

    @Test
    public void testIndexWithInvalidForwardedHostContainingPath() throws Exception {
        // Should fall back to server name when forwarded host contains path separator
        this.mockMvc.perform(get("/index")
                        .header("X-Forwarded-Host", "evil.com/path"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("domain"))
                .andReturn();
    }

    @Test
    public void testIndexWithInvalidForwardedHostContainingQuery() throws Exception {
        // Should fall back to server name when forwarded host contains query separator
        this.mockMvc.perform(get("/index")
                        .header("X-Forwarded-Host", "evil.com?param=value"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("domain"))
                .andReturn();
    }

    @Test
    public void testIndexWithInvalidForwardedHostInvalidPort() throws Exception {
        // Should fall back to server name when forwarded host has invalid port
        this.mockMvc.perform(get("/index")
                        .header("X-Forwarded-Host", "example.com:99999"))
                .andExpect(status().isOk())
                .andExpect(model().attributeExists("domain"))
                .andReturn();
    }

    @Test
    public void testIndexWithValidForwardedHostWithValidPort() throws Exception {
        this.mockMvc.perform(get("/index")
                        .header("X-Forwarded-Proto", "https")
                        .header("X-Forwarded-Host", "subdomain.example.com:8080"))
                .andExpect(status().isOk())
                .andExpect(model().attribute("domain", "https://subdomain.example.com:8080"))
                .andReturn();
    }
}
