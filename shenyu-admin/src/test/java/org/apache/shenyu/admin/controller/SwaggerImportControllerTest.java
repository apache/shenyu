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

import org.apache.shenyu.admin.model.dto.SwaggerImportRequest;
import org.apache.shenyu.admin.service.SwaggerImportService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.core.Is.is;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test for {@link SwaggerImportController}.
 */
@ExtendWith(MockitoExtension.class)
public class SwaggerImportControllerTest {

    private static final String IMPORT_REQUEST = "{"
            + "\"swaggerUrl\":\"https://8.8.8.8/swagger.json\","
            + "\"projectName\":\"test\""
            + "}";

    private MockMvc mockMvc;

    @InjectMocks
    private SwaggerImportController swaggerImportController;

    @Mock
    private SwaggerImportService swaggerImportService;

    @BeforeEach
    public void setUp() {
        mockMvc = MockMvcBuilders.standaloneSetup(swaggerImportController).build();
    }

    @Test
    public void importSwaggerShouldReturnBadRequestCodeWhenBodyExceedsLimit() throws Exception {
        when(swaggerImportService.importSwagger(any(SwaggerImportRequest.class)))
                .thenThrow(new IllegalArgumentException(
                        "Swagger document response body exceeds maximum size of 10 bytes"));

        mockMvc.perform(MockMvcRequestBuilders.post("/swagger/import")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(IMPORT_REQUEST))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(HttpStatus.BAD_REQUEST.value())));
    }

    @Test
    public void importMcpConfigShouldReturnBadRequestCodeWhenBodyExceedsLimit() throws Exception {
        when(swaggerImportService.importMcpConfig(any(SwaggerImportRequest.class)))
                .thenThrow(new IllegalArgumentException(
                        "Swagger document response body exceeds maximum size of 10 bytes"));

        mockMvc.perform(MockMvcRequestBuilders.post("/swagger/import/mcp")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content(IMPORT_REQUEST))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(HttpStatus.BAD_REQUEST.value())));
    }
}
