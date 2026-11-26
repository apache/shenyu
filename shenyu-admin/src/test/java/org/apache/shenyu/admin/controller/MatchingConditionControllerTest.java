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

import org.apache.shenyu.admin.model.vo.MatchingConditionCatalogVO;
import org.apache.shenyu.admin.service.MatchingConditionMetadataService;
import org.apache.shenyu.admin.service.impl.MatchingConditionMetadataServiceImpl;
import org.apache.shenyu.common.condition.MatchingConditionCatalog;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.core.Is.is;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Tests for {@link MatchingConditionController}.
 */
final class MatchingConditionControllerTest {

    private MockMvc mockMvc;

    private MatchingConditionCatalogVO catalog;

    @BeforeEach
    void setUp() {
        catalog = new MatchingConditionMetadataServiceImpl().catalog();
        MatchingConditionMetadataService service = () -> catalog;
        mockMvc = MockMvcBuilders.standaloneSetup(new MatchingConditionController(service)).build();
    }

    @Test
    void shouldReturnCurrentCatalog() throws Exception {
        mockMvc.perform(get("/matching-conditions"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(CommonErrorCode.SUCCESSFUL)))
                .andExpect(jsonPath("$.data.version", is(MatchingConditionCatalog.VERSION)))
                .andExpect(jsonPath("$.data.sources", hasSize(MatchingConditionCatalog.sources().size())))
                .andExpect(jsonPath("$.data.operators", hasSize(MatchingConditionCatalog.operators().size())))
                .andExpect(jsonPath("$.data.sources[0].id", is("path")))
                .andExpect(jsonPath("$.data.operators[0].id", is("match")));
    }

    @Test
    void shouldAcceptExplicitCurrentVersion() throws Exception {
        mockMvc.perform(get("/matching-conditions").param("version", MatchingConditionCatalog.VERSION))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(CommonErrorCode.SUCCESSFUL)))
                .andExpect(jsonPath("$.data.version", is(MatchingConditionCatalog.VERSION)));
    }

    @Test
    void shouldTreatBlankVersionAsAbsent() throws Exception {
        mockMvc.perform(get("/matching-conditions").param("version", ""))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(CommonErrorCode.SUCCESSFUL)))
                .andExpect(jsonPath("$.data.version", is(MatchingConditionCatalog.VERSION)));
    }

    @Test
    void shouldReturnBoundedErrorForUnsupportedVersion() throws Exception {
        mockMvc.perform(get("/matching-conditions").param("version", "unsupported-version"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code", is(CommonErrorCode.NOT_FOUND_EXCEPTION)))
                .andExpect(jsonPath("$.message", is("Unsupported matching condition catalog version")));
    }
}
