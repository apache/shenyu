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

import java.util.ArrayList;
import java.util.List;
import org.apache.shenyu.admin.model.entity.OperationRecordLog;
import org.apache.shenyu.admin.service.OperationRecordLogService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

/**
 * Test cases for OperationRecordLogControllerTest.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class OperationRecordLogControllerTest {

    private MockMvc mockMvc;

    @InjectMocks
    private OperationRecordLogController operationRecordLogController;

    @Mock
    private OperationRecordLogService operationRecordLogService;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(operationRecordLogController)
                .build();
    }

    @Test
    public void testList() throws Exception {
        List<OperationRecordLog> operationRecordLogs = new ArrayList<>();
        given(this.operationRecordLogService.list()).willReturn(operationRecordLogs);
        this.mockMvc.perform(MockMvcRequestBuilders.get("/operation-record/log/list"))
                .andExpect(status().isOk())
                .andReturn();
    }

    @Test
    public void testClean() throws Exception {
        given(this.operationRecordLogService.cleanHistory(any())).willReturn(true);
        this.mockMvc.perform(MockMvcRequestBuilders.delete("/operation-record/log/clean/" + "2020-10-22 10:10:10")
                .contentType(MediaType.APPLICATION_JSON)
                .accept(MediaType.APPLICATION_JSON))
                .andExpect(status().isOk())
                .andReturn();
    }
}
