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

import org.apache.shenyu.admin.disruptor.ShenyuHttpRequestRecordDisruptorPublisher;
import org.apache.shenyu.admin.model.dto.ShenyuHttpRequestRecordDTO;
import org.apache.shenyu.admin.record.entity.RecordTaskInfo;
import org.apache.shenyu.admin.record.entity.ReplayProgress;
import org.apache.shenyu.admin.service.HttpRecordService;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.CommonErrorCode;
import org.apache.shenyu.common.utils.GsonUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.zip.CRC32;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.BDDMockito.given;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(MockitoExtension.class)
class HttpRecordControllerTest {

    private MockMvc mockMvc;

    @Mock
    private ShenyuHttpRequestRecordDisruptorPublisher publisher;

    @Mock
    private HttpRecordService httpRecordService;

    @InjectMocks
    private HttpRecordController httpRecordController;

    private List<ShenyuHttpRequestRecordDTO.Record> recordList;

    @BeforeEach
    public void setUp() {
        this.mockMvc = MockMvcBuilders.standaloneSetup(httpRecordController).build();
        ShenyuHttpRequestRecordDTO.Record record1 = new ShenyuHttpRequestRecordDTO.Record();
        record1.setMethod("GET");
        record1.setStatus(200);
        record1.setQueryParams("test1=test1&test2=test2");
        record1.setResponseBody("testResponseBody");
        record1.setRequestUri("/testRequestUri");
        ShenyuHttpRequestRecordDTO.Record record2 = new ShenyuHttpRequestRecordDTO.Record();
        record2.setMethod("POST");
        record2.setStatus(200);
        record2.setRequestBody("testRequestBody2");
        record2.setRequestUri("/testRequestUri2");
        record2.setResponseBody("testResponseBody2");
        this.recordList = Arrays.asList(record1, record2);
    }

    @Test
    public void testUploadHappyPath() throws Exception {
        ArgumentCaptor<ShenyuHttpRequestRecordDTO> captor = ArgumentCaptor.forClass(ShenyuHttpRequestRecordDTO.class);
        byte[] content = GsonUtils.getInstance().toJson(recordList).getBytes(StandardCharsets.UTF_8);
        CRC32 crc32 = new CRC32();
        crc32.update(content);
        long crc32Value = crc32.getValue();
        mockMvc.perform(MockMvcRequestBuilders.post("/record/upload-record")
                .header(Constants.X_RECORD_CRC32, crc32Value)
                .contentType(MediaType.APPLICATION_JSON)
                .content(content)
        ).andExpect(status().isOk()).andReturn();
        Mockito.verify(publisher, Mockito.times(1)).publish(captor.capture());
        ShenyuHttpRequestRecordDTO dto = captor.getValue();
        assertEquals(2, dto.getRecords().size());
        assertEquals("GET", dto.getRecords().get(0).getMethod());
        assertEquals("/testRequestUri", dto.getRecords().get(0).getRequestUri());
        assertEquals("POST", dto.getRecords().get(1).getMethod());
    }

    @Test
    public void testUploadNoCrc32() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.post("/record/upload-record")
                .contentType(MediaType.APPLICATION_JSON)
                .content(GsonUtils.getInstance().toJson(recordList).getBytes(StandardCharsets.UTF_8))
        ).andExpect(status().is(400)).andReturn();
        Mockito.verify(publisher, Mockito.times(0)).publish(Mockito.any());
    }

    @Test
    public void testUploadCrc32ValidationFailed() throws Exception {
        byte[] content = GsonUtils.getInstance().toJson(recordList).getBytes(StandardCharsets.UTF_8);
        long wrongCrc32 = 99999L;

        mockMvc.perform(MockMvcRequestBuilders.post("/record/upload-record")
                .header(Constants.X_RECORD_CRC32, wrongCrc32)
                .contentType(MediaType.APPLICATION_JSON)
                .content(content)
        ).andExpect(status().isOk());

        Mockito.verify(publisher, Mockito.never()).publish(Mockito.any());
    }

    @Test
    public void testUploadWithEmptyRecords() throws Exception {
        byte[] content = "[]".getBytes(StandardCharsets.UTF_8);
        CRC32 crc32 = new CRC32();
        crc32.update(content);
        long crc32Value = crc32.getValue();

        mockMvc.perform(MockMvcRequestBuilders.post("/record/upload-record")
                .header(Constants.X_RECORD_CRC32, crc32Value)
                .contentType(MediaType.APPLICATION_JSON)
                .content(content)
        ).andExpect(status().isOk());

        Mockito.verify(publisher, Mockito.never()).publish(Mockito.any());
    }

    @Test
    public void testUploadWithInvalidJsonBody() throws Exception {
        byte[] content = "not json".getBytes(StandardCharsets.UTF_8);
        CRC32 crc32 = new CRC32();
        crc32.update(content);
        long crc32Value = crc32.getValue();

        mockMvc.perform(MockMvcRequestBuilders.post("/record/upload-record")
                .header(Constants.X_RECORD_CRC32, crc32Value)
                .contentType(MediaType.APPLICATION_JSON)
                .content(content)
        ).andExpect(status().isOk());

        Mockito.verify(publisher, Mockito.never()).publish(Mockito.any());
    }

    @Test
    public void testListRecordTasksWithResults() throws Exception {
        List<RecordTaskInfo> tasks = Arrays.asList(
                new RecordTaskInfo("task1", 10, 1000L),
                new RecordTaskInfo("task2", 20, 2000L)
        );
        given(httpRecordService.listRecordTasks()).willReturn(tasks);

        mockMvc.perform(MockMvcRequestBuilders.get("/record"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(CommonErrorCode.SUCCESSFUL))
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(2))
                .andExpect(jsonPath("$.data[0].taskId").value("task1"))
                .andExpect(jsonPath("$.data[0].recordCount").value(10))
                .andExpect(jsonPath("$.data[1].taskId").value("task2"));
    }

    @Test
    public void testListRecordTasksEmpty() throws Exception {
        given(httpRecordService.listRecordTasks()).willReturn(new ArrayList<>());

        mockMvc.perform(MockMvcRequestBuilders.get("/record"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(CommonErrorCode.SUCCESSFUL))
                .andExpect(jsonPath("$.data").isArray())
                .andExpect(jsonPath("$.data.length()").value(0));
    }

    @Test
    public void testStartReplaySuccess() throws Exception {
        given(httpRecordService.startReplay("task1", "http://127.0.0.1:9195")).willReturn("replay123");

        mockMvc.perform(MockMvcRequestBuilders.post("/record/replay")
                .param("taskId", "task1")
                .param("targetHost", "http://127.0.0.1:9195")
        ).andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(CommonErrorCode.SUCCESSFUL))
                .andExpect(jsonPath("$.data").value("replay123"));
    }

    @Test
    public void testStartReplayWithBlankTaskId() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.post("/record/replay")
                .param("taskId", "")
                .param("targetHost", "http://127.0.0.1:9195")
        ).andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(CommonErrorCode.ERROR))
                .andExpect(jsonPath("$.message").value("taskId and targetHost must not be blank"));
    }

    @Test
    public void testStartReplayWithBlankTargetHost() throws Exception {
        mockMvc.perform(MockMvcRequestBuilders.post("/record/replay")
                .param("taskId", "task1")
                .param("targetHost", "")
        ).andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(CommonErrorCode.ERROR))
                .andExpect(jsonPath("$.message").value("taskId and targetHost must not be blank"));
    }

    @Test
    public void testStartReplayThrowsIllegalArgumentException() throws Exception {
        given(httpRecordService.startReplay("badTask", "http://127.0.0.1:9195"))
                .willThrow(new IllegalArgumentException("Task not found"));

        mockMvc.perform(MockMvcRequestBuilders.post("/record/replay")
                .param("taskId", "badTask")
                .param("targetHost", "http://127.0.0.1:9195")
        ).andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(CommonErrorCode.ERROR))
                .andExpect(jsonPath("$.message").value("Task not found"));
    }

    @Test
    public void testStartReplayThrowsGenericException() throws Exception {
        given(httpRecordService.startReplay("task1", "http://127.0.0.1:9195"))
                .willThrow(new RuntimeException("Internal error"));

        mockMvc.perform(MockMvcRequestBuilders.post("/record/replay")
                .param("taskId", "task1")
                .param("targetHost", "http://127.0.0.1:9195")
        ).andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(CommonErrorCode.ERROR))
                .andExpect(jsonPath("$.message").value("Failed to start replay: Internal error"));
    }

    @Test
    public void testGetReplayProgressSuccess() throws Exception {
        ReplayProgress progress = new ReplayProgress("replay123", "task1", 10);
        progress.incrementSucceeded();
        progress.incrementFailed();
        given(httpRecordService.getReplayProgress("replay123")).willReturn(progress);

        mockMvc.perform(MockMvcRequestBuilders.get("/record/progress/{replayId}", "replay123"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(CommonErrorCode.SUCCESSFUL))
                .andExpect(jsonPath("$.data.replayId").value("replay123"))
                .andExpect(jsonPath("$.data.taskId").value("task1"))
                .andExpect(jsonPath("$.data.total").value(10))
                .andExpect(jsonPath("$.data.succeeded").value(1))
                .andExpect(jsonPath("$.data.failed").value(1));
    }

    @Test
    public void testGetReplayProgressNotFound() throws Exception {
        given(httpRecordService.getReplayProgress("unknown")).willReturn(null);

        mockMvc.perform(MockMvcRequestBuilders.get("/record/progress/{replayId}", "unknown"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.code").value(CommonErrorCode.ERROR))
                .andExpect(jsonPath("$.message").value("Replay not found for replayId: unknown"));
    }
}
