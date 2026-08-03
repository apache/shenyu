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

package org.apache.shenyu.admin.service.impl;

import okhttp3.Response;
import org.apache.shenyu.admin.config.properties.HttpRecordProperties;
import org.apache.shenyu.admin.model.dto.ShenyuHttpRequestRecordDTO;
import org.apache.shenyu.admin.record.HttpRecordRepository;
import org.apache.shenyu.admin.record.entity.RecordTaskInfo;
import org.apache.shenyu.admin.record.entity.ReplayProgress;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.common.constant.Constants;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadPoolExecutor;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class HttpRecordServiceImplTest {

    @Mock
    private ThreadPoolExecutor replayExecutor;

    @Mock
    private HttpRecordProperties properties;

    @Mock
    private HttpUtils httpUtils;

    @Mock
    private HttpRecordRepository localRepository;

    @Mock
    private HttpRecordRepository hdfsRepository;

    private HttpRecordServiceImpl service;

    @BeforeEach
    void setUp() throws IOException {
        MockitoAnnotations.openMocks(this);

        when(properties.getStorageType()).thenReturn("local");
        Map<String, HttpRecordRepository> repositoryMap = new HashMap<>();
        repositoryMap.put("local", localRepository);
        repositoryMap.put("hdfs", hdfsRepository);

        doAnswer(invocation -> {
            ((Runnable) invocation.getArgument(0)).run();
            return null;
        }).when(replayExecutor).submit(any(Runnable.class));

        Response mockResponse = mock(Response.class);
        when(mockResponse.code()).thenReturn(200);
        when(httpUtils.requestJson(anyString(), anyString(), anyMap(), any(HttpUtils.HTTPMethod.class)))
                .thenReturn(mockResponse);

        service = new HttpRecordServiceImpl(replayExecutor, properties, httpUtils, repositoryMap);
    }

    private ShenyuHttpRequestRecordDTO.Record buildRecord(final String method, final String uri,
                                                          final String queryParams, final String body,
                                                          final Map<String, String> headers) {
        ShenyuHttpRequestRecordDTO.Record record = new ShenyuHttpRequestRecordDTO.Record();
        record.setTaskId("task-1");
        record.setTraceId("trace-1");
        record.setMethod(method);
        record.setRequestUri(uri);
        record.setQueryParams(queryParams);
        record.setRequestBody(body);
        record.setRequestHeaders(headers);
        return record;
    }

    @Test
    void testListRecordTasks() {
        RecordTaskInfo taskInfo = new RecordTaskInfo("task-1", 10L, System.currentTimeMillis());
        when(localRepository.listRecordTasks()).thenReturn(Collections.singletonList(taskInfo));

        List<RecordTaskInfo> result = service.listRecordTasks();

        assertEquals(1, result.size());
        assertEquals("task-1", result.get(0).getTaskId());
        verify(localRepository).listRecordTasks();
    }

    @Test
    void testStartReplayThrowsWhenNoRecords() {
        when(localRepository.loadRecords("empty-task")).thenReturn(Collections.emptyList());

        assertThrows(IllegalArgumentException.class, () -> service.startReplay("empty-task", "http://localhost"));
    }

    @Test
    void testStartReplayReturnsReplayId() {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("GET", "/api/test", null, null, null);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));

        String replayId = service.startReplay("task-1", "http://localhost:9195");

        assertNotNull(replayId);
    }

    @Test
    void testStartReplayProgressTotalMatchesRecordCount() {
        List<ShenyuHttpRequestRecordDTO.Record> records = Arrays.asList(
                buildRecord("GET", "/api/a", null, null, null),
                buildRecord("POST", "/api/b", null, "{}", null),
                buildRecord("PUT", "/api/c", null, "{}", null)
        );
        when(localRepository.loadRecords("task-1")).thenReturn(records);

        String replayId = service.startReplay("task-1", "http://localhost:9195");

        ReplayProgress progress = service.getReplayProgress(replayId);
        assertNotNull(progress);
        assertEquals(3, progress.getTotal());
        assertEquals(3, progress.getSucceeded());
        assertEquals(0, progress.getFailed());
    }

    @Test
    void testGetReplayProgressReturnsNullForUnknownId() {
        assertNull(service.getReplayProgress("nonexistent"));
    }

    @Test
    void testReplaySuccessIncrementsSucceeded() {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("GET", "/api/test", null, null, null);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));

        String replayId = service.startReplay("task-1", "http://localhost:9195");

        ReplayProgress progress = service.getReplayProgress(replayId);
        assertNotNull(progress);
        assertEquals(1, progress.getSucceeded());
        assertEquals(0, progress.getFailed());
        assertTrue(progress.isFinished());
    }

    @Test
    void testReplayFailureIncrementsFailed() throws IOException {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("GET", "/api/test", null, null, null);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));
        when(httpUtils.requestJson(anyString(), anyString(), anyMap(), any(HttpUtils.HTTPMethod.class)))
                .thenThrow(new IOException("connection refused"));

        String replayId = service.startReplay("task-1", "http://localhost:9195");

        ReplayProgress progress = service.getReplayProgress(replayId);
        assertNotNull(progress);
        assertEquals(0, progress.getSucceeded());
        assertEquals(1, progress.getFailed());
        assertTrue(progress.isFinished());
    }

    @Test
    void testReplayBuildsUrlWithQueryParams() throws IOException {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("GET", "/api/test", "foo=bar&baz=1", null, null);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));

        service.startReplay("task-1", "http://localhost:9195");

        verify(httpUtils).requestJson(
                eq("http://localhost:9195/api/test?foo=bar&baz=1"),
                anyString(),
                anyMap(),
                any(HttpUtils.HTTPMethod.class)
        );
    }

    @Test
    void testReplayBuildsUrlWithoutQueryParams() throws IOException {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("GET", "/api/test", null, null, null);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));

        service.startReplay("task-1", "http://localhost:9195");

        verify(httpUtils).requestJson(
                eq("http://localhost:9195/api/test"),
                anyString(),
                anyMap(),
                any(HttpUtils.HTTPMethod.class)
        );
    }

    @Test
    void testReplayStripsTrailingSlashFromHost() throws IOException {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("GET", "/api/test", null, null, null);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));

        service.startReplay("task-1", "http://localhost:9195/");

        verify(httpUtils).requestJson(
                eq("http://localhost:9195/api/test"),
                anyString(),
                anyMap(),
                any(HttpUtils.HTTPMethod.class)
        );
    }

    @SuppressWarnings("unchecked")
    @Test
    void testReplayHeadersFilterContentLengthAndHost() throws IOException {
        Map<String, String> originalHeaders = new HashMap<>();
        originalHeaders.put("Content-Type", "application/json");
        originalHeaders.put("Content-Length", "256");
        originalHeaders.put("Host", "original-host.com");
        originalHeaders.put("Authorization", "Bearer token");

        ShenyuHttpRequestRecordDTO.Record record = buildRecord("POST", "/api/test", null, "{}", originalHeaders);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));

        service.startReplay("task-1", "http://localhost:9195");

        ArgumentCaptor<Map<String, String>> headersCaptor = ArgumentCaptor.forClass(Map.class);
        verify(httpUtils).requestJson(anyString(), anyString(), headersCaptor.capture(), any(HttpUtils.HTTPMethod.class));

        Map<String, String> replayHeaders = headersCaptor.getValue();
        assertEquals("application/json", replayHeaders.get("Content-Type"));
        assertEquals("Bearer token", replayHeaders.get("Authorization"));
        assertNull(replayHeaders.get("Content-Length"));
        assertNull(replayHeaders.get("Host"));
    }

    @SuppressWarnings("unchecked")
    @Test
    void testReplayHeadersAddShenyuReplayFlag() throws IOException {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("GET", "/api/test", null, null, null);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));

        service.startReplay("task-1", "http://localhost:9195");

        ArgumentCaptor<Map<String, String>> headersCaptor = ArgumentCaptor.forClass(Map.class);
        verify(httpUtils).requestJson(anyString(), anyString(), headersCaptor.capture(), any(HttpUtils.HTTPMethod.class));

        Map<String, String> replayHeaders = headersCaptor.getValue();
        assertEquals("true", replayHeaders.get(Constants.X_SHENYU_REPLAY));
    }

    @SuppressWarnings("unchecked")
    @Test
    void testReplayHeadersWithNullOriginalHeaders() throws IOException {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("GET", "/api/test", null, null, null);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));

        service.startReplay("task-1", "http://localhost:9195");

        ArgumentCaptor<Map<String, String>> headersCaptor = ArgumentCaptor.forClass(Map.class);
        verify(httpUtils).requestJson(anyString(), anyString(), headersCaptor.capture(), any(HttpUtils.HTTPMethod.class));

        Map<String, String> replayHeaders = headersCaptor.getValue();
        assertEquals(1, replayHeaders.size());
        assertEquals("true", replayHeaders.get(Constants.X_SHENYU_REPLAY));
    }

    @Test
    void testRepositorySelectionByStorageType() {
        when(properties.getStorageType()).thenReturn("hdfs");
        Map<String, HttpRecordRepository> repositoryMap = new HashMap<>();
        repositoryMap.put("local", localRepository);
        repositoryMap.put("hdfs", hdfsRepository);

        HttpRecordServiceImpl svc = new HttpRecordServiceImpl(replayExecutor, properties, httpUtils, repositoryMap);

        RecordTaskInfo taskInfo = new RecordTaskInfo("task-1", 5L, System.currentTimeMillis());
        when(hdfsRepository.listRecordTasks()).thenReturn(Collections.singletonList(taskInfo));

        List<RecordTaskInfo> result = svc.listRecordTasks();
        assertEquals(1, result.size());
        verify(hdfsRepository).listRecordTasks();
        verify(localRepository, org.mockito.Mockito.never()).listRecordTasks();
    }

    @Test
    void testRepositoryFallsBackToLocal() {
        when(properties.getStorageType()).thenReturn("unknown");
        Map<String, HttpRecordRepository> repositoryMap = new HashMap<>();
        repositoryMap.put("local", localRepository);
        repositoryMap.put("hdfs", hdfsRepository);

        HttpRecordServiceImpl svc = new HttpRecordServiceImpl(replayExecutor, properties, httpUtils, repositoryMap);

        RecordTaskInfo taskInfo = new RecordTaskInfo("task-1", 5L, System.currentTimeMillis());
        when(localRepository.listRecordTasks()).thenReturn(Collections.singletonList(taskInfo));

        List<RecordTaskInfo> result = svc.listRecordTasks();
        assertEquals(1, result.size());
        verify(localRepository).listRecordTasks();
    }

    @Test
    void testReplaySendsCorrectMethodAndBody() throws IOException {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("POST", "/api/test", null, "{\"key\":\"value\"}", null);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));

        service.startReplay("task-1", "http://localhost:9195");

        verify(httpUtils).requestJson(
                eq("http://localhost:9195/api/test"),
                eq("{\"key\":\"value\"}"),
                anyMap(),
                eq(HttpUtils.HTTPMethod.POST)
        );
    }

    @Test
    void testReplaySendsEmptyBodyWhenNull() throws IOException {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("GET", "/api/test", null, null, null);
        when(localRepository.loadRecords("task-1")).thenReturn(Collections.singletonList(record));

        service.startReplay("task-1", "http://localhost:9195");

        verify(httpUtils).requestJson(
                anyString(),
                eq(""),
                anyMap(),
                any(HttpUtils.HTTPMethod.class)
        );
    }

    @Test
    void testStartReplaySubmitsAllRecordsToExecutor() {
        List<ShenyuHttpRequestRecordDTO.Record> records = Arrays.asList(
                buildRecord("GET", "/api/a", null, null, null),
                buildRecord("POST", "/api/b", null, null, null)
        );
        when(localRepository.loadRecords("task-1")).thenReturn(records);

        service.startReplay("task-1", "http://localhost:9195");

        verify(replayExecutor, org.mockito.Mockito.times(2)).submit(any(Runnable.class));
    }

    @Test
    void testStartReplayProgressTracksTaskId() {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("GET", "/api/test", null, null, null);
        when(localRepository.loadRecords("my-task")).thenReturn(Collections.singletonList(record));

        String replayId = service.startReplay("my-task", "http://localhost:9195");

        ReplayProgress progress = service.getReplayProgress(replayId);
        assertNotNull(progress);
        assertEquals("my-task", progress.getTaskId());
        assertEquals(replayId, progress.getReplayId());
    }
}
