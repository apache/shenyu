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

package org.apache.shenyu.admin.record;

import org.apache.shenyu.admin.config.properties.HttpRecordProperties;
import org.apache.shenyu.admin.model.dto.ShenyuHttpRequestRecordDTO;
import org.apache.shenyu.admin.record.entity.RecordTaskInfo;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

class LocalJsonFileHttpRecordRepositoryTest {

    @TempDir
    private Path tempDir;

    @Mock
    private HttpRecordProperties properties;

    private LocalJsonFileHttpRecordRepository repository;

    @BeforeEach
    void setUp() {
        MockitoAnnotations.openMocks(this);
        when(properties.getStoragePath()).thenReturn(tempDir.toString());
        repository = new LocalJsonFileHttpRecordRepository(properties);
    }

    private ShenyuHttpRequestRecordDTO.Record buildRecord(final String taskId, final String traceId,
                                                          final String method, final String uri,
                                                          final String queryParams, final String body,
                                                          final Map<String, String> headers) {
        ShenyuHttpRequestRecordDTO.Record record = new ShenyuHttpRequestRecordDTO.Record();
        record.setTaskId(taskId);
        record.setTraceId(traceId);
        record.setMethod(method);
        record.setRequestUri(uri);
        record.setQueryParams(queryParams);
        record.setRequestBody(body);
        record.setRequestHeaders(headers);
        return record;
    }

    private ShenyuHttpRequestRecordDTO buildDTO(final List<ShenyuHttpRequestRecordDTO.Record> records) {
        ShenyuHttpRequestRecordDTO dto = new ShenyuHttpRequestRecordDTO();
        dto.setRecords(records);
        return dto;
    }

    @Test
    void testSaveCreatesFileAndWritesRecords() throws IOException {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("task-1", "trace-1", "GET", "/api/test", null, null, null);
        repository.save(buildDTO(Collections.singletonList(record)));

        Path file = tempDir.resolve("task-1.jsonl");
        assertTrue(Files.exists(file));
        List<String> lines = Files.readAllLines(file);
        assertEquals(1, lines.size());
        assertTrue(lines.get(0).contains("\"traceId\":\"trace-1\""));
        assertTrue(lines.get(0).contains("\"method\":\"GET\""));
    }

    @Test
    void testSaveGroupsByTaskId() throws IOException {
        ShenyuHttpRequestRecordDTO.Record r1 = buildRecord("task-a", "trace-1", "GET", "/a", null, null, null);
        ShenyuHttpRequestRecordDTO.Record r2 = buildRecord("task-b", "trace-2", "POST", "/b", null, null, null);
        ShenyuHttpRequestRecordDTO.Record r3 = buildRecord("task-a", "trace-3", "PUT", "/c", null, null, null);
        repository.save(buildDTO(Arrays.asList(r1, r2, r3)));

        Path fileA = tempDir.resolve("task-a.jsonl");
        Path fileB = tempDir.resolve("task-b.jsonl");
        assertTrue(Files.exists(fileA));
        assertTrue(Files.exists(fileB));
        assertEquals(2, Files.readAllLines(fileA).size());
        assertEquals(1, Files.readAllLines(fileB).size());
    }

    @Test
    void testSaveAppendsToExistingFile() throws IOException {
        ShenyuHttpRequestRecordDTO.Record r1 = buildRecord("task-1", "trace-1", "GET", "/a", null, null, null);
        repository.save(buildDTO(Collections.singletonList(r1)));

        ShenyuHttpRequestRecordDTO.Record r2 = buildRecord("task-1", "trace-2", "POST", "/b", null, null, null);
        repository.save(buildDTO(Collections.singletonList(r2)));

        Path file = tempDir.resolve("task-1.jsonl");
        List<String> lines = Files.readAllLines(file);
        assertEquals(2, lines.size());
        assertTrue(lines.get(0).contains("trace-1"));
        assertTrue(lines.get(1).contains("trace-2"));
    }

    @Test
    void testSaveCreatesDirectoryIfNotExists() {
        Path nestedDir = tempDir.resolve("nested").resolve("sub");
        when(properties.getStoragePath()).thenReturn(nestedDir.toString());

        LocalJsonFileHttpRecordRepository repo = new LocalJsonFileHttpRecordRepository(properties);
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("task-1", "trace-1", "GET", "/api/test", null, null, null);
        repo.save(buildDTO(Collections.singletonList(record)));

        assertTrue(Files.exists(nestedDir.resolve("task-1.jsonl")));
    }

    @Test
    void testSaveWithEmptyRecords() {
        repository.save(buildDTO(Collections.emptyList()));

        try (var paths = Files.list(tempDir)) {
            assertEquals(0, paths.count());
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    @Test
    void testListRecordTasksReturnsTaskInfo() throws IOException {
        ShenyuHttpRequestRecordDTO.Record r1 = buildRecord("task-1", "trace-1", "GET", "/a", null, null, null);
        ShenyuHttpRequestRecordDTO.Record r2 = buildRecord("task-1", "trace-2", "POST", "/b", null, null, null);
        repository.save(buildDTO(Arrays.asList(r1, r2)));

        List<RecordTaskInfo> tasks = repository.listRecordTasks();
        assertEquals(1, tasks.size());
        RecordTaskInfo info = tasks.get(0);
        assertEquals("task-1", info.getTaskId());
        assertEquals(2, info.getRecordCount());
        assertTrue(info.getLastModifiedMs() > 0);
    }

    @Test
    void testListRecordTasksEmptyWhenDirNotExists() {
        when(properties.getStoragePath()).thenReturn(tempDir.resolve("nonexistent").toString());
        LocalJsonFileHttpRecordRepository repo = new LocalJsonFileHttpRecordRepository(properties);

        List<RecordTaskInfo> tasks = repo.listRecordTasks();
        assertTrue(tasks.isEmpty());
    }

    @Test
    void testListRecordTasksIgnoresNonJsonlFiles() throws IOException {
        Files.writeString(tempDir.resolve("task-1.jsonl"), "{\"taskId\":\"task-1\"}" + System.lineSeparator());
        Files.writeString(tempDir.resolve("readme.txt"), "not a record file");
        Files.writeString(tempDir.resolve("config.yaml"), "key: value");

        List<RecordTaskInfo> tasks = repository.listRecordTasks();
        assertEquals(1, tasks.size());
        assertEquals("task-1", tasks.get(0).getTaskId());
    }

    @Test
    void testListRecordTasksEmptyDir() {
        List<RecordTaskInfo> tasks = repository.listRecordTasks();
        assertTrue(tasks.isEmpty());
    }

    @Test
    void testLoadRecordsReturnsCorrectRecords() throws IOException {
        Map<String, String> headers = new HashMap<>();
        headers.put("Content-Type", "application/json");
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("task-1", "trace-1", "POST", "/api/test",
                "foo=bar", "{\"key\":\"val\"}", headers);
        repository.save(buildDTO(Collections.singletonList(record)));

        List<ShenyuHttpRequestRecordDTO.Record> loaded = repository.loadRecords("task-1");
        assertEquals(1, loaded.size());
        ShenyuHttpRequestRecordDTO.Record r = loaded.get(0);
        assertEquals("task-1", r.getTaskId());
        assertEquals("trace-1", r.getTraceId());
        assertEquals("POST", r.getMethod());
        assertEquals("/api/test", r.getRequestUri());
        assertEquals("foo=bar", r.getQueryParams());
        assertEquals("{\"key\":\"val\"}", r.getRequestBody());
        assertNotNull(r.getRequestHeaders());
        assertEquals("application/json", r.getRequestHeaders().get("Content-Type"));
    }

    @Test
    void testLoadRecordsReturnsEmptyWhenFileNotExists() {
        List<ShenyuHttpRequestRecordDTO.Record> loaded = repository.loadRecords("nonexistent");
        assertTrue(loaded.isEmpty());
    }

    @Test
    void testLoadRecordsSkipsBlankLines() throws IOException {
        Path file = tempDir.resolve("task-1.jsonl");
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("task-1", "trace-1", "GET", "/a", null, null, null);
        String jsonLine = new com.google.gson.Gson().toJson(record);
        Files.writeString(file, jsonLine + System.lineSeparator()
                + System.lineSeparator()
                + "   " + System.lineSeparator()
                + jsonLine + System.lineSeparator());

        List<ShenyuHttpRequestRecordDTO.Record> loaded = repository.loadRecords("task-1");
        assertEquals(2, loaded.size());
    }

    @Test
    void testLoadRecordsThrowsOnInvalidJson() throws IOException {
        Path file = tempDir.resolve("task-1.jsonl");
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("task-1", "trace-1", "GET", "/a", null, null, null);
        String jsonLine = new com.google.gson.Gson().toJson(record);
        Files.writeString(file, jsonLine + System.lineSeparator()
                + "this is not json" + System.lineSeparator());

        assertThrows(com.google.gson.JsonSyntaxException.class, () -> repository.loadRecords("task-1"));
    }

    @Test
    void testSaveAndLoadRoundTrip() {
        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token123");
        headers.put("X-Custom", "value");
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("task-1", "trace-abc", "POST", "/api/submit",
                "page=1&size=10", "{\"name\":\"test\"}", headers);
        repository.save(buildDTO(Collections.singletonList(record)));

        List<ShenyuHttpRequestRecordDTO.Record> loaded = repository.loadRecords("task-1");
        assertEquals(1, loaded.size());
        ShenyuHttpRequestRecordDTO.Record r = loaded.get(0);
        assertEquals("task-1", r.getTaskId());
        assertEquals("trace-abc", r.getTraceId());
        assertEquals("POST", r.getMethod());
        assertEquals("/api/submit", r.getRequestUri());
        assertEquals("page=1&size=10", r.getQueryParams());
        assertEquals("{\"name\":\"test\"}", r.getRequestBody());
        assertEquals("Bearer token123", r.getRequestHeaders().get("Authorization"));
        assertEquals("value", r.getRequestHeaders().get("X-Custom"));
    }

    @Test
    void testListRecordTasksWithMultipleFiles() throws IOException {
        ShenyuHttpRequestRecordDTO.Record r1 = buildRecord("alpha", "t1", "GET", "/a", null, null, null);
        ShenyuHttpRequestRecordDTO.Record r2 = buildRecord("beta", "t2", "POST", "/b", null, null, null);
        ShenyuHttpRequestRecordDTO.Record r3 = buildRecord("beta", "t3", "PUT", "/c", null, null, null);
        repository.save(buildDTO(Arrays.asList(r1, r2, r3)));

        List<RecordTaskInfo> tasks = repository.listRecordTasks();
        assertEquals(2, tasks.size());
        Map<String, RecordTaskInfo> taskMap = new HashMap<>();
        for (RecordTaskInfo info : tasks) {
            taskMap.put(info.getTaskId(), info);
        }
        assertTrue(taskMap.containsKey("alpha"));
        assertTrue(taskMap.containsKey("beta"));
        assertEquals(1, taskMap.get("alpha").getRecordCount());
        assertEquals(2, taskMap.get("beta").getRecordCount());
    }

    @Test
    void testLoadRecordsWithInvalidTaskIdPathTraversal() {
        List<ShenyuHttpRequestRecordDTO.Record> loaded = repository.loadRecords("../../etc/passwd");
        assertTrue(loaded.isEmpty());
    }

    @Test
    void testLoadRecordsWithInvalidTaskIdNull() {
        List<ShenyuHttpRequestRecordDTO.Record> loaded = repository.loadRecords(null);
        assertTrue(loaded.isEmpty());
    }

    @Test
    void testLoadRecordsWithInvalidTaskIdBlank() {
        List<ShenyuHttpRequestRecordDTO.Record> loaded = repository.loadRecords("");
        assertTrue(loaded.isEmpty());
        loaded = repository.loadRecords("   ");
        assertTrue(loaded.isEmpty());
    }

    @Test
    void testLoadRecordsWithInvalidTaskIdSpecialChars() {
        assertTrue(repository.loadRecords("task/1").isEmpty());
        assertTrue(repository.loadRecords("task\\1").isEmpty());
        assertTrue(repository.loadRecords("task.1").isEmpty());
        assertTrue(repository.loadRecords("task id").isEmpty());
        assertTrue(repository.loadRecords("task!@#").isEmpty());
    }

    @Test
    void testLoadRecordsWithValidTaskIdCharacters() throws IOException {
        ShenyuHttpRequestRecordDTO.Record record = buildRecord("task-1_abc", "trace-1", "GET", "/a", null, null, null);
        repository.save(buildDTO(Collections.singletonList(record)));

        List<ShenyuHttpRequestRecordDTO.Record> loaded = repository.loadRecords("task-1_abc");
        assertEquals(1, loaded.size());
        assertEquals("task-1_abc", loaded.get(0).getTaskId());
    }

    @Test
    void testLoadRecordsWithMultipleRecords() {
        ShenyuHttpRequestRecordDTO.Record r1 = buildRecord("task-1", "trace-1", "GET", "/a", null, null, null);
        ShenyuHttpRequestRecordDTO.Record r2 = buildRecord("task-1", "trace-2", "POST", "/b", "q=1", null, null);
        ShenyuHttpRequestRecordDTO.Record r3 = buildRecord("task-1", "trace-3", "PUT", "/c", null, "{}", null);
        repository.save(buildDTO(Arrays.asList(r1, r2, r3)));

        List<ShenyuHttpRequestRecordDTO.Record> loaded = repository.loadRecords("task-1");
        assertEquals(3, loaded.size());
        assertEquals("GET", loaded.get(0).getMethod());
        assertEquals("POST", loaded.get(1).getMethod());
        assertEquals("PUT", loaded.get(2).getMethod());
        assertEquals("q=1", loaded.get(1).getQueryParams());
        assertEquals("{}", loaded.get(2).getRequestBody());
    }
}
