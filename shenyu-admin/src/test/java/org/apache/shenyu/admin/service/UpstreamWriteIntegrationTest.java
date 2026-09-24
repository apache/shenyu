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


package org.apache.shenyu.admin.service;

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.discovery.DiscoveryProcessor;
import org.apache.shenyu.admin.discovery.DiscoveryProcessorHolder;
import org.apache.shenyu.admin.model.dto.DiscoveryUpstreamDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Transaction regression tests for upstream writes.
 */
public class UpstreamWriteIntegrationTest extends AbstractSpringIntegrationTest {

    @Resource
    private DiscoveryUpstreamService upstreamService;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @MockBean
    private DiscoveryProcessorHolder processorHolder;

    private DiscoveryProcessor processor;

    @BeforeEach
    public void setup() {
        processor = mock(DiscoveryProcessor.class);
        when(processorHolder.chooseProcessor("local")).thenReturn(processor);
        jdbcTemplate.update("INSERT INTO discovery (id, discovery_name, discovery_level, namespace_id, discovery_type) "
                + "VALUES ('write-test', 'write-test', '0', 'write-ns', 'local')");
        jdbcTemplate.update("INSERT INTO discovery_handler (id, discovery_id, handler) VALUES ('write-test', 'write-test', '{}')");
        jdbcTemplate.update("INSERT INTO proxy_selector (id, name, plugin_name, type, forward_port, namespace_id) "
                + "VALUES ('write-test', 'write-test', 'tcp', 'tcp', 18080, 'write-ns')");
        jdbcTemplate.update("INSERT INTO discovery_rel (id, plugin_name, discovery_handler_id, proxy_selector_id) "
                + "VALUES ('write-test', 'tcp', 'write-test', 'write-test')");
    }

    @AfterEach
    public void cleanup() {
        jdbcTemplate.update("DELETE FROM discovery_upstream WHERE discovery_handler_id = 'write-test'");
        jdbcTemplate.update("DELETE FROM discovery_rel WHERE id = 'write-test'");
        jdbcTemplate.update("DELETE FROM proxy_selector WHERE id = 'write-test'");
        jdbcTemplate.update("DELETE FROM discovery_handler WHERE id = 'write-test'");
        jdbcTemplate.update("DELETE FROM discovery WHERE id = 'write-test'");
    }

    @Test
    public void testCreateRollsBackOnSyncFailure() {
        failSync();
        assertThrows(IllegalStateException.class, () -> upstreamService.createOrUpdate(upstream("localhost:8080")));
        assertEquals(0, countUpstreams());
    }

    @Test
    public void testUpdateRollsBackOnSyncFailure() {
        upstreamService.createOrUpdate(upstream("localhost:8080"));
        DiscoveryUpstreamDTO update = upstream("localhost:8081");
        update.setId(jdbcTemplate.queryForObject("SELECT id FROM discovery_upstream WHERE discovery_handler_id = 'write-test'", String.class));
        failSync();
        assertThrows(IllegalStateException.class, () -> upstreamService.createOrUpdate(update));
        assertEquals("localhost:8080", jdbcTemplate.queryForObject("SELECT upstream_url FROM discovery_upstream WHERE id = ?", String.class, update.getId()));
    }

    @Test
    public void testBatchInsertFailureRollsBackBeforeSync() {
        assertThrows(DataIntegrityViolationException.class,
                () -> upstreamService.createOrUpdateBatch(List.of(upstream("localhost:8080"), upstream(null))));
        assertEquals(0, countUpstreams());
        verifyNoInteractions(processor);
    }

    @Test
    public void testBatchSyncFailureRollsBackAllRows() {
        failSync();
        assertThrows(IllegalStateException.class,
                () -> upstreamService.createOrUpdateBatch(List.of(upstream("localhost:8080"), upstream("localhost:8081"))));
        assertEquals(0, countUpstreams());
    }

    private void failSync() {
        doThrow(new IllegalStateException("sync failed")).when(processor).changeUpstream(any(), any());
    }

    private int countUpstreams() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM discovery_upstream WHERE discovery_handler_id = 'write-test'", Integer.class);
    }

    private DiscoveryUpstreamDTO upstream(final String url) {
        DiscoveryUpstreamDTO dto = new DiscoveryUpstreamDTO();
        dto.setDiscoveryHandlerId("write-test");
        dto.setNamespaceId("write-ns");
        dto.setProtocol("tcp");
        dto.setUrl(url);
        dto.setStatus(0);
        dto.setWeight(50);
        dto.setProps("{}");
        return dto;
    }
}

