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
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Verify dict imports roll back dictionary rows and synchronous audit events.
 */
public class DictImportIntegrationTest extends AbstractSpringIntegrationTest {

    @Resource
    private ShenyuDictService shenyuDictService;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @AfterEach
    public void cleanup() {
        jdbcTemplate.update("DELETE FROM shenyu_dict WHERE type = 'dict-import-test'");
        jdbcTemplate.update("DELETE FROM operation_record_log WHERE context LIKE '%dict-import-%'");
    }

    @Test
    public void testFailedImportRollsBackRowsAndAuditLog() {
        assertThrows(DataIntegrityViolationException.class,
                () -> shenyuDictService.importData(List.of(dict("dict-import-first"), dict("dict-import-first"))));
        assertEquals(0, dictCount());
        assertEquals(0, auditCount());
    }

    @Test
    public void testSuccessfulImportCommitsRowsAndAuditLog() {
        assertEquals(2, shenyuDictService.importData(List.of(dict("dict-import-first"), dict("dict-import-second"))).getSuccessCount());
        assertEquals(2, dictCount());
        assertEquals(2, auditCount());
    }

    private int dictCount() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM shenyu_dict WHERE type = 'dict-import-test'", Integer.class);
    }

    private int auditCount() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM operation_record_log WHERE context LIKE '%dict-import-%'", Integer.class);
    }

    private ShenyuDictDTO dict(final String name) {
        ShenyuDictDTO dto = new ShenyuDictDTO();
        dto.setType("dict-import-test");
        dto.setDictCode("test-code");
        dto.setDictName(name);
        dto.setSort(1);
        dto.setEnabled(true);
        return dto;
    }
}

