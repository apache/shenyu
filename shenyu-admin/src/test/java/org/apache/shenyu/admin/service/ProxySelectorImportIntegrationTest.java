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
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.common.dto.ProxySelectorData;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verify proxy selector imports are atomic, including their ID mappings.
 */
public class ProxySelectorImportIntegrationTest extends AbstractSpringIntegrationTest {

    @Resource
    private ProxySelectorService proxySelectorService;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @Resource
    private PlatformTransactionManager transactionManager;

    @Test
    public void testLegacyImportRollsBackEarlierRows() {
        assertThrows(DataIntegrityViolationException.class,
                () -> proxySelectorService.importData(List.of(selector("first", "valid"), selector("second", null))));
        assertEquals(0, countImported());
    }

    @Test
    public void testNamespaceImportFailurePreservesContextAndInput() {
        ConfigsImportContext context = new ConfigsImportContext();
        context.getProxySelectorIdMapping().put("existing", "existing-id");
        ProxySelectorData first = selector("first", "valid");
        assertThrows(DataIntegrityViolationException.class,
                () -> proxySelectorService.importData("import-target", List.of(first, selector("second", null)), context));
        assertEquals(0, countImported());
        assertEquals(Map.of("existing", "existing-id"), context.getProxySelectorIdMapping());
        assertEquals("first", first.getId());
        assertEquals("import-source", first.getNamespaceId());
    }

    @Test
    public void testNamespaceImportPublishesCommittedMapping() {
        ConfigsImportContext context = new ConfigsImportContext();
        try {
            assertEquals(1, proxySelectorService.importData("import-target", List.of(selector("first", "valid")), context).getSuccessCount());
            String id = context.getProxySelectorIdMapping().get("first");
            assertNotNull(id);
            assertEquals("import-target", jdbcTemplate.queryForObject("SELECT namespace_id FROM proxy_selector WHERE id = ?", String.class, id));
        } finally {
            jdbcTemplate.update("DELETE FROM proxy_selector WHERE namespace_id = 'import-target'");
        }
    }

    @Test
    public void testOuterRollbackDoesNotPublishMapping() {
        ConfigsImportContext context = new ConfigsImportContext();
        new TransactionTemplate(transactionManager).executeWithoutResult(status -> {
            proxySelectorService.importData("import-target", List.of(selector("first", "valid")), context);
            assertEquals(1, countImported());
            assertTrue(context.getProxySelectorIdMapping().isEmpty());
            status.setRollbackOnly();
        });
        assertEquals(0, countImported());
        assertTrue(context.getProxySelectorIdMapping().isEmpty());
    }

    private int countImported() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM proxy_selector WHERE namespace_id IN ('import-source', 'import-target')", Integer.class);
    }

    private ProxySelectorData selector(final String id, final String name) {
        ProxySelectorData data = new ProxySelectorData();
        data.setId(id);
        data.setName(name);
        data.setPluginName("tcp");
        data.setType("tcp");
        data.setForwardPort(18080);
        data.setNamespaceId("import-source");
        return data;
    }
}

