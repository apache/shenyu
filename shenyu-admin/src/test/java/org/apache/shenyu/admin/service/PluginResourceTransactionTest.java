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
import org.apache.shenyu.admin.mapper.ResourceMapper;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.event.plugin.PluginCreatedEvent;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.doAnswer;

/**
 * Exercise the actual event listener proxy and database rollback, without a test-managed transaction.
 */
public final class PluginResourceTransactionTest extends AbstractSpringIntegrationTest {

    private static final String PLUGIN_NAME = "review-rollback";

    @Resource
    private ApplicationEventPublisher publisher;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @SpyBean
    private ResourceMapper resourceMapper;

    private String resourceId;

    @AfterEach
    public void cleanup() {
        jdbcTemplate.update("DELETE FROM permission WHERE resource_id IN (SELECT id FROM resource WHERE name = ?)", PLUGIN_NAME);
        jdbcTemplate.update("DELETE FROM resource WHERE name = ?", PLUGIN_NAME);
    }

    @Test
    public void failedButtonInsertRollsBackResourceAndPermissionCreatedByEvents() {
        doAnswer(invocation -> {
            resourceId = jdbcTemplate.queryForObject("SELECT id FROM resource WHERE name = ?", String.class, PLUGIN_NAME);
            assertEquals(1, jdbcTemplate.queryForObject("SELECT COUNT(*) FROM permission WHERE resource_id = ?", Integer.class, resourceId));
            throw new DataIntegrityViolationException("forced button insert failure");
        }).when(resourceMapper).insertBatch(anyList());
        PluginDO plugin = new PluginDO();
        plugin.setId("review-plugin");
        plugin.setName(PLUGIN_NAME);

        assertThrows(DataIntegrityViolationException.class, () -> publisher.publishEvent(new PluginCreatedEvent(plugin, "test")));

        assertNotNull(resourceId, "The resource and permission must exist before injecting the batch failure");
        assertEquals(0, jdbcTemplate.queryForObject("SELECT COUNT(*) FROM resource WHERE name = ?", Integer.class, PLUGIN_NAME));
        assertEquals(0, jdbcTemplate.queryForObject("SELECT COUNT(*) FROM permission WHERE resource_id = ?", Integer.class, resourceId));
    }
}

