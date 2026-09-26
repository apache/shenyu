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
import org.apache.shenyu.admin.mapper.NamespaceUserRelMapper;
import org.apache.shenyu.admin.model.entity.NamespaceUserRelDO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.model.event.rule.RuleCreatedEvent;
import org.apache.shenyu.admin.model.event.selector.SelectorCreatedEvent;
import org.apache.shenyu.admin.service.impl.DataPermissionServiceImpl;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.when;

/**
 * Verify permission listeners grant namespace users atomically.
 */
public class PermissionListenerIntegrationTest extends AbstractSpringIntegrationTest {

    @Resource
    private DataPermissionServiceImpl permissionService;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @Resource
    private PlatformTransactionManager transactionManager;

    @MockBean
    private NamespaceUserRelMapper namespaceUserRelMapper;

    @AfterEach
    public void cleanup() {
        jdbcTemplate.update("DELETE FROM data_permission WHERE data_id = 'listener-data'");
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testRepeatedEventDoesNotDuplicateGrants(final boolean rule) {
        when(namespaceUserRelMapper.selectListByNamespaceId("listener-namespace")).thenReturn(List.of(user("first"), user("second"), user("first")));
        invokeListener(rule);
        invokeListener(rule);
        assertEquals(2, countPermissions());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testInvalidUserRollsBackAllGrants(final boolean rule) {
        when(namespaceUserRelMapper.selectListByNamespaceId("listener-namespace")).thenReturn(List.of(user("first"), user(null)));
        assertThrows(DataIntegrityViolationException.class, () -> invokeListener(rule));
        assertEquals(0, countPermissions());
    }

    @ParameterizedTest
    @ValueSource(booleans = {false, true})
    public void testOuterRollbackRemovesAllGrants(final boolean rule) {
        when(namespaceUserRelMapper.selectListByNamespaceId("listener-namespace")).thenReturn(List.of(user("first"), user("second")));
        new TransactionTemplate(transactionManager).executeWithoutResult(status -> {
            invokeListener(rule);
            assertEquals(2, countPermissions());
            status.setRollbackOnly();
        });
        assertEquals(0, countPermissions());
    }

    private void invokeListener(final boolean rule) {
        if (rule) {
            RuleDO data = new RuleDO();
            data.setId("listener-data");
            data.setNamespaceId("listener-namespace");
            permissionService.onRuleCreated(new RuleCreatedEvent(data, "test"));
        } else {
            SelectorDO data = new SelectorDO();
            data.setId("listener-data");
            data.setNamespaceId("listener-namespace");
            permissionService.onSelectorCreated(new SelectorCreatedEvent(data, "test"));
        }
    }

    private NamespaceUserRelDO user(final String id) {
        NamespaceUserRelDO user = new NamespaceUserRelDO();
        user.setUserId(id);
        return user;
    }

    private int countPermissions() {
        return jdbcTemplate.queryForObject("SELECT COUNT(*) FROM data_permission WHERE data_id = 'listener-data'", Integer.class);
    }
}

