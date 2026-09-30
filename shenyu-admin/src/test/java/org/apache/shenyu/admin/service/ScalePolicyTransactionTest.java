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
import org.apache.shenyu.admin.model.dto.ScalePolicyDTO;
import org.apache.shenyu.admin.scale.scaler.ScaleService;
import org.apache.shenyu.admin.scale.scaler.cache.ScalePolicyCache;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.Statement;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.clearInvocations;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;

/**
 * Verify scale-policy activation only follows a committed update.
 */
public class ScalePolicyTransactionTest extends AbstractSpringIntegrationTest {

    @Resource
    private ScalePolicyService policyService;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @Resource
    private PlatformTransactionManager transactionManager;

    @MockBean
    private ScalePolicyCache policyCache;

    @MockBean
    private ScaleService scaleService;

    @BeforeEach
    public void setup() {
        jdbcTemplate.update("INSERT INTO scale_policy (id, sort, status, num) VALUES ('transaction-policy', 1, 1, 10)");
        clearInvocations(policyCache, scaleService);
    }

    @AfterEach
    public void cleanup() {
        jdbcTemplate.update("DELETE FROM scale_policy WHERE id = 'transaction-policy'");
    }

    @Test
    public void testRollbackDoesNotChangeCacheOrScale() {
        new TransactionTemplate(transactionManager).executeWithoutResult(status -> {
            assertEquals(1, policyService.update(policy()));
            verifyNoInteractions(policyCache, scaleService);
            status.setRollbackOnly();
        });
        assertEquals(10, persistedNum());
        verifyNoInteractions(policyCache, scaleService);
    }

    @Test
    public void testActivationSeesCommittedPolicy() {
        doAnswer(invocation -> {
            try (Connection connection = jdbcTemplate.getDataSource().getConnection();
                 Statement statement = connection.createStatement();
                 ResultSet result = statement.executeQuery("SELECT num FROM scale_policy WHERE id = 'transaction-policy'")) {
                result.next();
                assertEquals(20, result.getInt(1));
            }
            return null;
        }).when(scaleService).executeScaling();
        assertEquals(1, policyService.update(policy()));
        verify(policyCache).updatePolicy(any());
        verify(scaleService).executeScaling();
    }

    @Test
    public void testMissingPolicyDoesNotActivate() {
        ScalePolicyDTO dto = policy();
        dto.setId("missing-policy");
        assertEquals(0, policyService.update(dto));
        verifyNoInteractions(policyCache, scaleService);
    }

    @Test
    public void testExternalFailureOccursAfterDatabaseCommit() {
        doThrow(new IllegalStateException("scaling unavailable")).when(scaleService).executeScaling();
        assertThrows(IllegalStateException.class, () -> policyService.update(policy()));
        assertEquals(20, persistedNum());
        verify(policyCache).updatePolicy(any());
    }

    private int persistedNum() {
        return jdbcTemplate.queryForObject("SELECT num FROM scale_policy WHERE id = 'transaction-policy'", Integer.class);
    }

    private ScalePolicyDTO policy() {
        ScalePolicyDTO dto = new ScalePolicyDTO();
        dto.setId("transaction-policy");
        dto.setSort(1);
        dto.setStatus(1);
        dto.setNum(20);
        return dto;
    }
}

