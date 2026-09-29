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

package org.apache.shenyu.admin.jpa.repository;

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.OperationRecordLog;
import org.apache.shenyu.admin.model.query.RecordLogQueryCondition;
import org.junit.jupiter.api.Test;

import java.util.Date;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class OperationRecordLogRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private OperationRecordLogRepository operationRecordLogRepository;

    @Test
    void saveGeneratesIdentityId() {
        OperationRecordLog recordLog = buildRecordLog("admin", "create selector success");
        OperationRecordLog saved = operationRecordLogRepository.save(recordLog);
        assertNotNull(saved.getId());
    }

    @Test
    void selectByConditionFiltersByTimeKeywordAndOperator() {
        long base = System.currentTimeMillis();
        operationRecordLogRepository.save(buildRecordLogAt("admin", "create selector success", base));
        operationRecordLogRepository.save(buildRecordLogAt("admin", "delete rule success", base));
        operationRecordLogRepository.save(buildRecordLogAt("guest", "create selector success", base));

        RecordLogQueryCondition condition = new RecordLogQueryCondition();
        condition.setStartTime(new Date(base - 60_000L));
        condition.setEndTime(new Date(base + 60_000L));
        condition.setKeyword("selector");
        condition.setUsername("admin");
        List<OperationRecordLog> matched = operationRecordLogRepository.selectByCondition(condition);
        assertEquals(1, matched.size());
        assertEquals("create selector success", matched.get(0).getContext());

        condition.setExcluded("selector");
        assertTrue(operationRecordLogRepository.selectByCondition(condition).isEmpty());
    }

    @Test
    void deleteByBeforeRemovesOnlyOlderRows() {
        long base = System.currentTimeMillis();
        OperationRecordLog old = operationRecordLogRepository.save(buildRecordLogAt("admin", "old record", base - 10_000_000L));
        operationRecordLogRepository.save(buildRecordLogAt("admin", "new record", base));

        int deleted = operationRecordLogRepository.deleteByBefore(new Date(base - 5_000_000L));
        assertEquals(1, deleted);
        assertTrue(operationRecordLogRepository.findById(old.getId()).isEmpty());
    }

    private OperationRecordLog buildRecordLog(final String operator, final String context) {
        return buildRecordLogAt(operator, context, System.currentTimeMillis());
    }

    private OperationRecordLog buildRecordLogAt(final String operator, final String context, final long time) {
        OperationRecordLog recordLog = new OperationRecordLog();
        recordLog.setColor("green");
        recordLog.setContext(context);
        recordLog.setOperator(operator);
        recordLog.setOperationTime(new Date(time));
        recordLog.setOperationType("create");
        return recordLog;
    }
}
