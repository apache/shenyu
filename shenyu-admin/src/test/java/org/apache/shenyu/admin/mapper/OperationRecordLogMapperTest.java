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

package org.apache.shenyu.admin.mapper;

import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.OperationRecordLog;
import org.apache.shenyu.admin.model.query.RecordLogQueryCondition;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;

import java.util.Date;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * test for {@linkplain OperationRecordLogMapper}.
 */
public class OperationRecordLogMapperTest extends AbstractSpringIntegrationTest {

    private static final Long ID = 1270728683L;

    private static final String RED = "red";

    private static final String KEYWORD = "keyword";

    private static final String USERNAME = "username";

    private static final String OPERATION_TYPE = "type";

    private static final Date OPERATION_TIME = new Date(1270728683L);

    @Resource
    private OperationRecordLogMapper operationRecordLogMapper;

    private final OperationRecordLog operationRecordLog = buildOperationRecordLog();

    @BeforeEach
    public void before() {
        assertEquals(operationRecordLogMapper.insert(operationRecordLog), 1);
    }

    @Test
    public void testSelectByCondition() {
        RecordLogQueryCondition matchCondition = new RecordLogQueryCondition();
        matchCondition.setUsername(USERNAME);
        RecordLogQueryCondition unMatchCondition = new RecordLogQueryCondition();
        unMatchCondition.setType("unmatch");
        assertNotNull(operationRecordLogMapper.selectByCondition(matchCondition));
        assertEquals((operationRecordLogMapper.selectByCondition(unMatchCondition)).size(), 0);
    }

    @Test
    public void testSelectLimit() {
        List<OperationRecordLog> list = operationRecordLogMapper.selectLimit("username", 1);
        assertNotNull(list);
        assertEquals(list.size(), 1);
        assertEquals(list.get(0).getOperator(), USERNAME);
        assertEquals(list.get(0).getOperationTime(), OPERATION_TIME);
        assertEquals(list.get(0).getOperationType(), OPERATION_TYPE);
        assertEquals(list.get(0).getColor(), RED);
        assertEquals(list.get(0).getContext(), KEYWORD);
        assertEquals(list.get(0).getId(), ID);
    }

    @AfterEach
    public void testDeleteByBefore() {
        assertEquals(operationRecordLogMapper.deleteByBefore(new Date(1270728684L)), 1);
    }

    private OperationRecordLog buildOperationRecordLog() {
        OperationRecordLog res = new OperationRecordLog();
        res.setId(ID);
        res.setColor(RED);
        res.setOperator(USERNAME);
        res.setOperationTime(OPERATION_TIME);
        res.setContext(KEYWORD);
        res.setOperationType(OPERATION_TYPE);
        return res;
    }
}
