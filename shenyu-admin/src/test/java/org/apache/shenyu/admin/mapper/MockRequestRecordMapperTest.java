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
import org.apache.shenyu.admin.model.entity.MockRequestRecordDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;
import org.springframework.transaction.annotation.Transactional;

import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for ShenyuDictMapper.
 */
public class MockRequestRecordMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private MockRequestRecordMapper mockRequestRecordMapper;

    @Test
    @Transactional
    public void insert() {
        assertEquals(mockRequestRecordMapper.insert(buildTagDO()), 1);
    }

    @Test
    @Transactional
    public void insertBatch() {
        List<MockRequestRecordDO> mockRequestRecordDOS = Arrays.asList(buildTagDO(), buildTagDO(), buildTagDO());
        assertEquals(mockRequestRecordMapper.insertBatch(mockRequestRecordDOS), 3);
    }

    @Test
    @Transactional
    public void deleteById() {
        MockRequestRecordDO mockRequestRecordDO = buildTagDO();
        assertEquals(mockRequestRecordMapper.insert(mockRequestRecordDO), 1);
        assertEquals(mockRequestRecordMapper.deleteById(mockRequestRecordDO.getId()), 1);
        assertEquals(mockRequestRecordMapper.count(mockRequestRecordDO), 0);
    }

    @Test
    @Transactional
    public void queryById() {
        MockRequestRecordDO mockRequestRecordDO = buildTagDO();
        assertEquals(mockRequestRecordMapper.insert(mockRequestRecordDO), 1);
        MockRequestRecordDO queryResult = mockRequestRecordMapper.queryById(mockRequestRecordDO.getId());
        assertEquals(queryResult.getDateCreated(), mockRequestRecordDO.getDateCreated());
        assertEquals(queryResult.getDateUpdated(), mockRequestRecordDO.getDateUpdated());
        assertEquals(queryResult.getApiId(), mockRequestRecordDO.getApiId());
        assertEquals(queryResult.getBody(), mockRequestRecordDO.getBody());
        assertEquals(queryResult.getHeader(), mockRequestRecordDO.getHeader());
        assertEquals(queryResult.getHost(), mockRequestRecordDO.getHost());
        assertEquals(queryResult.getPathVariable(), mockRequestRecordDO.getPathVariable());
        assertEquals(queryResult.getPort(), mockRequestRecordDO.getPort());
        assertEquals(queryResult.getQuery(), mockRequestRecordDO.getQuery());
    }

    @Test
    @Transactional
    public void queryAll() {
        MockRequestRecordDO mockRequestRecordDO = buildTagDO();
        assertEquals(mockRequestRecordMapper.insert(mockRequestRecordDO), 1);
        List<MockRequestRecordDO> mockRequestRecordDOS = mockRequestRecordMapper.queryAll(mockRequestRecordDO);
        assertEquals(mockRequestRecordDOS.size(), 1);
        assertEquals(mockRequestRecordDO, mockRequestRecordDOS.get(0));
    }

    @Test
    @Transactional
    public void existed() {
        MockRequestRecordDO mockRequestRecordDO = buildTagDO();
        assertNull(mockRequestRecordMapper.existed(mockRequestRecordDO.getId()));
        int insertRows = mockRequestRecordMapper.insert(mockRequestRecordDO);
        assertEquals(insertRows, 1);
        assertTrue(mockRequestRecordMapper.existed(mockRequestRecordDO.getId()));
    }

    @Test
    @Transactional
    public void count() {
        List<MockRequestRecordDO> mockRequestRecordDOS = Arrays.asList(buildTagDO(), buildTagDO(), buildTagDO());
        int insertRows = mockRequestRecordMapper.insertBatch(mockRequestRecordDOS);
        assertEquals(insertRows, 3);
        MockRequestRecordDO queryRecord = buildTagDO();
        queryRecord.setId(null);
        queryRecord.setDateCreated(null);
        queryRecord.setDateUpdated(null);
        long count = mockRequestRecordMapper.count(queryRecord);
        assertEquals(count, 3);
    }

    @Test
    @Transactional
    public void update() {
        MockRequestRecordDO mockRequestRecordDO = buildTagDO();
        int insertRows = mockRequestRecordMapper.insert(mockRequestRecordDO);
        assertEquals(insertRows, 1);
        MockRequestRecordDO updateRecord = MockRequestRecordDO.builder()
                .id(mockRequestRecordDO.getId())
                .apiId("2")
                .body("{\"name\": \"romeo\"}")
                .header("userId: 2;")
                .host("192.168.1.2")
                .pathVariable("/mock/test1")
                .port(8081)
                .query("test query")
                .dateUpdated(new Timestamp(System.currentTimeMillis()))
                .build();
        int updateRows = mockRequestRecordMapper.update(updateRecord);
        assertEquals(updateRows, 1);
        MockRequestRecordDO updateAfter = mockRequestRecordMapper.queryById(mockRequestRecordDO.getId());
        assertEquals(updateAfter.getApiId(), updateRecord.getApiId());
        assertEquals(updateAfter.getBody(), updateRecord.getBody());
        assertEquals(updateAfter.getHeader(), updateRecord.getHeader());
        assertEquals(updateAfter.getHost(), updateRecord.getHost());
        assertEquals(updateAfter.getPathVariable(), updateRecord.getPathVariable());
        assertEquals(updateAfter.getPort(), updateRecord.getPort());
        assertEquals(updateAfter.getQuery(), updateRecord.getQuery());
        assertEquals(updateAfter.getDateUpdated(), updateRecord.getDateUpdated());
    }

    private MockRequestRecordDO buildTagDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        return MockRequestRecordDO.builder()
                .id(id)
                .dateCreated(now)
                .dateUpdated(now)
                .apiId("1")
                .body("{\"name\": \"julia\"}")
                .header("userId: 1;")
                .host("192.168.1.1")
                .pathVariable("/mock/test")
                .port(8080)
                .query("")
                .build();
    }

}
