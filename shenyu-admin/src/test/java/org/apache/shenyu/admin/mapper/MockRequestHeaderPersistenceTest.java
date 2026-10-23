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

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.dto.MockRequestRecordDTO;
import org.apache.shenyu.admin.model.entity.MockRequestRecordDO;
import org.apache.shenyu.admin.model.query.MockRequestRecordQuery;
import org.apache.shenyu.admin.model.vo.MockRequestRecordVO;
import org.apache.shenyu.admin.service.MockRequestRecordService;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Verifies complete header persistence and equality filtering with production mappers.
 */
@Transactional
public class MockRequestHeaderPersistenceTest extends AbstractSpringIntegrationTest {

    @Resource
    private MockRequestRecordMapper mapper;

    @Resource
    private MockRequestRecordService service;

    @ParameterizedTest
    @ValueSource(ints = {2048, 16384, 60000})
    public void testServiceSavesAndUpdatesLongHeaders(final int length) {
        MockRequestRecordDO record = record(header(length));
        MockRequestRecordDTO request = new MockRequestRecordDTO();
        request.setApiId(record.getApiId());
        request.setHost(record.getHost());
        request.setPort(record.getPort());
        request.setUrl(record.getUrl());
        request.setPathVariable(record.getPathVariable());
        request.setQuery(record.getQuery());
        request.setHeader(record.getHeader());
        assertEquals(1, service.createOrUpdate(request));
        MockRequestRecordVO saved = service.queryByApiId(record.getApiId());
        assertEquals(record.getHeader(), saved.getHeader());
        request.setId(saved.getId());
        request.setHeader(header(length) + " ");
        assertEquals(1, service.createOrUpdate(request));
        assertEquals(request.getHeader(), service.findById(saved.getId()).getHeader());
    }

    @Test
    public void testSelectiveAndBatchWritesKeepCompleteHeaders() {
        MockRequestRecordDO selective = record(header(16384));
        assertEquals(1, mapper.insertSelective(selective));
        assertEquals(selective.getHeader(), mapper.queryById(selective.getId()).getHeader());
        MockRequestRecordDO first = record(header(32768));
        MockRequestRecordDO second = record(header(16384));
        mapper.insertBatch(List.of(first, second));
        assertEquals(first.getHeader(), mapper.queryById(first.getId()).getHeader());
        assertEquals(second.getHeader(), mapper.queryById(second.getId()).getHeader());
    }

    @Test
    public void testLongHeaderEqualityFiltersCompareTheWholeValue() {
        MockRequestRecordDO record = record(header(60000));
        assertEquals(1, mapper.insert(record));
        MockRequestRecordQuery query = new MockRequestRecordQuery();
        query.setApiId(record.getApiId());
        query.setHeader(record.getHeader());
        assertEquals(1, mapper.selectByQuery(query).size());
        MockRequestRecordDO filter = new MockRequestRecordDO();
        filter.setId(record.getId());
        filter.setHeader(record.getHeader());
        assertEquals(1, mapper.queryAll(filter).size());
        assertEquals(1, mapper.count(filter));

        // Keep the long common prefix so truncated comparisons cannot pass.
        query.setHeader(record.getHeader() + "different");
        filter.setHeader(query.getHeader());
        assertTrue(mapper.selectByQuery(query).isEmpty());
        assertTrue(mapper.queryAll(filter).isEmpty());
        assertEquals(0, mapper.count(filter));
    }

    @Test
    public void testMissingHeadersRemainEmpty() {
        MockRequestRecordDO full = record(null);
        mapper.insert(full);
        assertEquals("", mapper.queryById(full.getId()).getHeader());
        MockRequestRecordDO selective = record(null);
        mapper.insertSelective(selective);
        assertEquals("", mapper.queryById(selective.getId()).getHeader());
        MockRequestRecordDO batch = record(null);
        mapper.insertBatch(List.of(batch));
        assertEquals("", mapper.queryById(batch.getId()).getHeader());
    }

    @Test
    public void testUnicodeHeaderRoundTrip() {
        MockRequestRecordDO record = record("{\"X-Note\":\"" + "汉字🙂".repeat(2000) + "\"}");
        mapper.insert(record);
        assertEquals(record.getHeader(), mapper.queryById(record.getId()).getHeader());
    }

    private String header(final int length) {
        return "{\"Authorization\":\"Bearer " + "t".repeat(length) + "\"}";
    }

    private MockRequestRecordDO record(final String header) {
        String id = UUIDUtils.getInstance().generateShortUuid();
        Timestamp time = Timestamp.valueOf("2026-01-01 00:00:00");
        return MockRequestRecordDO.builder().id(id).apiId("header-" + id).host("localhost").port(9195)
                .url("http://localhost:9195/test").pathVariable("path").query("query").header(header)
                .dateCreated(time).dateUpdated(time).build();
    }
}
