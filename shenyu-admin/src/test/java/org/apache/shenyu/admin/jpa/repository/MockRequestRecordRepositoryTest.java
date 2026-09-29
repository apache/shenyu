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
import org.apache.shenyu.admin.model.entity.MockRequestRecordDO;
import org.apache.shenyu.admin.model.query.MockRequestRecordQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

class MockRequestRecordRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private MockRequestRecordRepository mockRequestRecordRepository;

    @Test
    void selectByQueryAppliesNonNullConditionsOnly() {
        mockRequestRecordRepository.save(buildRecord("api-1", "127.0.0.1", "/get"));
        mockRequestRecordRepository.save(buildRecord("api-2", "10.0.0.1", "/post"));

        MockRequestRecordQuery query = new MockRequestRecordQuery();
        query.setApiId("api-1");
        assertEquals(1, mockRequestRecordRepository.selectByQuery(query).size());

        query.setApiId(null);
        query.setHost("10.0.0.1");
        List<MockRequestRecordDO> matched = mockRequestRecordRepository.selectByQuery(query);
        assertEquals(1, matched.size());
        assertEquals("api-2", matched.get(0).getApiId());

        query.setHost(null);
        query.setUrl("/nothing");
        assertTrue(mockRequestRecordRepository.selectByQuery(query).isEmpty());
    }

    @Test
    void findByApiIdReturnsAllRecordsOfApi() {
        mockRequestRecordRepository.save(buildRecord("api-x", "127.0.0.1", "/get"));
        mockRequestRecordRepository.save(buildRecord("api-x", "127.0.0.2", "/get"));
        mockRequestRecordRepository.save(buildRecord("api-y", "127.0.0.1", "/get"));
        assertEquals(2, mockRequestRecordRepository.findByApiId("api-x").size());
    }

    @Test
    void deleteByIdsReturnsDeletedRowCount() {
        MockRequestRecordDO first = mockRequestRecordRepository.save(buildRecord("api-d", "127.0.0.1", "/get"));
        MockRequestRecordDO second = mockRequestRecordRepository.save(buildRecord("api-d", "127.0.0.2", "/get"));
        String missingId = UUIDUtils.getInstance().generateShortUuid();

        int deleted = mockRequestRecordRepository.deleteByIds(List.of(first.getId(), second.getId(), missingId));
        assertEquals(2, deleted);
        assertTrue(mockRequestRecordRepository.findById(first.getId()).isEmpty());
    }

    @Test
    void updateLoadedEntityKeepsDateCreated() {
        MockRequestRecordDO saved = mockRequestRecordRepository.save(buildRecord("api-u", "127.0.0.1", "/get"));
        MockRequestRecordDO loaded = mockRequestRecordRepository.findById(saved.getId()).orElseThrow();
        loaded.setBody("{\"k\":\"v\"}");
        mockRequestRecordRepository.save(loaded);

        MockRequestRecordDO reloaded = mockRequestRecordRepository.findById(saved.getId()).orElseThrow();
        assertEquals("{\"k\":\"v\"}", reloaded.getBody());
        assertTrue(Objects.nonNull(reloaded.getDateCreated()));
    }

    private MockRequestRecordDO buildRecord(final String apiId, final String host, final String url) {
        MockRequestRecordDO mockRequestRecordDO = new MockRequestRecordDO();
        mockRequestRecordDO.setId(UUIDUtils.getInstance().generateShortUuid());
        mockRequestRecordDO.setApiId(apiId);
        mockRequestRecordDO.setHost(host);
        mockRequestRecordDO.setUrl(url);
        mockRequestRecordDO.setPort(8080);
        mockRequestRecordDO.setPathVariable("");
        mockRequestRecordDO.setQuery("");
        mockRequestRecordDO.setHeader("");
        return mockRequestRecordDO;
    }
}
