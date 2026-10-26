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
import org.apache.shenyu.admin.model.entity.InstanceInfoDO;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.apache.shenyu.admin.service.impl.InstanceInfoServiceImpl;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.annotation.Resource;
import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicInteger;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Test cases for InstanceInfoMapper.
 */
public final class InstanceInfoMapperTest extends AbstractSpringIntegrationTest {

    private static final AtomicInteger NEXT_PORT = new AtomicInteger(10000);

    private static final long FIXED_HEART_BEAT_TIME = 1700000000000L;

    @Resource
    private InstanceInfoMapper instanceInfoMapper;

    private InstanceInfoDO instance;

    @BeforeEach
    void setUp() {
        instance = buildInstance();
        int inserted = instanceInfoMapper.insert(instance);
        assertEquals(1, inserted);
    }

    @AfterEach
    void tearDown() {
        if (Objects.nonNull(instance)) {
            instanceInfoMapper.delete(instance.getId());
        }
    }

    @Test
    void existed() {
        Boolean exists = instanceInfoMapper.existed(instance.getId());
        assertEquals(true, exists);
    }

    @Test
    void selectAll() {
        List<InstanceInfoDO> list = instanceInfoMapper.selectAll();
        assertThat(list.size(), greaterThanOrEqualTo(1));
        assertTrue(list.stream().anyMatch(item -> instance.getId().equals(item.getId()) && instance.getInstanceState().equals(item.getInstanceState())));
    }

    @Test
    void findAllByNamespaceId() {
        List<InstanceInfoDO> list = instanceInfoMapper.findAllByNamespaceId(instance.getNamespaceId());
        assertThat(list.size(), greaterThanOrEqualTo(1));
        assertTrue(list.stream().anyMatch(item -> instance.getId().equals(item.getId()) && instance.getInstanceState().equals(item.getInstanceState())));
    }

    @Test
    void selectById() {
        InstanceInfoDO found = instanceInfoMapper.selectById(instance.getId());
        assertNotNull(found);
        assertEquals(instance.getId(), found.getId());
    }

    @Test
    void selectOneByQuery() {
        InstanceQuery query = new InstanceQuery();
        query.setInstanceId(instance.getId());
        query.setNamespaceId(instance.getNamespaceId());
        query.setInstanceType(instance.getInstanceType());
        query.setInstanceIp(instance.getInstanceIp());
        query.setInstancePort(instance.getInstancePort());
        InstanceInfoDO one = instanceInfoMapper.selectOneByQuery(query);
        assertThat(one, notNullValue());
        assertEquals(instance.getId(), one.getId());
    }

    @Test
    void selectByQuery() {
        InstanceQuery query = new InstanceQuery();
        query.setNamespaceId(instance.getNamespaceId());
        query.setInstanceType(instance.getInstanceType());
        query.setInstanceIp(instance.getInstanceIp());
        query.setInstancePort(instance.getInstancePort());
        List<InstanceInfoDO> list = instanceInfoMapper.selectByQuery(query);
        assertThat(list.size(), greaterThanOrEqualTo(1));
        assertTrue(list.stream().anyMatch(item -> instance.getId().equals(item.getId()) && instance.getInstanceState().equals(item.getInstanceState())));
    }

    @Test
    void insertAndDelete() {
        InstanceInfoDO data = buildInstance();
        int inserted = instanceInfoMapper.insert(data);
        assertEquals(1, inserted);
        InstanceInfoDO found = instanceInfoMapper.selectById(data.getId());
        assertNotNull(found);
        int deleted = instanceInfoMapper.delete(data.getId());
        assertEquals(1, deleted);
    }

    @Test
    void insertRoundTripsLastHeartBeatTime() {
        InstanceInfoDO data = buildInstance();
        data.setLastHeartBeatTime(FIXED_HEART_BEAT_TIME);
        assertEquals(1, instanceInfoMapper.insert(data));
        try {
            InstanceInfoDO found = instanceInfoMapper.selectById(data.getId());
            assertNotNull(found);
            assertEquals(FIXED_HEART_BEAT_TIME, found.getLastHeartBeatTime());

            InstanceQuery query = new InstanceQuery();
            query.setInstanceId(data.getId());
            query.setNamespaceId(data.getNamespaceId());
            query.setInstanceType(data.getInstanceType());
            query.setInstanceIp(data.getInstanceIp());
            query.setInstancePort(data.getInstancePort());
            InstanceInfoDO one = instanceInfoMapper.selectOneByQuery(query);
            assertNotNull(one);
            assertEquals(FIXED_HEART_BEAT_TIME, one.getLastHeartBeatTime());

            InstanceInfoDO viaResultMap = instanceInfoMapper.findAllByNamespaceId(data.getNamespaceId())
                    .stream().filter(item -> data.getId().equals(item.getId())).findFirst().orElse(null);
            assertNotNull(viaResultMap);
            assertEquals(FIXED_HEART_BEAT_TIME, viaResultMap.getLastHeartBeatTime());
        } finally {
            instanceInfoMapper.delete(data.getId());
        }
    }

    @Test
    void updateRoundTripsLastHeartBeatTime() {
        instance.setLastHeartBeatTime(FIXED_HEART_BEAT_TIME);
        int updated = instanceInfoMapper.updateById(instance);
        assertEquals(1, updated);
        InstanceInfoDO found = instanceInfoMapper.selectById(instance.getId());
        assertNotNull(found);
        assertEquals(FIXED_HEART_BEAT_TIME, found.getLastHeartBeatTime());
    }

    @Test
    void testLegacyInformationIsReadableWithoutRewritingTheStoredRow() {
        String information = "{\"cpu\":4}";
        String stored = GsonUtils.getInstance().toJson(information);
        instance.setInstanceInfo(stored);
        instanceInfoMapper.updateById(instance);
        InstanceInfoServiceImpl service = new InstanceInfoServiceImpl(instanceInfoMapper);

        assertEquals(information, service.findById(instance.getId()).getInstanceInfo());
        assertEquals(stored, instanceInfoMapper.selectById(instance.getId()).getInstanceInfo());
    }

    @Test
    void updateById() {
        instance.setInstanceType("http");
        instance.setInstanceInfo("updated-info");
        int updated = instanceInfoMapper.updateById(instance);
        assertEquals(1, updated);
        InstanceInfoDO found = instanceInfoMapper.selectById(instance.getId());
        assertEquals("http", found.getInstanceType());
        assertEquals("updated-info", found.getInstanceInfo());
    }

    private InstanceInfoDO buildInstance() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        InstanceInfoDO data = new InstanceInfoDO();
        data.setId(UUIDUtils.getInstance().generateShortUuid());
        data.setDateCreated(now);
        data.setDateUpdated(now);
        data.setInstanceIp("127.0.0.1");
        data.setInstancePort(Integer.toString(NEXT_PORT.getAndIncrement()));
        data.setInstanceType("grpc");
        data.setInstanceInfo("info");
        data.setInstanceState(1);
        data.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        data.setLastHeartBeatTime(FIXED_HEART_BEAT_TIME - 1);
        return data;
    }
}
