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
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.annotation.Resource;
import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

/**
 * Test cases for InstanceInfoMapper.
 */
public final class InstanceInfoMapperTest extends AbstractSpringIntegrationTest {

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
    }

    @Test
    void findAllByNamespaceId() {
        List<InstanceInfoDO> list = instanceInfoMapper.findAllByNamespaceId(instance.getNamespaceId());
        assertThat(list.size(), greaterThanOrEqualTo(1));
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
        data.setInstancePort("8080");
        data.setInstanceType("grpc");
        data.setInstanceInfo("info");
        data.setInstanceState(1);
        data.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        data.setLastHeartBeatTime(System.currentTimeMillis());
        return data;
    }
}
