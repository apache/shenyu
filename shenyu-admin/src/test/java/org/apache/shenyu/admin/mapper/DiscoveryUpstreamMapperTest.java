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
import org.apache.shenyu.admin.model.entity.DiscoveryUpstreamDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

class DiscoveryUpstreamMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private DiscoveryUpstreamMapper discoveryUpstreamMapper;

    private final DiscoveryUpstreamDO discoveryUpstreamDO = build();

    @BeforeEach
    void setUp() {

        insert();
    }

    @AfterEach
    void remove() {
        discoveryUpstreamMapper.deleteByIds(Collections.singletonList(discoveryUpstreamDO.getId()));
    }

    @Test
    void existed() {

        Boolean b = discoveryUpstreamMapper.existed(discoveryUpstreamDO.getId());
        assertEquals(true, b);
    }

    @Test
    void selectByIds() {

        List<DiscoveryUpstreamDO> dos = discoveryUpstreamMapper.selectByIds(Collections.singletonList(discoveryUpstreamDO.getId()));
        assertEquals(1, dos.size());
        assertEquals("1", dos.get(0).getDiscoveryHandlerId());
    }

    void insert() {

        int count = discoveryUpstreamMapper.insert(discoveryUpstreamDO);
        assertEquals(1, count);
    }

    @Test
    void update() {

        discoveryUpstreamDO.setDiscoveryHandlerId("2");
        discoveryUpstreamMapper.update(discoveryUpstreamDO);
        List<DiscoveryUpstreamDO> dos = discoveryUpstreamMapper.selectByIds(Collections.singletonList(discoveryUpstreamDO.getId()));
        assertEquals("2", dos.get(0).getDiscoveryHandlerId());
    }

    @Test
    void deleteByIds() {

        discoveryUpstreamMapper.deleteByIds(Collections.singletonList(discoveryUpstreamDO.getId()));
        List<DiscoveryUpstreamDO> dos = discoveryUpstreamMapper.selectByIds(Collections.singletonList(discoveryUpstreamDO.getId()));
        assertEquals(0, dos.size());
    }

    private DiscoveryUpstreamDO build() {

        DiscoveryUpstreamDO discoveryUpstreamDO = new DiscoveryUpstreamDO();
        discoveryUpstreamDO.setId(UUIDUtils.getInstance().generateShortUuid());
        discoveryUpstreamDO.setDiscoveryHandlerId("1");
        discoveryUpstreamDO.setStatus(1);
        discoveryUpstreamDO.setWeight(1);
        discoveryUpstreamDO.setProps("test");
        discoveryUpstreamDO.setUrl("test");
        discoveryUpstreamDO.setProtocol("test");
        discoveryUpstreamDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        discoveryUpstreamDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        return discoveryUpstreamDO;
    }
}
