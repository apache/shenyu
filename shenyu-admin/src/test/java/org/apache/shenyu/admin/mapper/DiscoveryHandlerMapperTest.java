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
import org.apache.shenyu.admin.model.entity.DiscoveryHandlerDO;
import org.apache.shenyu.admin.model.entity.DiscoveryRelDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

public class DiscoveryHandlerMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private DiscoveryHandlerMapper discoveryHandlerMapper;

    @Resource
    private DiscoveryRelMapper discoveryRelMapper;

    private final DiscoveryHandlerDO discoveryHandlerDO = buildDiscoveryHandlerDO();

    private final DiscoveryRelDO discoveryRelDO = buildDiscoveryRelDO();

    private DiscoveryHandlerDO buildDiscoveryHandlerDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        DiscoveryHandlerDO discoveryHandlerDO = new DiscoveryHandlerDO();
        discoveryHandlerDO.setId("1721396397234827264");
        discoveryHandlerDO.setDiscoveryId("33333333333333333");
        discoveryHandlerDO.setHandler(UUIDUtils.getInstance().generateShortUuid());
        discoveryHandlerDO.setListenerNode(UUIDUtils.getInstance().generateShortUuid());
        discoveryHandlerDO.setDateCreated(now);
        discoveryHandlerDO.setDateUpdated(now);
        discoveryHandlerDO.setProps("");
        return discoveryHandlerDO;
    }

    private DiscoveryRelDO buildDiscoveryRelDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        DiscoveryRelDO discoveryRelDO = new DiscoveryRelDO();
        discoveryRelDO.setId("1821396397234827264");
        discoveryRelDO.setDateUpdated(now);
        discoveryRelDO.setDateCreated(now);
        discoveryRelDO.setPluginName("pluginName");
        discoveryRelDO.setDiscoveryHandlerId("1721396397234827264");
        discoveryRelDO.setSelectorId("4444444444444");
        discoveryRelDO.setProxySelectorId("222222222222");
        return discoveryRelDO;
    }

    @BeforeEach
    public void before() {
        int count = discoveryHandlerMapper.insert(discoveryHandlerDO);
        assertEquals(1, count);
        int count2 = discoveryRelMapper.insertSelective(discoveryRelDO);
        assertEquals(1, count2);

    }

    @AfterEach
    public void after() {
        int count = discoveryHandlerMapper.delete("1721396397234827264");
        assertEquals(1, count);
        int count2 = discoveryRelMapper.delete("1821396397234827264");
        assertEquals(1, count2);
    }

    @Test
    public void testSelectByProxySelectorId() {
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectByProxySelectorId("222222222222");
        assertNotNull(discoveryHandlerDO);
    }

    @Test
    public void testSelectAll() {
        List<DiscoveryHandlerDO> discoveryHandlerDOS = discoveryHandlerMapper.selectAll();
        assertThat(discoveryHandlerDOS.size(), greaterThan(0));
    }

    @Test
    public void testSelectByDiscoveryId() {
        List<DiscoveryHandlerDO> discoveryHandlerDOS = discoveryHandlerMapper.selectByDiscoveryId("33333333333333333");
        assertThat(discoveryHandlerDOS.size(), greaterThan(0));
    }

    @Test
    public void testSelectBySelectorId() {
        DiscoveryHandlerDO discoveryHandlerDO = discoveryHandlerMapper.selectBySelectorId("4444444444444");
        assertNotNull(discoveryHandlerDO);
    }

}
