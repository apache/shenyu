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
import org.apache.shenyu.admin.model.entity.ProxySelectorDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ProxySelectorQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

class ProxySelectorMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private ProxySelectorMapper proxySelectorMapper;

    private final ProxySelectorDO proxySelectorDO = build();

    @BeforeEach
    void setUp() {

        insert();
    }

    @Test
    void existed() {

        Boolean b = proxySelectorMapper.existed(proxySelectorDO.getId());
        assertEquals(true, b);
    }

    @Test
    void selectByQuery() {

        ProxySelectorDO newProxySelectorDO = build();
        newProxySelectorDO.setName("test2");
        proxySelectorMapper.insert(newProxySelectorDO);
        ProxySelectorQuery query = new ProxySelectorQuery("test2", new PageParameter());
        List<ProxySelectorDO> list = proxySelectorMapper.selectByQuery(query);
        assertEquals(list.size(), 1);
        assertEquals(list.get(0).getName(), "test2");
    }

    @Test
    void nameExisted() {

        Boolean b = proxySelectorMapper.nameExisted("test");
        assertEquals(true, b);
    }

    void insert() {

        int count = proxySelectorMapper.insert(proxySelectorDO);
        assertEquals(1, count);
    }

    @Test
    void update() {

        proxySelectorDO.setName("test1");
        int count = proxySelectorMapper.update(proxySelectorDO);
        assertEquals(1, count);
    }

    @Test
    void selectByIds() {

        List<ProxySelectorDO> list = proxySelectorMapper.selectByIds(Collections.singletonList(proxySelectorDO.getId()));
        assertEquals(list.size(), 1);
        assertEquals(list.get(0).getName(), "test");
    }

    @Test
    void deleteByIds() {

        proxySelectorMapper.deleteByIds(Collections.singletonList(proxySelectorDO.getId()));
        Boolean b = proxySelectorMapper.existed(1);
        assertNull(b);
    }

    private ProxySelectorDO build() {

        ProxySelectorDO proxySelectorDO = new ProxySelectorDO();
        proxySelectorDO.setId(UUIDUtils.getInstance().generateShortUuid());
        proxySelectorDO.setName("test");
        proxySelectorDO.setPluginName("test");
        proxySelectorDO.setForwardPort(8080);
        proxySelectorDO.setType("tcp");
        proxySelectorDO.setProps("test");
        proxySelectorDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        proxySelectorDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        return proxySelectorDO;
    }
}
