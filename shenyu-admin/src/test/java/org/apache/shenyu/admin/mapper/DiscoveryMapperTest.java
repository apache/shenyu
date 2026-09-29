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
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import jakarta.annotation.Resource;
import java.sql.Timestamp;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.nullValue;

/**
 * Test cases for DiscoveryMapper.
 */
public class DiscoveryMapperTest extends AbstractSpringIntegrationTest {

    private static final String ANOTHER_NAMESPACE_ID = "another-namespace-id";

    @Resource
    private DiscoveryMapper discoveryMapper;

    @Test
    public void deleteScopedByNamespace() {
        DiscoveryDO discoveryInNamespace = buildDiscoveryDO();
        DiscoveryDO discoveryInAnotherNamespace = buildDiscoveryDO();
        discoveryInAnotherNamespace.setNamespaceId(ANOTHER_NAMESPACE_ID);
        assertThat(discoveryMapper.insert(discoveryInNamespace), equalTo(1));
        assertThat(discoveryMapper.insert(discoveryInAnotherNamespace), equalTo(1));

        // wrong namespace: delete nothing
        assertThat(discoveryMapper.delete(discoveryInNamespace.getId(), ANOTHER_NAMESPACE_ID), equalTo(0));
        assertThat(discoveryMapper.selectById(discoveryInNamespace.getId()).getId(), equalTo(discoveryInNamespace.getId()));
        assertThat(discoveryMapper.selectById(discoveryInAnotherNamespace.getId()).getId(), equalTo(discoveryInAnotherNamespace.getId()));

        // matching namespace: delete only that row
        assertThat(discoveryMapper.delete(discoveryInNamespace.getId(), SYS_DEFAULT_NAMESPACE_ID), equalTo(1));
        assertThat(discoveryMapper.selectById(discoveryInNamespace.getId()), nullValue());
        assertThat(discoveryMapper.selectById(discoveryInAnotherNamespace.getId()).getId(), equalTo(discoveryInAnotherNamespace.getId()));

        // cleanup
        assertThat(discoveryMapper.delete(discoveryInAnotherNamespace.getId(), ANOTHER_NAMESPACE_ID), equalTo(1));
    }

    @Test
    public void updateScopedByNamespace() {
        DiscoveryDO discoveryInNamespace = buildDiscoveryDO();
        DiscoveryDO discoveryInAnotherNamespace = buildDiscoveryDO();
        discoveryInAnotherNamespace.setNamespaceId(ANOTHER_NAMESPACE_ID);
        assertThat(discoveryMapper.insert(discoveryInNamespace), equalTo(1));
        assertThat(discoveryMapper.insert(discoveryInAnotherNamespace), equalTo(1));

        DiscoveryDO updated = buildDiscoveryDO();
        updated.setId(discoveryInNamespace.getId());
        updated.setServerList("http://localhost:9999");
        updated.setNamespaceId(ANOTHER_NAMESPACE_ID);

        // wrong namespace: update nothing
        assertThat(discoveryMapper.update(updated), equalTo(0));

        // matching namespace: update only that row
        updated.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        assertThat(discoveryMapper.update(updated), equalTo(1));
        assertThat(discoveryMapper.selectById(discoveryInNamespace.getId()).getServerList(), equalTo("http://localhost:9999"));
        assertThat(discoveryMapper.selectById(discoveryInAnotherNamespace.getId()).getServerList(), equalTo(discoveryInAnotherNamespace.getServerList()));

        // cleanup
        assertThat(discoveryMapper.delete(discoveryInNamespace.getId(), SYS_DEFAULT_NAMESPACE_ID), equalTo(1));
        assertThat(discoveryMapper.delete(discoveryInAnotherNamespace.getId(), ANOTHER_NAMESPACE_ID), equalTo(1));
    }

    @Test
    public void updateSelectiveScopedByNamespace() {
        DiscoveryDO discoveryInNamespace = buildDiscoveryDO();
        DiscoveryDO discoveryInAnotherNamespace = buildDiscoveryDO();
        discoveryInAnotherNamespace.setNamespaceId(ANOTHER_NAMESPACE_ID);
        assertThat(discoveryMapper.insert(discoveryInNamespace), equalTo(1));
        assertThat(discoveryMapper.insert(discoveryInAnotherNamespace), equalTo(1));

        DiscoveryDO updated = DiscoveryDO.builder()
                .id(discoveryInNamespace.getId())
                .serverList("http://localhost:9998")
                .namespaceId(ANOTHER_NAMESPACE_ID)
                .dateUpdated(new Timestamp(System.currentTimeMillis()))
                .build();

        // wrong namespace: update nothing
        assertThat(discoveryMapper.updateSelective(updated), equalTo(0));

        // matching namespace: update only that row
        updated.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        assertThat(discoveryMapper.updateSelective(updated), equalTo(1));
        assertThat(discoveryMapper.selectById(discoveryInNamespace.getId()).getServerList(), equalTo("http://localhost:9998"));
        assertThat(discoveryMapper.selectById(discoveryInAnotherNamespace.getId()).getServerList(), equalTo(discoveryInAnotherNamespace.getServerList()));

        // cleanup
        assertThat(discoveryMapper.delete(discoveryInNamespace.getId(), SYS_DEFAULT_NAMESPACE_ID), equalTo(1));
        assertThat(discoveryMapper.delete(discoveryInAnotherNamespace.getId(), ANOTHER_NAMESPACE_ID), equalTo(1));
    }

    private DiscoveryDO buildDiscoveryDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        return DiscoveryDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .discoveryName("discovery-" + UUIDUtils.getInstance().generateShortUuid())
                .discoveryType("local")
                .discoveryLevel("1")
                .serverList("http://localhost:8080")
                .pluginName("divide")
                .props("{}")
                .namespaceId(SYS_DEFAULT_NAMESPACE_ID)
                .dateCreated(now)
                .dateUpdated(now)
                .build();
    }
}
