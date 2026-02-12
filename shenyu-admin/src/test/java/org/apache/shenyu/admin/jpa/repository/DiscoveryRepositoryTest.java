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

import com.alibaba.nacos.api.common.Constants;
import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.time.LocalDateTime;

class DiscoveryRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private DiscoveryRepository discoveryRepository;

    private final DiscoveryDO discoveryDO = buildDefaultNamespaceDiscoveryDO(UUIDUtils.getInstance().generateShortUuid());

    @Test
    @Transactional
    void updateSelective() {
        discoveryRepository.save(discoveryDO);
        DiscoveryDO update = new DiscoveryDO();
        update.setId(discoveryDO.getId());
        update.setDiscoveryLevel("level1");
        int updateCount = discoveryRepository.updateSelective(update);
        Assertions.assertEquals(1, updateCount);
    }

    private DiscoveryDO buildDefaultNamespaceDiscoveryDO(final String id) {
        DiscoveryDO discoveryDO = new DiscoveryDO();
        discoveryDO.setId(id);
        discoveryDO.setNamespaceId(Constants.DEFAULT_NAMESPACE_ID);
        discoveryDO.setDiscoveryName("name");
        discoveryDO.setDiscoveryType("type");
        discoveryDO.setDiscoveryLevel("level");
        discoveryDO.setServerList("serverList");
        discoveryDO.setPluginName("pluginName");
        discoveryDO.setProps("props");
        discoveryDO.setDateCreated(Timestamp.valueOf(LocalDateTime.now()));
        discoveryDO.setDateUpdated(Timestamp.valueOf(LocalDateTime.now()));
        return discoveryDO;
    }
}