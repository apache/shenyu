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
import org.apache.shenyu.admin.model.entity.InstanceInfoDO;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class InstanceInfoRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private InstanceInfoRepository instanceInfoRepository;

    @Test
    void selectOneByQueryWithPartialConditions() {
        InstanceInfoDO target = buildInstanceInfo("10.0.0.1", "9195", "http", "namespaceA");
        instanceInfoRepository.save(target);
        instanceInfoRepository.save(buildInstanceInfo("10.0.0.2", "9196", "grpc", "namespaceA"));

        InstanceQuery query = new InstanceQuery();
        query.setInstanceIp("10.0.0.1");
        query.setInstancePort("9195");
        List<InstanceInfoDO> matched = instanceInfoRepository.selectOneByQuery(query);
        assertEquals(1, matched.size());
        assertEquals(target.getId(), matched.get(0).getId());
    }

    @Test
    void selectOneByQueryWithoutConditionsMatchesAll() {
        instanceInfoRepository.save(buildInstanceInfo("10.0.1.1", "9195", "http", "namespaceB"));
        assertTrue(instanceInfoRepository.selectOneByQuery(new InstanceQuery()).size() >= 1);
    }

    @Test
    void selectByQueryMandatoryNamespaceAndLikeIp() {
        instanceInfoRepository.save(buildInstanceInfo("192.168.1.10", "9195", "http", "namespaceC"));
        instanceInfoRepository.save(buildInstanceInfo("192.168.1.11", "9195", "http", "namespaceC"));
        instanceInfoRepository.save(buildInstanceInfo("10.0.0.1", "9195", "http", "namespaceD"));

        InstanceQuery query = new InstanceQuery();
        query.setNamespaceId("namespaceC");
        query.setInstanceIp("192.168.1");
        assertEquals(2, instanceInfoRepository.selectByQuery(query).size());

        query.setInstanceIp(null);
        query.setInstanceType("grpc");
        assertTrue(instanceInfoRepository.selectByQuery(query).isEmpty());
    }

    @Test
    void updateLoadedEntityPersistsChangedFields() {
        InstanceInfoDO saved = instanceInfoRepository.save(buildInstanceInfo("10.0.2.1", "9195", "http", "namespaceA"));
        InstanceInfoDO loaded = instanceInfoRepository.findById(saved.getId()).orElseThrow();
        loaded.setInstanceInfo("updated-info");
        loaded.setInstanceState(0);
        instanceInfoRepository.save(loaded);

        InstanceInfoDO reloaded = instanceInfoRepository.findById(saved.getId()).orElseThrow();
        assertEquals("updated-info", reloaded.getInstanceInfo());
        assertEquals(0, reloaded.getInstanceState());
        assertNotNull(reloaded.getDateCreated());
    }

    private InstanceInfoDO buildInstanceInfo(final String ip, final String port, final String type, final String namespaceId) {
        InstanceInfoDO instanceInfoDO = new InstanceInfoDO();
        instanceInfoDO.setId(UUIDUtils.getInstance().generateShortUuid());
        instanceInfoDO.setInstanceIp(ip);
        instanceInfoDO.setInstancePort(port);
        instanceInfoDO.setInstanceType(type);
        instanceInfoDO.setInstanceInfo("info");
        instanceInfoDO.setInstanceState(1);
        instanceInfoDO.setNamespaceId(namespaceId);
        return instanceInfoDO;
    }
}
