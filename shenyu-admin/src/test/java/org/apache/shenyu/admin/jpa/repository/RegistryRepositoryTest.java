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
import org.apache.shenyu.admin.model.entity.RegistryDO;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.RegistryQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Page;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

class RegistryRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private RegistryRepository registryRepository;

    @Test
    void findByRegistryIdReturnsMatchedRow() {
        RegistryDO registryDO = registryRepository.save(buildRegistry("reg-100", "127.0.0.1:8848", "default-ns"));
        assertTrue(registryRepository.findByRegistryId("reg-100").isPresent());
        assertTrue(registryRepository.findByRegistryId("reg-none").isEmpty());
        assertEquals(registryDO.getId(), registryRepository.findByRegistryId("reg-100").orElseThrow().getId());
    }

    @Test
    void selectByQueryWithLikeAndEqConditions() {
        registryRepository.save(buildRegistry("reg-a", "127.0.0.1:8848", "ns-1"));
        registryRepository.save(buildRegistry("reg-b", "127.0.0.2:8848", "ns-1"));
        registryRepository.save(buildRegistry("reg-c", "10.0.0.1:2379", "ns-2"));

        RegistryQuery query = new RegistryQuery();
        query.setRegistryId("reg-");
        query.setNamespace("ns-1");
        Page<RegistryDO> page = registryRepository.selectByQuery(query, PageResultUtils.of(new PageParameter(1, 10)));
        assertEquals(2, page.getTotalElements());

        query.setAddress("2379");
        page = registryRepository.selectByQuery(query, PageResultUtils.of(new PageParameter(1, 10)));
        assertTrue(page.isEmpty());
    }

    @Test
    void updateLoadedEntityKeepsDateCreated() {
        RegistryDO saved = registryRepository.save(buildRegistry("reg-u", "127.0.0.1:8848", "default-ns"));
        RegistryDO loaded = registryRepository.findById(saved.getId()).orElseThrow();
        loaded.setAddress("127.0.0.9:9999");
        registryRepository.save(loaded);

        RegistryDO reloaded = registryRepository.findById(saved.getId()).orElseThrow();
        assertEquals("127.0.0.9:9999", reloaded.getAddress());
        assertNotNull(reloaded.getDateCreated());
    }

    private RegistryDO buildRegistry(final String registryId, final String address, final String namespace) {
        RegistryDO registryDO = new RegistryDO();
        registryDO.setId(UUIDUtils.getInstance().generateShortUuid());
        registryDO.setRegistryId(registryId);
        registryDO.setAddress(address);
        registryDO.setNamespace(namespace);
        registryDO.setProtocol("zookeeper");
        registryDO.setRegistryGroup("shenyu");
        return registryDO;
    }
}
