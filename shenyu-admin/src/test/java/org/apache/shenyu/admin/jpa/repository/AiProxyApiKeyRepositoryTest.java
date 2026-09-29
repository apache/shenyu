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
import org.apache.shenyu.admin.model.entity.ProxyApiKeyDO;
import org.apache.shenyu.admin.model.query.ProxyApiKeyQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Arrays;
import java.util.List;

class AiProxyApiKeyRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private AiProxyApiKeyRepository aiProxyApiKeyRepository;

    @Test
    @Transactional
    void updateEnableBatch() {
        ProxyApiKeyDO entity1 = buildProxyApiKeyDO();
        ProxyApiKeyDO entity2 = buildProxyApiKeyDO();
        ProxyApiKeyDO entity3 = buildProxyApiKeyDO();
        entity3.setEnabled(false);

        ProxyApiKeyDO saved1 = aiProxyApiKeyRepository.save(entity1);
        ProxyApiKeyDO saved2 = aiProxyApiKeyRepository.save(entity2);
        ProxyApiKeyDO saved3 = aiProxyApiKeyRepository.save(entity3);

        List<String> idsToUpdate = Arrays.asList(saved1.getId(), saved2.getId(), saved3.getId());
        int updatedCount = aiProxyApiKeyRepository.updateEnableBatch(idsToUpdate, false);

        Assertions.assertEquals(3, updatedCount);

        ProxyApiKeyDO updated1 = aiProxyApiKeyRepository.findById(saved1.getId()).orElse(null);
        ProxyApiKeyDO updated2 = aiProxyApiKeyRepository.findById(saved2.getId()).orElse(null);
        ProxyApiKeyDO updated3 = aiProxyApiKeyRepository.findById(saved3.getId()).orElse(null);

        Assertions.assertNotNull(updated1);
        Assertions.assertFalse(updated1.getEnabled());
        Assertions.assertNotNull(updated2);
        Assertions.assertFalse(updated2.getEnabled());
        Assertions.assertNotNull(updated3);
        Assertions.assertFalse(updated3.getEnabled());
    }

    @Test
    @Transactional
    void pageByConditionWithAllConditions() {
        for (int i = 0; i < 5; i++) {
            ProxyApiKeyDO entity = buildProxyApiKeyDO();
            entity.setProxyApiKey("test-key-" + i);
            entity.setEnabled(true);
            entity.setNamespaceId("namespaceA");
            entity.setSelectorId("selector-1");
            aiProxyApiKeyRepository.save(entity);
        }

        for (int i = 0; i < 3; i++) {
            ProxyApiKeyDO entity = buildProxyApiKeyDO();
            entity.setProxyApiKey("other-key-" + i);
            entity.setEnabled(false);
            aiProxyApiKeyRepository.save(entity);
        }

        ProxyApiKeyQuery query = new ProxyApiKeyQuery();
        query.setProxyApiKey("test-key-0");
        query.setEnabled(true);
        query.setNamespaceId("namespaceA");
        query.setSelectorId("selector-1");

        Page<ProxyApiKeyDO> page = aiProxyApiKeyRepository.pageByCondition(query, PageRequest.of(0, 10));

        Assertions.assertEquals(1, page.getTotalElements());
        Assertions.assertEquals("test-key-0", page.getContent().get(0).getProxyApiKey());
    }

    @Test
    @Transactional
    void pageByConditionWithNullConditions() {
        for (int i = 0; i < 5; i++) {
            aiProxyApiKeyRepository.save(buildProxyApiKeyDO());
        }

        ProxyApiKeyQuery query = new ProxyApiKeyQuery();

        Page<ProxyApiKeyDO> page = aiProxyApiKeyRepository.pageByCondition(query, PageRequest.of(0, 10));

        Assertions.assertEquals(5, page.getTotalElements());
    }

    @Test
    @Transactional
    void pageByConditionWithEmptyStringConditions() {
        for (int i = 0; i < 3; i++) {
            ProxyApiKeyDO entity = buildProxyApiKeyDO();
            entity.setEnabled(true);
            aiProxyApiKeyRepository.save(entity);
        }

        ProxyApiKeyQuery query = new ProxyApiKeyQuery();
        query.setProxyApiKey("");
        query.setNamespaceId("");
        query.setSelectorId("");
        query.setEnabled(true);

        Page<ProxyApiKeyDO> page = aiProxyApiKeyRepository.pageByCondition(query, PageRequest.of(0, 10));

        Assertions.assertEquals(3, page.getTotalElements());
    }

    @Test
    @Transactional
    void pageByConditionWithPagination() {
        int totalCount = 15;
        for (int i = 0; i < totalCount; i++) {
            aiProxyApiKeyRepository.save(buildProxyApiKeyDO());
        }

        ProxyApiKeyQuery query = new ProxyApiKeyQuery();
        int pageSize = 5;

        Page<ProxyApiKeyDO> firstPage = aiProxyApiKeyRepository.pageByCondition(query, PageRequest.of(0, pageSize));
        Assertions.assertEquals(totalCount, firstPage.getTotalElements());
        Assertions.assertEquals(pageSize, firstPage.getContent().size());

        Page<ProxyApiKeyDO> secondPage = aiProxyApiKeyRepository.pageByCondition(query, PageRequest.of(1, pageSize));
        Assertions.assertEquals(totalCount, secondPage.getTotalElements());
        Assertions.assertEquals(pageSize, secondPage.getContent().size());

        Page<ProxyApiKeyDO> thirdPage = aiProxyApiKeyRepository.pageByCondition(query, PageRequest.of(2, pageSize));
        Assertions.assertEquals(totalCount, thirdPage.getTotalElements());
        Assertions.assertEquals(pageSize, thirdPage.getContent().size());
    }

    @Test
    @Transactional
    void pageByConditionWithOrderByDateUpdated() {
        ProxyApiKeyDO entity1 = buildProxyApiKeyDO();
        aiProxyApiKeyRepository.save(entity1);

        try {
            Thread.sleep(100);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }

        ProxyApiKeyDO entity2 = buildProxyApiKeyDO();
        aiProxyApiKeyRepository.save(entity2);

        ProxyApiKeyQuery query = new ProxyApiKeyQuery();
        Page<ProxyApiKeyDO> page = aiProxyApiKeyRepository.pageByCondition(query, PageRequest.of(0, 10));

        Assertions.assertEquals(2, page.getTotalElements());
        Assertions.assertTrue(page.getContent().get(0).getDateUpdated().after(page.getContent().get(1).getDateUpdated()));
    }

    @Test
    @Transactional
    void existsBySelectorIdAndProxyApiKey() {
        String selectorId = "selector-test";
        String proxyApiKey = "proxy-key-test";

        ProxyApiKeyDO entity = buildProxyApiKeyDO();
        entity.setSelectorId(selectorId);
        entity.setProxyApiKey(proxyApiKey);
        aiProxyApiKeyRepository.save(entity);

        boolean exists = aiProxyApiKeyRepository.existsBySelectorIdAndProxyApiKey(selectorId, proxyApiKey);
        Assertions.assertTrue(exists);

        boolean notExists = aiProxyApiKeyRepository.existsBySelectorIdAndProxyApiKey(selectorId, "non-existent-key");
        Assertions.assertFalse(notExists);
    }

    private ProxyApiKeyDO buildProxyApiKeyDO() {
        ProxyApiKeyDO entity = new ProxyApiKeyDO();
        entity.setId(UUIDUtils.getInstance().generateShortUuid());
        entity.setProxyApiKey(UUIDUtils.getInstance().generateShortUuid());
        entity.setEnabled(true);
        entity.setNamespaceId("default-namespace");
        entity.setSelectorId("default-selector");
        entity.setDescription("test description");
        entity.setDateCreated(Timestamp.valueOf(LocalDateTime.now()));
        entity.setDateUpdated(Timestamp.valueOf(LocalDateTime.now()));
        return entity;
    }
}