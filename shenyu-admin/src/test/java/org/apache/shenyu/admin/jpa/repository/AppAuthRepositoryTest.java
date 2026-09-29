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
import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.entity.AuthParamDO;
import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.admin.model.query.AppAuthQuery;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.data.domain.PageRequest;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

class AppAuthRepositoryTest extends AbstractSpringIntegrationTest {

    @Resource
    private AppAuthRepository appAuthRepository;

    @Resource
    private AuthParamRepository authParamRepository;

    @Resource
    private AuthPathRepository authPathRepository;

    @Test
    @Transactional
    void saveAndFindByKeys() {
        AppAuthDO appAuthDO = buildAppAuthDO("6138-find");
        AppAuthDO saved = appAuthRepository.save(appAuthDO);

        assertNotNull(saved.getId());
        AppAuthDO byAppKey = appAuthRepository.findByAppKey(appAuthDO.getAppKey()).orElse(null);
        assertNotNull(byAppKey);
        assertEquals(appAuthDO.getNamespaceId(), byAppKey.getNamespaceId());

        AppAuthDO byDualKey = appAuthRepository.findByIdAndNamespaceId(saved.getId(), appAuthDO.getNamespaceId()).orElse(null);
        assertNotNull(byDualKey);
        assertEquals(appAuthDO.getAppKey(), byDualKey.getAppKey());
        assertFalse(appAuthRepository.findByIdAndNamespaceId(saved.getId(), "wrong-namespace").isPresent());

        List<AppAuthDO> byNamespaceIds = appAuthRepository.findByNamespaceIdIn(Arrays.asList(appAuthDO.getNamespaceId(), "absent-namespace"));
        assertEquals(1, byNamespaceIds.size());
        assertEquals(appAuthDO.getAppKey(), byNamespaceIds.get(0).getAppKey());
    }

    @Test
    @Transactional
    void pageByQueryAppliesOnlyNonNullConditions() {
        AppAuthDO first = buildAppAuthDO("6138-query-a");
        AppAuthDO second = buildAppAuthDO("6138-query-b");
        second.setPhone("13800000000");
        appAuthRepository.save(first);
        appAuthRepository.save(second);

        AppAuthQuery query = new AppAuthQuery();
        query.setNamespaceId(first.getNamespaceId());
        assertEquals(2, appAuthRepository.pageByQuery(query, PageRequest.of(0, 10)).getTotalElements());

        query.setAppKey(first.getAppKey());
        assertEquals(1, appAuthRepository.pageByQuery(query, PageRequest.of(0, 10)).getTotalElements());

        query.setPhone(second.getPhone());
        assertEquals(0, appAuthRepository.pageByQuery(query, PageRequest.of(0, 10)).getTotalElements());

        query.setAppKey(null);
        assertEquals(1, appAuthRepository.pageByQuery(query, PageRequest.of(0, 10)).getTotalElements());
    }

    @Test
    @Transactional
    void updateAppSecretByAppKeyUpdatesMatchingRowsOnly() {
        AppAuthDO first = appAuthRepository.save(buildAppAuthDO("6138-sk-a"));
        AppAuthDO second = appAuthRepository.save(buildAppAuthDO("6138-sk-b"));

        int updated = appAuthRepository.updateAppSecretByAppKey(first.getAppKey(), "new-secret");

        assertEquals(1, updated);
        assertEquals("new-secret", appAuthRepository.findById(first.getId()).orElseThrow().getAppSecret());
        Assertions.assertNotEquals("new-secret", appAuthRepository.findById(second.getId()).orElseThrow().getAppSecret());
    }

    @Test
    @Transactional
    void updateEnableAndOpenBatchFlipAllMatchedRows() {
        AppAuthDO first = appAuthRepository.save(buildAppAuthDO("6138-batch-a"));
        AppAuthDO second = appAuthRepository.save(buildAppAuthDO("6138-batch-b"));
        List<String> ids = Arrays.asList(first.getId(), second.getId());

        appAuthRepository.updateEnableBatch(ids, false);
        appAuthRepository.updateOpenBatch(ids, false);

        for (String id : ids) {
            AppAuthDO reloaded = appAuthRepository.findById(id).orElse(null);
            assertNotNull(reloaded);
            assertFalse(reloaded.getEnabled());
            assertFalse(reloaded.getOpen());
        }
    }

    @Test
    @Transactional
    void deleteByIdsCascadesToParamsAndPaths() {
        AppAuthDO first = appAuthRepository.save(buildAppAuthDO("6138-del-a"));
        AppAuthDO second = appAuthRepository.save(buildAppAuthDO("6138-del-b"));
        authParamRepository.save(AuthParamDO.create(first.getId(), "app-one", "{\"app\":\"app-one\"}"));
        authPathRepository.save(AuthPathDO.create("/http/test/**", first.getId(), "app-one"));
        authPathRepository.save(AuthPathDO.create("/http/other/**", second.getId(), "app-two"));

        int deleted = appAuthRepository.deleteByIds(Arrays.asList(first.getId(), second.getId()));
        assertEquals(2, deleted);

        authParamRepository.deleteByAuthIds(Arrays.asList(first.getId(), second.getId()));
        authPathRepository.deleteByAuthIds(Arrays.asList(first.getId(), second.getId()));

        assertEquals(0, authParamRepository.findByAuthIdIn(Arrays.asList(first.getId(), second.getId())).size());
        assertEquals(0, authPathRepository.findByAuthId(first.getId()).size());
        assertFalse(appAuthRepository.findById(first.getId()).isPresent());
    }

    private AppAuthDO buildAppAuthDO(final String appKey) {
        AppAuthDO appAuthDO = AppAuthDO.builder()
                .appKey(appKey)
                .appSecret("secret-" + appKey)
                .open(true)
                .enabled(true)
                .namespaceId("namespace-6138")
                .build();
        appAuthDO.setId(UUIDUtils.getInstance().generateShortUuid());
        return appAuthDO;
    }
}
