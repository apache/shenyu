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
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.assertj.core.util.Lists;
import org.junit.jupiter.api.Test;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.TransactionTemplate;

import jakarta.annotation.Resource;
import java.util.List;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * Test cases for DataPermissionMapper.
 */

public class DataPermissionMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private DataPermissionMapper dataPermissionMapper;

    @Resource
    private PlatformTransactionManager transactionManager;

    @Test
    public void testDuplicateGrantIsRejected() {
        DataPermissionDO permission = buildDataPermissionDO();
        dataPermissionMapper.insertSelective(permission);
        DataPermissionDO duplicate = DataPermissionDO.buildCreatePermissionDO(permission.getDataId(), permission.getUserId(), permission.getDataType());

        assertThrows(DuplicateKeyException.class, () -> dataPermissionMapper.insertSelective(duplicate));
        assertEquals(permission.getId(), dataPermissionMapper.findOneByUniqueKey(permission.getDataId(), permission.getUserId(), permission.getDataType()).getId());
    }

    @Test
    public void testDuplicateBatchDoesNotInsertOtherGrants() {
        DataPermissionDO permission = buildDataPermissionDO();
        dataPermissionMapper.insertSelective(permission);
        DataPermissionDO duplicate = DataPermissionDO.buildCreatePermissionDO(permission.getDataId(), permission.getUserId(), permission.getDataType());
        DataPermissionDO other = DataPermissionDO.buildCreatePermissionDO(UUIDUtils.getInstance().generateShortUuid(), permission.getUserId(), permission.getDataType());

        assertThrows(DuplicateKeyException.class, () -> dataPermissionMapper.insertBatch(List.of(other, duplicate)));
        assertEquals(1, dataPermissionMapper.listByUserId(permission.getUserId()).size());
    }

    @Test
    public void testDistinctGrantKeysArePreserved() {
        DataPermissionDO permission = buildDataPermissionDO();
        List<DataPermissionDO> permissions = List.of(permission,
                DataPermissionDO.buildCreatePermissionDO(permission.getDataId(), UUIDUtils.getInstance().generateShortUuid(), permission.getDataType()),
                DataPermissionDO.buildCreatePermissionDO(UUIDUtils.getInstance().generateShortUuid(), permission.getUserId(), permission.getDataType()),
                DataPermissionDO.buildCreatePermissionDO(permission.getDataId(), permission.getUserId(), permission.getDataType() + 1));

        assertEquals(4, dataPermissionMapper.insertBatch(permissions));
        assertEquals(3, dataPermissionMapper.listByUserId(permission.getUserId()).size());
    }

    @Test
    public void testConcurrentGrantCommitsOnlyOnce() throws Exception {
        DataPermissionDO permission = buildDataPermissionDO();
        DataPermissionDO duplicate = DataPermissionDO.buildCreatePermissionDO(permission.getDataId(), permission.getUserId(), permission.getDataType());
        CyclicBarrier barrier = new CyclicBarrier(2);
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            Future<Boolean> first = executor.submit(() -> insertConcurrentGrant(permission, barrier));
            Future<Boolean> second = executor.submit(() -> insertConcurrentGrant(duplicate, barrier));
            int committed = (first.get(20, TimeUnit.SECONDS) ? 1 : 0) + (second.get(20, TimeUnit.SECONDS) ? 1 : 0);
            assertEquals(1, committed);
            assertEquals(1, dataPermissionMapper.listByUserId(permission.getUserId()).size());
        } finally {
            executor.shutdownNow();
        }
    }

    private boolean insertConcurrentGrant(final DataPermissionDO permission, final CyclicBarrier barrier) {
        try {
            new TransactionTemplate(transactionManager).executeWithoutResult(status -> {
                assertEquals(List.of(), dataPermissionMapper.selectUserIds(permission.getDataId(), permission.getDataType()));
                assertDoesNotThrow(() -> barrier.await(10, TimeUnit.SECONDS));
                dataPermissionMapper.insertSelective(permission);
            });
            return true;
        } catch (DuplicateKeyException e) {
            return false;
        }
    }

    @Test
    public void insertBatch() {

        DataPermissionDO dataPermissionDO1 = buildDataPermissionDO();
        DataPermissionDO dataPermissionDO2 = buildDataPermissionDO();
        DataPermissionDO dataPermissionDO3 = buildDataPermissionDO();
        List<DataPermissionDO> dataList = Lists.list(dataPermissionDO1, dataPermissionDO2, dataPermissionDO3);
        int ret = dataPermissionMapper.insertBatch(dataList);
        assertEquals(dataList.size(), ret);

    }

    private DataPermissionDO buildDataPermissionDO() {

        String id = UUIDUtils.getInstance().generateShortUuid();
        String dataId = UUIDUtils.getInstance().generateShortUuid();
        String userId = UUIDUtils.getInstance().generateShortUuid();
        int dataType = 2;

        return DataPermissionDO.builder()
                .dataId(dataId)
                .userId(userId)
                .id(id)
                .dataType(dataType).build();
    }

    @Test
    public void deleteByDataIdList() {

        DataPermissionDO dataPermissionDO1 = buildDataPermissionDO();
        DataPermissionDO dataPermissionDO2 = buildDataPermissionDO();
        DataPermissionDO dataPermissionDO3 = buildDataPermissionDO();
        List<DataPermissionDO> dataList = Lists.list(dataPermissionDO1, dataPermissionDO2, dataPermissionDO3);
        int ret1 = dataPermissionMapper.insertBatch(dataList);
        assertEquals(dataList.size(), ret1);

        List<String> dataIdList = Lists.list(dataPermissionDO1.getDataId(), dataPermissionDO2.getDataId(), dataPermissionDO3.getDataId());
        int ret2 = dataPermissionMapper.deleteByDataIdList(dataIdList);
        assertEquals(dataList.size(), ret2);
    }

}
