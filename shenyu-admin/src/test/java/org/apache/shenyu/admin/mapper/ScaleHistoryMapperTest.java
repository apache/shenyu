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
import org.apache.shenyu.admin.model.entity.ScaleHistoryDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Test;

import jakarta.annotation.Resource;
import java.sql.Timestamp;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * Test cases for ScaleHistoryMapper.
 */
public final class ScaleHistoryMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private ScaleHistoryMapper scaleHistoryMapper;

    @Test
    public void insertPreservesManuallyAssignedId() {
        String id = UUIDUtils.getInstance().generateShortUuid();
        Timestamp now = new Timestamp(System.currentTimeMillis());
        ScaleHistoryDO record = new ScaleHistoryDO(1, 2, 1, "scale");
        record.setId(id);
        record.setDateCreated(now);
        record.setDateUpdated(now);

        assertEquals(1, scaleHistoryMapper.insert(record));
        assertEquals(id, scaleHistoryMapper.selectByPrimaryKey(id).getId());
        assertEquals(1, scaleHistoryMapper.deleteByPrimaryKey(id));
    }

    @Test
    public void insertSelectiveIncludesIdAndUsesDatabaseDefaults() {
        String id = UUIDUtils.getInstance().generateShortUuid();
        ScaleHistoryDO record = new ScaleHistoryDO(1, 1, 0, null);
        record.setId(id);

        assertEquals(1, scaleHistoryMapper.insertSelective(record));
        ScaleHistoryDO stored = scaleHistoryMapper.selectByPrimaryKey(id);
        assertEquals(id, stored.getId());
        assertNull(stored.getMsg());
        assertEquals(1, scaleHistoryMapper.deleteByPrimaryKey(id));
    }
}
