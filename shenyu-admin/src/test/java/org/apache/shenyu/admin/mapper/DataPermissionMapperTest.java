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

import javax.annotation.Resource;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test cases for DataPermissionMapper.
 */

public class DataPermissionMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private DataPermissionMapper dataPermissionMapper;

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
