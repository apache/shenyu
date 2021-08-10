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
import org.apache.shenyu.admin.model.entity.AuthParamDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.Test;
import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import static org.hamcrest.Matchers.comparesEqualTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.Assert.assertThat;

/**
 * Test cases for AuthParamMapper.
 */
public final class AuthParamMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private AuthParamMapper authParamMapper;

    @Test
    public void testInsertAndUpdate() {
        AuthParamDO record = buildAuthParamDO();
        int count = authParamMapper.save(record);
        assertThat(count, comparesEqualTo(1));

        record.setAppParam("change_app_param");
        count = authParamMapper.update(record);
        assertThat(count, comparesEqualTo(1));

        String authId = record.getAuthId();
        count = authParamMapper.deleteByAuthId(authId);
        assertThat(count, comparesEqualTo(1));
    }

    @Test
    public void testBatchSave() {
        List<AuthParamDO> authParamDOList = new ArrayList<>();
        AuthParamDO record = buildAuthParamDO();
        AuthParamDO record2 = buildAuthParamDO();
        authParamDOList.add(record);
        authParamDOList.add(record2);
        int count = authParamMapper.batchSave(authParamDOList);
        assertThat(count, comparesEqualTo(2));

        for (AuthParamDO each : authParamDOList) {
            String authId = each.getAuthId();
            count = authParamMapper.deleteByAuthId(authId);
            assertThat(count, comparesEqualTo(1));
        }
    }

    @Test
    public void testFindByAuthId() {
        AuthParamDO record = buildAuthParamDO();
        int count = authParamMapper.save(record);
        assertThat(count, comparesEqualTo(1));

        String authId = record.getAuthId();
        List<AuthParamDO> authParamDOs = authParamMapper.findByAuthId(authId);
        assertThat(authParamDOs.size(), comparesEqualTo(1));

        count = authParamMapper.deleteByAuthId(authId);
        assertThat(count, comparesEqualTo(1));
    }

    @Test
    public void testFindByAuthIdAndAppName() {
        AuthParamDO record = buildAuthParamDO();
        int count = authParamMapper.save(record);
        assertThat(count, comparesEqualTo(1));

        String authId = record.getAuthId();
        String authName = record.getAppName();
        AuthParamDO authParamDOs = authParamMapper.findByAuthIdAndAppName(authId, authName);
        assertThat(authParamDOs, notNullValue(AuthParamDO.class));

        count = authParamMapper.deleteByAuthId(authId);
        assertThat(count, comparesEqualTo(1));
    }

    /**
     * Build auth param do.
     * @return the auth param do
     */
    private AuthParamDO buildAuthParamDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        String authId = UUIDUtils.getInstance().generateShortUuid();
        return AuthParamDO.builder()
                .id(id)
                .authId(authId)
                .appName("test_app_name")
                .appParam("test_app_param")
                .dateUpdated(now)
                .dateCreated(now)
                .build();
    }
}
