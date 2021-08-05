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
import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.Test;
import javax.annotation.Resource;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

/**
 * Test cases for AuthPathMapper.
 */
public final class AuthPathMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private AuthPathMapper authPathMapper;

    @Test
    public void testSaveAndUpdate() {
        AuthPathDO authPathDO = buildAuthPathDO();
        int count = authPathMapper.save(authPathDO);
        assertThat(count, greaterThan(0));

        authPathDO.setPath("test_path01");
        count = authPathMapper.update(authPathDO);
        assertThat(count, greaterThan(0));
    }

    @Test
    public void testBatchSave() {
        AuthPathDO authPathDO = buildAuthPathDO();
        List<AuthPathDO> authPathDOList = new ArrayList<>();
        authPathDOList.add(authPathDO);
        int count = authPathMapper.batchSave(authPathDOList);
        assertThat(count, is(1));
    }

    @Test
    public void testFindByAuthId() {
        AuthPathDO authPathDO = buildAuthPathDO();
        int count = authPathMapper.save(authPathDO);
        assertThat(count, greaterThan(0));

        List<AuthPathDO> authPathDOList = authPathMapper.findByAuthId(authPathDO.getAuthId());
        assertThat(authPathDOList.size(), is(1));
    }

    @Test
    public void testFindByAuthIdAndAppName() {
        AuthPathDO authPathDO = buildAuthPathDO();
        int count = authPathMapper.save(authPathDO);
        assertThat(count, greaterThan(0));

        List<AuthPathDO> authPathDOList = authPathMapper.findByAuthIdAndAppName(authPathDO.getAuthId(), authPathDO.getAppName());
        assertThat(authPathDOList.size(), is(1));
    }

    @Test
    public void testDeleteByAuthIdAndAppName() {
        AuthPathDO authPathDO = buildAuthPathDO();
        int count = authPathMapper.save(authPathDO);
        assertThat(count, greaterThan(0));

        count = authPathMapper.deleteByAuthIdAndAppName(authPathDO.getAuthId(), authPathDO.getAppName());
        assertThat(count, greaterThan(0));
    }

    @Test
    public void deleteByAuthId() {
        AuthPathDO authPathDO = buildAuthPathDO();
        int count = authPathMapper.save(authPathDO);
        assertThat(count, greaterThan(0));

        count = authPathMapper.deleteByAuthId(authPathDO.getAuthId());
        assertThat(count, greaterThan(0));
    }

    private AuthPathDO buildAuthPathDO() {
        Timestamp now = new Timestamp(System.currentTimeMillis());
        String id = UUIDUtils.getInstance().generateShortUuid();
        String authId = UUIDUtils.getInstance().generateShortUuid();
        return AuthPathDO.builder()
                .id(id)
                .authId(authId)
                .appName("test_app")
                .path("test_path")
                .enabled(false)
                .dateUpdated(now)
                .dateCreated(now)
                .build();
    }
}
