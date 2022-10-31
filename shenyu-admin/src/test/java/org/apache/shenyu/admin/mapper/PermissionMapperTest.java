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
import org.apache.shenyu.admin.model.entity.PermissionDO;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;

/**
 * test for {@linkplain PermissionMapper}.
 */
public class PermissionMapperTest extends AbstractSpringIntegrationTest {

    @Autowired
    private PermissionMapper permissionMapper;

    @Test
    public void testInsertBatch() {

        List<PermissionDO> permissionDOS = new ArrayList<>();
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1346775491550474240").id(UUID.randomUUID().toString()).build());
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1346776175553376256").id(UUID.randomUUID().toString()).build());
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1346777157943259136").id(UUID.randomUUID().toString()).build());
        permissionDOS.add(PermissionDO.builder().objectId("1346358560427216896").resourceId("1347053375029653504").id(UUID.randomUUID().toString()).build());

        assertThat(permissionMapper.insertBatch(permissionDOS), equalTo(permissionDOS.size()));
    }
}
