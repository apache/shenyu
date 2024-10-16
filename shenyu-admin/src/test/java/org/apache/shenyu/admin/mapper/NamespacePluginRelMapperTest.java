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

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.model.entity.NamespacePluginRelDO;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;
import java.util.List;

class NamespacePluginRelMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Test
    void testSelectByIds() {
        String id = UUIDUtils.getInstance().generateShortUuid();
        NamespacePluginRelDO namespacePluginRelDO = NamespacePluginRelDO.builder()
                .id(id)
                .pluginId("plugin")
                .namespaceId("default")
                .config("{}")
                .sort(0)
                .enabled(false)
                .dateCreated(new Timestamp(new java.util.Date().getTime()))
                .dateUpdated(new Timestamp(new java.util.Date().getTime()))
                .build();

        namespacePluginRelMapper.insertSelective(namespacePluginRelDO);
        List<NamespacePluginVO> queryResults = namespacePluginRelMapper.selectByIds(List.of(id));
        Assertions.assertEquals(1, queryResults.size());
        Assertions.assertEquals(id, queryResults.get(0).getId());
    }
}