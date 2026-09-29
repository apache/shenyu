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
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.NamespacePluginQuery;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

class NamespacePluginRelMapperTest extends AbstractSpringIntegrationTest {

    @Resource
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Resource
    private PluginMapper pluginMapper;

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

    @Test
    void testSelectByQueryWithStringNamespaceId() {
        String namespaceId = "namespace-text";
        NamespacePluginRelDO relation = NamespacePluginRelDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .pluginId("plugin")
                .namespaceId(namespaceId)
                .config("{}")
                .sort(0)
                .enabled(false)
                .dateCreated(new Timestamp(new java.util.Date().getTime()))
                .dateUpdated(new Timestamp(new java.util.Date().getTime()))
                .build();
        namespacePluginRelMapper.insertSelective(relation);

        NamespacePluginQuery query = new NamespacePluginQuery(null, null, new PageParameter(), namespaceId);
        List<NamespacePluginVO> results = namespacePluginRelMapper.selectByQuery(query);
        Assertions.assertFalse(results.isEmpty());
        Assertions.assertEquals(namespaceId, results.get(0).getNamespaceId());
    }

    @Test
    void testNameExistedExcludeUsesPluginName() {
        String pluginId = UUIDUtils.getInstance().generateShortUuid();
        String pluginName = "namespace-plugin-" + pluginId;
        pluginMapper.insert(PluginDO.builder()
                .id(pluginId)
                .name(pluginName)
                .role("proxy")
                .sort(0)
                .enabled(true)
                .dateCreated(new Timestamp(System.currentTimeMillis()))
                .dateUpdated(new Timestamp(System.currentTimeMillis()))
                .build());
        String relationId = UUIDUtils.getInstance().generateShortUuid();
        namespacePluginRelMapper.insertSelective(NamespacePluginRelDO.builder()
                .id(relationId)
                .pluginId(pluginId)
                .namespaceId(SYS_DEFAULT_NAMESPACE_ID)
                .enabled(true)
                .sort(0)
                .build());

        Assertions.assertTrue(namespacePluginRelMapper.nameExistedExclude(pluginName,
                Collections.singletonList("excluded-" + pluginId), SYS_DEFAULT_NAMESPACE_ID));
        Assertions.assertNull(namespacePluginRelMapper.nameExistedExclude(pluginName,
                Collections.singletonList(pluginId), SYS_DEFAULT_NAMESPACE_ID));
        Assertions.assertNull(namespacePluginRelMapper.nameExistedExclude("missing-" + pluginName,
                Collections.singletonList("excluded-" + pluginId), SYS_DEFAULT_NAMESPACE_ID));
    }
}
