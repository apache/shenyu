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

package org.apache.shenyu.admin.service;

import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.mapper.AppAuthMapper;
import org.apache.shenyu.admin.mapper.AuthPathMapper;
import org.apache.shenyu.admin.mapper.DiscoveryMapper;
import org.apache.shenyu.admin.mapper.MetaDataMapper;
import org.apache.shenyu.admin.mapper.NamespaceMapper;
import org.apache.shenyu.admin.mapper.NamespacePluginRelMapper;
import org.apache.shenyu.admin.mapper.RuleMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.DiscoveryDO;
import org.apache.shenyu.admin.model.entity.NamespaceDO;
import org.apache.shenyu.admin.model.query.NamespaceQuery;
import org.apache.shenyu.admin.model.vo.NamespaceVO;
import org.apache.shenyu.admin.service.impl.NamespaceServiceImpl;
import org.apache.shenyu.admin.service.publish.NamespaceEventPublisher;
import org.apache.shenyu.admin.utils.SessionUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;

/**
 * Tests for namespace listing and deletion.
 */
@ExtendWith(MockitoExtension.class)
public final class NamespaceServiceImplTest {

    @Mock
    private NamespaceMapper namespaceMapper;

    @Mock
    private NamespaceUserService namespaceUserService;

    @Mock
    private NamespaceEventPublisher namespaceEventPublisher;

    @Mock
    private NamespacePluginRelMapper namespacePluginRelMapper;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private RuleMapper ruleMapper;

    @Mock
    private AuthPathMapper authPathMapper;

    @Mock
    private MetaDataMapper metaDataMapper;

    @Mock
    private DiscoveryMapper discoveryMapper;

    @Mock
    private AppAuthMapper appAuthMapper;

    @InjectMocks
    private NamespaceServiceImpl namespaceService;

    @Test
    public void testDeleteBlockedByDiscovery() {
        List<String> ids = List.of("namespace-primary-key");
        List<String> namespaceIds = List.of("namespace-id");
        when(namespaceMapper.selectByIds(ids)).thenReturn(List.of(NamespaceDO.builder().id(ids.get(0)).namespaceId(namespaceIds.get(0)).build()));
        when(discoveryMapper.selectAllByNamespaceIds(namespaceIds)).thenReturn(List.of(DiscoveryDO.builder().namespaceId(namespaceIds.get(0)).build()));

        ShenyuAdminException exception = assertThrows(ShenyuAdminException.class, () -> namespaceService.delete(ids));

        assertEquals("discovery exist under those namespace!", exception.getMessage());
        verify(namespaceMapper, never()).deleteByIds(anyList());
    }

    @Test
    public void testBatchDeleteBlockedByDiscoveryInOneNamespace() {
        List<String> ids = List.of("namespace-primary-key-1", "namespace-primary-key-2");
        List<String> namespaceIds = List.of("namespace-id-1", "namespace-id-2");
        when(namespaceMapper.selectByIds(ids)).thenReturn(List.of(
                NamespaceDO.builder().id(ids.get(0)).namespaceId(namespaceIds.get(0)).build(),
                NamespaceDO.builder().id(ids.get(1)).namespaceId(namespaceIds.get(1)).build()));
        when(discoveryMapper.selectAllByNamespaceIds(namespaceIds)).thenReturn(List.of(DiscoveryDO.builder().namespaceId(namespaceIds.get(1)).build()));

        assertThrows(ShenyuAdminException.class, () -> namespaceService.delete(ids));

        verify(namespaceMapper, never()).deleteByIds(anyList());
    }

    @Test
    public void testDeleteSucceedsWithoutDiscovery() {
        List<String> ids = List.of("namespace-primary-key");
        List<String> namespaceIds = List.of("namespace-id");
        when(namespaceMapper.selectByIds(ids)).thenReturn(List.of(NamespaceDO.builder().id(ids.get(0)).namespaceId(namespaceIds.get(0)).build()));
        when(discoveryMapper.selectAllByNamespaceIds(namespaceIds)).thenReturn(Collections.emptyList());

        assertEquals(ShenyuResultMessage.DELETE_SUCCESS, namespaceService.delete(ids));

        verify(discoveryMapper).selectAllByNamespaceIds(namespaceIds);
        verify(namespaceMapper).deleteByIds(ids);
    }

    @Test
    public void testAdminListFiltersByName() {
        NamespaceQuery query = new NamespaceQuery();
        query.setName("foo");

        NamespaceDO namespace = NamespaceDO.builder()
                .namespaceId("namespace-foo")
                .name("prod-foo")
                .build();

        try (MockedStatic<SessionUtil> sessionUtil = mockStatic(SessionUtil.class)) {
            sessionUtil.when(SessionUtil::isAdmin).thenReturn(true);
            when(namespaceMapper.selectByQuery(query)).thenReturn(List.of(namespace));

            List<NamespaceVO> result = namespaceService.list("foo");

            assertEquals(1, result.size());
            assertEquals("namespace-foo", result.get(0).getNamespaceId());
            assertEquals("prod-foo", result.get(0).getName());

            verify(namespaceMapper).selectByQuery(query);
            verify(namespaceMapper, never()).selectAll();
        }
    }

    @ParameterizedTest
    @NullAndEmptySource
    public void testAdminListWithoutNameReturnsAll(final String name) {
        NamespaceQuery query = new NamespaceQuery();
        query.setName(name);

        List<NamespaceDO> namespaces = List.of(
                NamespaceDO.builder().namespaceId("namespace-foo").name("foo").build(),
                NamespaceDO.builder().namespaceId("namespace-bar").name("bar").build());

        try (MockedStatic<SessionUtil> sessionUtil = mockStatic(SessionUtil.class)) {
            sessionUtil.when(SessionUtil::isAdmin).thenReturn(true);
            when(namespaceMapper.selectByQuery(query)).thenReturn(namespaces);

            List<NamespaceVO> result = namespaceService.list(name);

            assertEquals(List.of("foo", "bar"), result.stream().map(NamespaceVO::getName).toList());
            verify(namespaceMapper).selectByQuery(query);
        }
    }

    @Test
    public void testNonAdminListUsesAuthorizedNamespaces() {
        List<String> namespaceIds = List.of("namespace-foo");
        NamespaceDO namespace = NamespaceDO.builder()
                .namespaceId("namespace-foo")
                .name("prod-foo")
                .build();

        try (MockedStatic<SessionUtil> sessionUtil = mockStatic(SessionUtil.class)) {
            sessionUtil.when(SessionUtil::isAdmin).thenReturn(false);
            sessionUtil.when(SessionUtil::visitorId).thenReturn("user-1");
            when(namespaceUserService.listNamespaceIdByUserId("user-1")).thenReturn(namespaceIds);
            when(namespaceMapper.selectByNamespaceIdsAndName(namespaceIds, "foo"))
                    .thenReturn(List.of(namespace));

            List<NamespaceVO> result = namespaceService.list("foo");

            assertEquals(1, result.size());
            assertEquals("namespace-foo", result.get(0).getNamespaceId());
            verify(namespaceMapper).selectByNamespaceIdsAndName(namespaceIds, "foo");
            verify(namespaceMapper, never()).selectAll();
        }
    }

    @Test
    public void testNonAdminListWithoutPermissionsReturnsEmpty() {
        try (MockedStatic<SessionUtil> sessionUtil = mockStatic(SessionUtil.class)) {
            sessionUtil.when(SessionUtil::isAdmin).thenReturn(false);
            sessionUtil.when(SessionUtil::visitorId).thenReturn("user-1");
            when(namespaceUserService.listNamespaceIdByUserId("user-1"))
                    .thenReturn(Collections.emptyList());

            List<NamespaceVO> result = namespaceService.list("foo");

            assertTrue(result.isEmpty());
            verifyNoInteractions(namespaceMapper);
        }
    }
}
