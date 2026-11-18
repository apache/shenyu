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

import jakarta.annotation.Resource;
import org.apache.shenyu.admin.AbstractSpringIntegrationTest;
import org.apache.shenyu.admin.exception.ValidFailException;
import org.apache.shenyu.admin.mapper.PermissionMapper;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.ResourceMapper;
import org.apache.shenyu.admin.model.dto.PluginDTO;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.utils.ResourceUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mybatis.spring.SqlSessionTemplate;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.jdbc.core.JdbcTemplate;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.verify;

/**
 * Exercise manual plugin resource repair through the real service proxies and H2 transactions.
 */
public final class PluginResourceIdempotenceTest extends AbstractSpringIntegrationTest {

    private static final String PLUGIN_ID = "resource-repair-plugin";

    private static final String PLUGIN_NAME = "resource-repair";

    @Resource
    private PluginService pluginService;

    @Resource
    private ResourceService resourceService;

    @Resource
    private PluginMapper pluginMapper;

    @Resource
    private JdbcTemplate jdbcTemplate;

    @Resource
    private SqlSessionTemplate sqlSessionTemplate;

    @SpyBean
    private ResourceMapper resourceMapper;

    @SpyBean
    private PermissionMapper permissionMapper;

    @BeforeEach
    public void setUp() {
        PluginDTO dto = request();
        pluginMapper.insertSelective(PluginDO.buildPluginDO(dto));
    }

    @AfterEach
    public void cleanup() {
        reset(resourceMapper, permissionMapper);
        jdbcTemplate.update("DELETE FROM permission WHERE resource_id IN (SELECT id FROM resource WHERE name = ? OR perms LIKE ?)", PLUGIN_NAME, "plugin:" + PLUGIN_NAME + "%");
        jdbcTemplate.update("DELETE FROM resource WHERE name = ? OR perms LIKE ?", PLUGIN_NAME, "plugin:" + PLUGIN_NAME + "%");
        jdbcTemplate.update("DELETE FROM permission WHERE resource_id IN (SELECT id FROM resource WHERE name = ? OR perms LIKE ?)", "untrusted-request-name", "plugin:untrusted-request-name%");
        jdbcTemplate.update("DELETE FROM resource WHERE name = ? OR perms LIKE ?", "untrusted-request-name", "plugin:untrusted-request-name%");
        jdbcTemplate.update("DELETE FROM plugin WHERE name = ?", PLUGIN_NAME);
    }

    @Test
    public void completeResourcesCanBeGeneratedRepeatedlyWithoutWrites() {
        ResourceDO root = seedRoot();
        resourceService.createResourceBatch(ResourceUtil.buildPluginDataPermissionResource(root.getId(), PLUGIN_NAME));
        final List<Map<String, Object>> resources = resources();
        final List<Map<String, Object>> permissions = permissions();
        reset(resourceMapper);

        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, pluginService.createPluginResource(request()));
        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, pluginService.createPluginResource(request()));

        assertEquals(resources, resources());
        assertEquals(permissions, permissions());
        verify(resourceMapper, never()).insertSelective(any());
        verify(resourceMapper, never()).insertBatch(anyList());
    }

    @Test
    public void missingButtonsAreAddedWithoutChangingDisabledResourcesOrRoleBindings() {
        ResourceDO root = seedRoot();
        jdbcTemplate.update("UPDATE resource SET title = ?, status = 0 WHERE id = ?", "Custom plugin label", root.getId());
        ResourceDO button = ResourceUtil.buildPluginDataPermissionResource(root.getId(), PLUGIN_NAME).get(0);
        button.setTitle("Custom button label");
        button.setStatus(0);
        resourceService.createResourceBatch(Collections.singletonList(button));
        jdbcTemplate.update("INSERT INTO permission (id, object_id, resource_id) VALUES (?, ?, ?)", "resource-repair-role", "custom-role", root.getId());
        final Map<String, Object> originalRoot = jdbcTemplate.queryForMap("SELECT * FROM resource WHERE id = ?", root.getId());
        final Map<String, Object> originalButton = jdbcTemplate.queryForMap("SELECT * FROM resource WHERE id = ?", button.getId());
        final List<Map<String, Object>> originalPermissions = permissions();

        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, pluginService.createPluginResource(request()));

        assertEquals(10, resources().size());
        assertEquals(originalRoot, jdbcTemplate.queryForMap("SELECT * FROM resource WHERE id = ?", root.getId()));
        assertEquals(originalButton, jdbcTemplate.queryForMap("SELECT * FROM resource WHERE id = ?", button.getId()));
        assertTrue(permissions().containsAll(originalPermissions));
        assertEquals(11, permissions().size());
    }

    @Test
    public void missingRootUsesStoredPluginNameInsteadOfRequestName() {
        PluginDTO dto = request();
        dto.setName("untrusted-request-name");

        assertEquals(ShenyuResultMessage.CREATE_SUCCESS, pluginService.createPluginResource(dto));

        assertEquals(10, resources().size());
        assertEquals(10, permissions().size());
        assertEquals(0, jdbcTemplate.queryForObject("SELECT COUNT(*) FROM resource WHERE name = ?", Integer.class, dto.getName()));
    }

    @Test
    public void missingPluginCannotCreateResources() {
        PluginDTO dto = request();
        dto.setId("missing-plugin");

        assertThrows(ValidFailException.class, () -> pluginService.createPluginResource(dto));

        assertTrue(resources().isEmpty());
    }

    @Test
    public void unrelatedSameNameOrWrongTypeCannotBeReused() {
        ResourceDO root = seedRoot();
        jdbcTemplate.update("UPDATE resource SET parent_id = ? WHERE id = ?", "unrelated-parent", root.getId());
        assertThrows(ValidFailException.class, () -> pluginService.createPluginResource(request()));
        jdbcTemplate.update("UPDATE resource SET parent_id = ?, resource_type = ? WHERE id = ?", AdminConstants.RESOURCE_PLUGIN_ID, AdminResourceEnum.THREE_MENU.getCode(), root.getId());
        assertThrows(ValidFailException.class, () -> pluginService.createPluginResource(request()));
        assertEquals(1, resources().size());
    }

    @Test
    public void duplicateRootNamesCannotBeSilentlyReused() {
        seedRoot();
        seedRoot();

        assertThrows(ValidFailException.class, () -> pluginService.createPluginResource(request()));

        assertEquals(2, resources().size());
    }

    @Test
    public void permissionAttachedToWrongParentOrTypeCannotBeDuplicated() {
        ResourceDO root = seedRoot();
        ResourceDO button = ResourceUtil.buildPluginDataPermissionResource("unrelated-parent", PLUGIN_NAME).get(0);
        resourceService.createResourceBatch(Collections.singletonList(button));
        assertThrows(ValidFailException.class, () -> pluginService.createPluginResource(request()));
        jdbcTemplate.update("UPDATE resource SET parent_id = ?, resource_type = ? WHERE id = ?", root.getId(), AdminResourceEnum.SECOND_MENU.getCode(), button.getId());
        assertThrows(ValidFailException.class, () -> pluginService.createPluginResource(request()));
        assertEquals(2, resources().size());
    }

    @Test
    public void normalPluginCreationStillRejectsDuplicateResourcesAndPluginNames() {
        seedRoot();
        PluginDTO duplicate = request();
        duplicate.setId(null);
        assertThrows(ValidFailException.class, () -> pluginService.createOrUpdate(duplicate));
        pluginMapper.delete(PLUGIN_ID);
        assertThrows(ValidFailException.class, () -> pluginService.createOrUpdate(duplicate));

        assertEquals(1, resources().size());
    }

    @Test
    public void duplicateButtonsCannotBeSilentlyReused() {
        ResourceDO root = seedRoot();
        ResourceDO button = ResourceUtil.buildPluginDataPermissionResource(root.getId(), PLUGIN_NAME).get(0);
        ResourceDO duplicate = ResourceUtil.buildPluginDataPermissionResource(root.getId(), PLUGIN_NAME).get(0);
        resourceService.createResourceBatch(List.of(button, duplicate));

        assertThrows(ValidFailException.class, () -> pluginService.createPluginResource(request()));

        assertEquals(3, resources().size());
    }

    @Test
    public void failedRepairRollsBackNewRootAndPermissions() {
        AtomicBoolean rootCreated = new AtomicBoolean();
        doAnswer(invocation -> {
            assertEquals(1, resources().size());
            assertEquals(1, permissions().size());
            rootCreated.set(true);
            throw new DataIntegrityViolationException("forced button failure");
        }).when(resourceMapper).insertBatch(anyList());

        assertThrows(DataIntegrityViolationException.class, () -> pluginService.createPluginResource(request()));

        assertTrue(rootCreated.get());
        assertTrue(resources().isEmpty());
        assertTrue(permissions().isEmpty());
    }

    @Test
    public void failedButtonPermissionEventRollsBackRepairAndPreservesExistingResources() {
        ResourceDO root = seedRoot();
        final List<Map<String, Object>> originalResources = resources();
        final List<Map<String, Object>> originalPermissions = permissions();
        doAnswer(invocation -> {
            Object result = sqlSessionTemplate.getMapper(PermissionMapper.class).insertBatch(invocation.getArgument(0));
            assertEquals(10, resources().size());
            assertEquals(10, permissions().size());
            throw new DataIntegrityViolationException("forced failure after button permission insert");
        }).when(permissionMapper).insertBatch(anyList());

        assertThrows(DataIntegrityViolationException.class, () -> pluginService.createPluginResource(request()));

        assertNotNull(root.getId());
        assertEquals(originalResources, resources());
        assertEquals(originalPermissions, permissions());
    }

    @Test
    public void concurrentRequestsSerializeAndCreateOneResourceTree() throws Exception {
        CountDownLatch firstInsert = new CountDownLatch(1);
        CountDownLatch releaseInsert = new CountDownLatch(1);
        CountDownLatch secondStarted = new CountDownLatch(1);
        AtomicBoolean first = new AtomicBoolean(true);
        doAnswer(invocation -> {
            if (first.compareAndSet(true, false)) {
                firstInsert.countDown();
                assertTrue(releaseInsert.await(10, TimeUnit.SECONDS));
            }
            return sqlSessionTemplate.getMapper(ResourceMapper.class).insertBatch(invocation.getArgument(0));
        }).when(resourceMapper).insertBatch(anyList());
        ExecutorService executor = Executors.newFixedThreadPool(2);
        try {
            final Future<String> one = executor.submit(() -> pluginService.createPluginResource(request()));
            assertTrue(firstInsert.await(10, TimeUnit.SECONDS));
            final Future<String> two = executor.submit(() -> {
                secondStarted.countDown();
                return pluginService.createPluginResource(request());
            });
            assertTrue(secondStarted.await(10, TimeUnit.SECONDS));
            assertThrows(TimeoutException.class, () -> two.get(200, TimeUnit.MILLISECONDS));
            releaseInsert.countDown();
            assertEquals(ShenyuResultMessage.CREATE_SUCCESS, one.get(10, TimeUnit.SECONDS));
            assertEquals(ShenyuResultMessage.CREATE_SUCCESS, two.get(10, TimeUnit.SECONDS));
            assertEquals(10, resources().size());
            assertEquals(10, permissions().size());
            assertFalse(first.get());
        } finally {
            releaseInsert.countDown();
            executor.shutdownNow();
            assertTrue(executor.awaitTermination(10, TimeUnit.SECONDS));
        }
    }

    private ResourceDO seedRoot() {
        ResourceDO root = ResourceUtil.buildPluginResource(PLUGIN_NAME);
        resourceService.createResourceBatch(Collections.singletonList(root));
        return root;
    }

    private PluginDTO request() {
        PluginDTO dto = new PluginDTO();
        dto.setId(PLUGIN_ID);
        dto.setName(PLUGIN_NAME);
        dto.setEnabled(true);
        dto.setRole("Proxy");
        dto.setSort(1);
        return dto;
    }

    private List<Map<String, Object>> resources() {
        return jdbcTemplate.queryForList("SELECT * FROM resource WHERE name = ? OR perms LIKE ? ORDER BY id",
                PLUGIN_NAME, "plugin:" + PLUGIN_NAME + "%");
    }

    private List<Map<String, Object>> permissions() {
        return jdbcTemplate.queryForList("SELECT * FROM permission WHERE resource_id IN (SELECT id FROM resource WHERE name = ? OR perms LIKE ?) ORDER BY id",
                PLUGIN_NAME, "plugin:" + PLUGIN_NAME + "%");
    }
}
