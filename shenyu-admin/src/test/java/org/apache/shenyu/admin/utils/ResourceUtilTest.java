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

package org.apache.shenyu.admin.utils;

import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.enums.AdminPluginOperateEnum;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Test case for {@link ResourceUtil}.
 */
public class ResourceUtilTest {

    @Test
    public void testBuildPluginResource() {
        String pluginName = "test-plugin";
        ResourceDO resource = ResourceUtil.buildPluginResource(pluginName);

        Assert.assertNotNull(resource);
        Assert.assertEquals(AdminConstants.RESOURCE_PLUGIN_ID, resource.getParentId());
        Assert.assertEquals(pluginName, resource.getTitle());
        Assert.assertEquals(pluginName, resource.getName());
        Assert.assertEquals(AdminConstants.RESOURCE_PLUGIN_URL_PREFIX + pluginName, resource.getUrl());
        Assert.assertEquals(pluginName, resource.getComponent());
        Assert.assertEquals(AdminResourceEnum.SECOND_MENU.getCode(), resource.getResourceType().intValue());
        Assert.assertEquals(AdminConstants.RESOURCE_PLUGIN_DEFAULT_ICON, resource.getIcon());
        Assert.assertFalse(resource.getIsLeaf());
        Assert.assertEquals(Integer.valueOf(1), resource.getStatus());
    }

    @Test
    public void testBuildPluginDataPermissionResource() {
        String pluginResourceId = "test-plugin-id";
        String pluginName = "test-plugin";

        List<ResourceDO> resources = ResourceUtil.buildPluginDataPermissionResource(pluginResourceId, pluginName);

        Assert.assertNotNull(resources);
        Assert.assertEquals(9, resources.size());
        // 3 selector + 3 rule + 1 plugin sync

        // Verify selector permissions
        long selectorCount = resources.stream()
                .filter(r -> r.getPerms().contains("Selector"))
                .count();
        Assert.assertEquals(4, selectorCount);
        // add, delete, edit, query

        // Verify rule permissions
        long ruleCount = resources.stream()
                .filter(r -> r.getPerms().contains("Rule"))
                .count();
        Assert.assertEquals(4, ruleCount);
        // add, delete, edit, query

        // Verify plugin sync permission
        long syncCount = resources.stream()
                .filter(r -> r.getPerms().contains("plugin:test-plugin:modify"))
                .count();
        Assert.assertEquals(1, syncCount);

        // Verify all resources have correct parent ID
        resources.forEach(r -> Assert.assertEquals(pluginResourceId, r.getParentId()));
    }

    @Test
    public void testBuildMenuWithEmptyList() {
        List<ResourceVO> emptyList = Collections.emptyList();
        var result = ResourceUtil.buildMenu(emptyList);
        Assert.assertNotNull(result);
        Assert.assertTrue(result.isEmpty());
    }

    @Test
    public void testBuildMenuWithNullResources() {
        List<ResourceVO> listWithNulls = new ArrayList<>();
        listWithNulls.add(null);
        listWithNulls.add(createResourceVO("test", null, "Test Resource"));
        var result = ResourceUtil.buildMenu(listWithNulls);
        Assert.assertNotNull(result);
        // Should handle nulls gracefully
    }

    @Test
    public void testBuildMenuWithNormalResources() {
        List<ResourceVO> resources = new ArrayList<>();

        // Root menu
        ResourceVO root = createResourceVO("root", null, "Root Menu");
        resources.add(root);

        // Child menu
        ResourceVO child = createResourceVO("child", "root", "Child Menu");
        resources.add(child);

        var result = ResourceUtil.buildMenu(resources);
        Assert.assertNotNull(result);
        Assert.assertFalse(result.isEmpty());

        // Root should be in result
        var rootMenu = result.stream()
                .filter(m -> "root".equals(m.getId()))
                .findFirst();
        Assert.assertTrue(rootMenu.isPresent());
        Assert.assertEquals("Root Menu", rootMenu.get().getMeta().getTitle());

        // Child should be in root's children
        Assert.assertNotNull(rootMenu.get().getChildren());
        Assert.assertFalse(rootMenu.get().getChildren().isEmpty());
        var childMenu = rootMenu.get().getChildren().stream()
                .filter(m -> "child".equals(m.getId()))
                .findFirst();
        Assert.assertTrue(childMenu.isPresent());
        Assert.assertEquals("Child Menu", childMenu.get().getMeta().getTitle());
    }

    @Test
    public void testGetDeleteResourceIdsWithEmptyInputs() {
        List<String> emptyIds = Collections.emptyList();
        List<ResourceDO> emptyResources = Collections.emptyList();

        var result = ResourceUtil.getDeleteResourceIds(emptyIds, emptyResources);
        Assert.assertNotNull(result);
        Assert.assertTrue(result.isEmpty());
    }

    @Test
    public void testGetDeleteResourceIdsWithNullInputs() {
        var result = ResourceUtil.getDeleteResourceIds(null, null);
        Assert.assertNotNull(result);
        Assert.assertTrue(result.isEmpty());
    }

    @Test
    public void testGetDeleteResourceIdsWithSingleResource() {
        List<String> resourceIds = List.of("test-id");
        List<ResourceDO> allResources = List.of(
                createResourceDO("test-id", "Test Resource")
        );

        var result = ResourceUtil.getDeleteResourceIds(resourceIds, allResources);
        Assert.assertNotNull(result);
        Assert.assertEquals(1, result.size());
        Assert.assertEquals("test-id", result.get(0).getId());
    }

    @Test
    public void testGetDeleteResourceIdsWithParentChildRelationship() {
        List<String> resourceIds = List.of("parent-id");

        ResourceDO parent = createResourceDO("parent-id", "Parent");
        ResourceDO child = createResourceDO("child-id", "Child");
        child.setParentId("parent-id");

        List<ResourceDO> allResources = List.of(parent, child);

        var result = ResourceUtil.getDeleteResourceIds(resourceIds, allResources);
        Assert.assertNotNull(result);
        Assert.assertEquals(2, result.size());
        // Should include both parent and child

        // Check that both IDs are present
        var ids = result.stream().map(ResourceDO::getId).toList();
        Assert.assertTrue(ids.contains("parent-id"));
        Assert.assertTrue(ids.contains("child-id"));
    }

    @Test
    public void testGetDeleteResourceIdsWithMultipleLevels() {
        // parent -> child -> grandchild
        List<String> resourceIds = List.of("parent-id");

        ResourceDO parent = createResourceDO("parent-id", "Parent");
        ResourceDO child = createResourceDO("child-id", "Child");
        child.setParentId("parent-id");
        ResourceDO grandchild = createResourceDO("grandchild-id", "Grandchild");
        grandchild.setParentId("child-id");

        List<ResourceDO> allResources = List.of(parent, child, grandchild);

        var result = ResourceUtil.getDeleteResourceIds(resourceIds, allResources);
        Assert.assertNotNull(result);
        Assert.assertEquals(3, result.size());
        // Should include all three levels

        var ids = result.stream().map(ResourceDO::getId).toList();
        Assert.assertTrue(ids.contains("parent-id"));
        Assert.assertTrue(ids.contains("child-id"));
        Assert.assertTrue(ids.contains("grandchild-id"));
    }

    @Test
    public void testGetDeleteResourceIdsWithNonExistentId() {
        List<String> resourceIds = List.of("non-existent-id");
        List<ResourceDO> allResources = List.of(
                createResourceDO("existing-id", "Existing")
        );

        var result = ResourceUtil.getDeleteResourceIds(resourceIds, allResources);
        Assert.assertNotNull(result);
        Assert.assertTrue(result.isEmpty());
        // Non-existent ID should not cause issues
    }

    @Test
    public void testDataPermissionConstructor() {
        var dataPermission = new ResourceUtil.DataPermission(ConfigGroupEnum.SELECTOR, AdminPluginOperateEnum.ADD);
        Assert.assertNotNull(dataPermission);
        // Since fields are private, we can't directly test them, but constructor should work
    }

    @Test
    public void testBuildMenuSorting() {
        // Create resources with different sort orders
        ResourceVO parent = createResourceVO("parent", null, "Parent");
        parent.setSort(1);

        ResourceVO child1 = createResourceVO("child1", "parent", "Child 1");
        child1.setSort(2);

        ResourceVO child2 = createResourceVO("child2", "parent", "Child 2");
        child2.setSort(1);
        // Should come before child1

        List<ResourceVO> resources = new ArrayList<>();
        resources.add(parent);
        resources.add(child1);
        resources.add(child2);

        var result = ResourceUtil.buildMenu(resources);

        // Find parent and check children order
        var parentMenu = result.stream()
                .filter(m -> "parent".equals(m.getId()))
                .findFirst();
        Assert.assertTrue(parentMenu.isPresent());

        var children = parentMenu.get().getChildren();
        Assert.assertNotNull(children);
        Assert.assertEquals(2, children.size());

        // child2 should come before child1 due to sort order
        Assert.assertEquals("child2", children.get(0).getId());
        Assert.assertEquals("child1", children.get(1).getId());
    }

    @Test
    public void testBuildMenuWithNullSort() {
        ResourceVO parent = createResourceVO("parent", null, "Parent");
        parent.setSort(null);

        ResourceVO child1 = createResourceVO("child1", "parent", "Child 1");
        child1.setSort(1);

        ResourceVO child2 = createResourceVO("child2", "parent", "Child 2");
        child2.setSort(null);

        List<ResourceVO> resources = new ArrayList<>();
        resources.add(parent);
        resources.add(child1);
        resources.add(child2);

        var result = ResourceUtil.buildMenu(resources);

        // Should handle null sort values gracefully
        var parentMenu = result.stream()
                .filter(m -> "parent".equals(m.getId()))
                .findFirst();
        Assert.assertTrue(parentMenu.isPresent());
        Assert.assertNotNull(parentMenu.get().getChildren());
    }

    private ResourceVO createResourceVO(final String id, final String parentId, final String title) {
        ResourceVO vo = new ResourceVO();
        vo.setId(id);
        vo.setParentId(parentId);
        vo.setTitle(title);
        vo.setName(title);
        vo.setUrl("/test");
        vo.setComponent("TestComponent");
        vo.setResourceType(1);
        vo.setSort(1);
        vo.setIcon("icon");
        vo.setIsLeaf(false);
        vo.setIsRoute(0);
        vo.setPerms("test:perm");
        vo.setStatus(1);
        return vo;
    }

    private ResourceDO createResourceDO(final String id, final String title) {
        ResourceDO resourceDO = new ResourceDO();
        resourceDO.setId(id);
        resourceDO.setTitle(title);
        resourceDO.setName(title);
        resourceDO.setUrl("/test");
        resourceDO.setComponent("TestComponent");
        resourceDO.setResourceType(1);
        resourceDO.setSort(1);
        resourceDO.setIcon("icon");
        resourceDO.setIsLeaf(false);
        resourceDO.setIsRoute(0);
        resourceDO.setPerms("test:perm");
        resourceDO.setStatus(1);
        return resourceDO;
    }
}

