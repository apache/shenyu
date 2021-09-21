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

import org.apache.shenyu.admin.mapper.PermissionMapper;
import org.apache.shenyu.admin.mapper.ResourceMapper;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.ResourceQuery;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.service.impl.ResourceServiceImpl;
import org.apache.shenyu.common.constant.ResourceTypeConstants;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import static com.google.common.collect.Lists.newArrayList;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.reset;
import static org.mockito.Mockito.when;

/**
 * test for {@linkplain ResourceService}.
 */
@RunWith(MockitoJUnitRunner.class)
public class ResourceServiceTest {

    @InjectMocks
    private ResourceServiceImpl resourceService;

    @Mock
    private ResourceMapper resourceMapper;

    @Mock
    private PermissionMapper permissionMapper;

    @Test
    public void testCreateResource() {
        final ResourceDO resourceDO = new ResourceDO();
        resourceDO.setId("mock resource id");

        reset(resourceMapper);
        reset(permissionMapper);
        when(resourceMapper.insertSelective(resourceDO)).thenReturn(1);

        resourceService.createResource(resourceDO);
    }

    @Test
    public void testCreateOrUpdate() {
        // test update
        ResourceDTO forUpdateResource = new ResourceDTO();
        forUpdateResource.setId("mock id");
        reset(resourceMapper);
        when(resourceMapper.updateSelective(ResourceDO.buildResourceDO(forUpdateResource))).thenReturn(1);
        assertThat(resourceService.createOrUpdate(forUpdateResource), equalTo(1));
    }

    @Test
    public void testDelete() {
        ResourceDO resourceDO = new ResourceDO();
        resourceDO.setId("mock resource parent id");
        resourceDO.setParentId("");
        resourceDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        resourceDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));

        ResourceDO childResourceDO = new ResourceDO();
        childResourceDO.setId("mock resource child id");
        childResourceDO.setParentId("mock resource parent id");
        childResourceDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        childResourceDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));

        List<ResourceDO> resources = newArrayList(resourceDO, childResourceDO);
        List<String> deleteResourceIds = newArrayList("mock resource parent id", "mock resource child id");

        reset(resourceMapper);
        when(resourceMapper.selectAll()).thenReturn(resources);
        when(resourceMapper.delete(deleteResourceIds)).thenReturn(2);
        assertThat(resourceService.delete(deleteResourceIds), equalTo(2));
    }

    @Test
    public void testSelectById() {
        final String mockResourceId = "mock resource id";
        ResourceDO resourceDO = new ResourceDO();
        resourceDO.setId(mockResourceId);
        resourceDO.setParentId("mock resource parent id");
        resourceDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        resourceDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));

        reset(resourceMapper);
        when(resourceMapper.selectById(mockResourceId)).thenReturn(resourceDO);
        assertThat(resourceService.findById(mockResourceId), equalTo(ResourceVO.buildResourceVO(resourceDO)));
    }

    @Test
    public void testSelectByTitle() {
        final String mockResourceTitle = "mock resource title";
        ResourceDO resourceDO = new ResourceDO();
        resourceDO.setId("mock resource id");
        resourceDO.setParentId("mock resource parent id");
        resourceDO.setTitle(mockResourceTitle);
        resourceDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        resourceDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));

        reset(resourceMapper);
        when(resourceMapper.selectByTitle(mockResourceTitle)).thenReturn(resourceDO);
        assertThat(resourceService.findByTitle(mockResourceTitle), equalTo(ResourceVO.buildResourceVO(resourceDO)));
    }

    @Test
    public void testListByPage() {
        final String queryTitle = "mock query title";
        ResourceDO resourceDO = new ResourceDO();
        resourceDO.setId("mock resource id");
        resourceDO.setDateCreated(new Timestamp(System.currentTimeMillis()));
        resourceDO.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        PageParameter pageParameter = new PageParameter();
        ResourceQuery query = new ResourceQuery();
        query.setTitle(queryTitle);
        query.setPageParameter(pageParameter);
        when(resourceMapper.selectByQuery(query)).thenReturn(Collections.singletonList(resourceDO));
        List<ResourceVO> resourceList = newArrayList(ResourceVO.buildResourceVO(resourceDO));
        CommonPager<ResourceVO> commonPager = resourceService.listByPage(query);
        assertThat(commonPager.getDataList().size(), is(resourceList.size()));
    }

    @Test
    public void testGetMenuTreeWhenTheresNoResource() {
        reset(resourceMapper);
        when(resourceMapper.selectAll()).thenReturn(Collections.emptyList());
        assertNull(resourceService.getMenuTree());
    }

    @Test
    public void testGetMenuTreeWhenThereAreResources() {
        ResourceDO parentResource = new ResourceDO();
        parentResource.setId("mock parent resource id");
        parentResource.setParentId("mock resource parent id");
        parentResource.setTitle("mock parent resource title");
        parentResource.setResourceType(ResourceTypeConstants.MENU_TYPE_2);
        parentResource.setDateCreated(new Timestamp(System.currentTimeMillis()));
        parentResource.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        ResourceDO childResource1 = new ResourceDO();
        childResource1.setId("mock child resource1 id");
        childResource1.setParentId("mock parent resource id");
        childResource1.setTitle("mock parent resource1 title");
        childResource1.setResourceType(ResourceTypeConstants.MENU_TYPE_2);
        childResource1.setDateCreated(new Timestamp(System.currentTimeMillis()));
        childResource1.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        ResourceDO childResource2 = new ResourceDO();
        childResource2.setId("mock child resource2 id");
        childResource2.setParentId("mock parent resource id");
        childResource2.setTitle("mock parent resource2 title");
        childResource2.setResourceType(ResourceTypeConstants.MENU_TYPE_2);
        childResource2.setDateCreated(new Timestamp(System.currentTimeMillis()));
        childResource2.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        final List<ResourceDO> mockSelectAllResult = newArrayList(parentResource, childResource1, childResource2);

        reset(resourceMapper);
        when(resourceMapper.selectAll()).thenReturn(mockSelectAllResult);

        List<PermissionMenuVO.MenuInfo> menuInfoList = new ArrayList<>();
        resourceService.getMenuInfo(menuInfoList, mockSelectAllResult.stream().map(ResourceVO::buildResourceVO).collect(Collectors.toList()), null);
        assertThat(resourceService.getMenuTree(), equalTo(menuInfoList));
    }

    @Test
    public void testFindByParentId() {
        final String mockParentResourceId = "mock resource parent id";
        ResourceDO mockResource = new ResourceDO();
        mockResource.setId("mock parent resource id");
        mockResource.setParentId(mockParentResourceId);
        mockResource.setTitle("mock parent resource title");
        mockResource.setResourceType(AdminResourceEnum.THREE_MENU.getCode());
        mockResource.setDateCreated(new Timestamp(System.currentTimeMillis()));
        mockResource.setDateUpdated(new Timestamp(System.currentTimeMillis()));

        final List<ResourceDO> mockSelectByParentIdResult = newArrayList(mockResource);

        reset(resourceMapper);
        when(resourceMapper.selectByParentId(mockParentResourceId)).thenReturn(mockSelectByParentIdResult);

        final List<ResourceVO> expect = mockSelectByParentIdResult.stream()
                .filter(item -> item.getResourceType().equals(AdminResourceEnum.THREE_MENU.getCode()))
                .map(ResourceVO::buildResourceVO)
                .collect(Collectors.toList());
        assertThat(resourceService.findByParentId(mockParentResourceId), equalTo(expect));
    }

    @Test
    public void testGetMenuInfoWithGivingEmptyOrJustContainsNullResourceVoListItShouldNotAppendMenuInfoIntoResult() {
        final List<PermissionMenuVO.MenuInfo> expect = newArrayList();
        resourceService.getMenuInfo(expect, Collections.emptyList(), null);
        assertThat(expect, equalTo(newArrayList()));
    }

    @Test
    public void testGetMenuInfoCodeCoverage() {
        ResourceVO nullMenuInfoResource = new ResourceVO();
        nullMenuInfoResource.setResourceType(ResourceTypeConstants.MENU_TYPE_2);
        nullMenuInfoResource.setIsLeaf(false);

        ResourceVO mockParentResource = new ResourceVO();
        mockParentResource.setId("mock parent");
        mockParentResource.setResourceType(AdminResourceEnum.SECOND_MENU.getCode());
        mockParentResource.setIsLeaf(false);

        ResourceVO mockSecondLevelResource = new ResourceVO();
        mockSecondLevelResource.setId("mock 2nd");
        mockSecondLevelResource.setParentId("mock parent");
        mockSecondLevelResource.setResourceType(AdminResourceEnum.SECOND_MENU.getCode());
        mockSecondLevelResource.setIsLeaf(false);

        ResourceVO mockThirdLevelResource = new ResourceVO();
        mockThirdLevelResource.setId("mock 3nd");
        mockThirdLevelResource.setParentId("mock 2nd");
        mockThirdLevelResource.setResourceType(AdminResourceEnum.SECOND_MENU.getCode());
        mockThirdLevelResource.setIsLeaf(true);

        final List<PermissionMenuVO.MenuInfo> actual = newArrayList();
        final List<ResourceVO> resourceParam = newArrayList(nullMenuInfoResource, mockParentResource, mockSecondLevelResource, mockThirdLevelResource);
        resourceService.getMenuInfo(actual, resourceParam, null);

        PermissionMenuVO.MenuInfo parentMenuInfo = PermissionMenuVO.MenuInfo.buildMenuInfo(mockParentResource);
        PermissionMenuVO.MenuInfo secondMenuInfo = PermissionMenuVO.MenuInfo.buildMenuInfo(mockSecondLevelResource);
        PermissionMenuVO.MenuInfo thirdMenuInfo = PermissionMenuVO.MenuInfo.buildMenuInfo(mockThirdLevelResource);
        secondMenuInfo.setChildren(Collections.singletonList(thirdMenuInfo));
        parentMenuInfo.setChildren(Collections.singletonList(secondMenuInfo));
        final List<PermissionMenuVO.MenuInfo> expect = newArrayList(parentMenuInfo);
        assertThat(actual, equalTo(expect));
    }
}
