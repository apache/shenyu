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

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.vo.PermissionMenuVO;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.enums.AdminPluginOperateEnum;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.apache.shenyu.common.enums.ConfigGroupEnum;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * ResourceUtil.
 */
public final class ResourceUtil {
    
    /**
     * new plugin init data permission.
     */
    private static final List<DataPermission> NEW_PLUGIN_DATA_PERMISSION = new ArrayList<>();
    
    private ResourceUtil() {
    }
    
    static {
        // selector.
        NEW_PLUGIN_DATA_PERMISSION.add(new DataPermission(ConfigGroupEnum.SELECTOR, AdminPluginOperateEnum.ADD));
        NEW_PLUGIN_DATA_PERMISSION.add(new DataPermission(ConfigGroupEnum.SELECTOR, AdminPluginOperateEnum.DELETE));
        NEW_PLUGIN_DATA_PERMISSION.add(new DataPermission(ConfigGroupEnum.SELECTOR, AdminPluginOperateEnum.EDIT));
        NEW_PLUGIN_DATA_PERMISSION.add(new DataPermission(ConfigGroupEnum.SELECTOR, AdminPluginOperateEnum.QUERY));
        // rule
        NEW_PLUGIN_DATA_PERMISSION.add(new DataPermission(ConfigGroupEnum.RULE, AdminPluginOperateEnum.ADD));
        NEW_PLUGIN_DATA_PERMISSION.add(new DataPermission(ConfigGroupEnum.RULE, AdminPluginOperateEnum.DELETE));
        NEW_PLUGIN_DATA_PERMISSION.add(new DataPermission(ConfigGroupEnum.RULE, AdminPluginOperateEnum.EDIT));
        NEW_PLUGIN_DATA_PERMISSION.add(new DataPermission(ConfigGroupEnum.RULE, AdminPluginOperateEnum.QUERY));
        // plugin synchronize data
        NEW_PLUGIN_DATA_PERMISSION.add(new DataPermission(ConfigGroupEnum.PLUGIN, AdminPluginOperateEnum.SYNCHRONIZE));
    }
    
    /**
     * build plugin resource.
     *
     * @param pluginName plugin name
     * @return resource
     */
    public static ResourceDO buildPluginResource(final String pluginName) {
        return ResourceDO.buildResourceDO(ResourceDTO.builder()
                .parentId(AdminConstants.RESOURCE_PLUGIN_ID)
                .title(pluginName)
                .name(pluginName)
                .url(AdminConstants.RESOURCE_PLUGIN_URL_PREFIX + pluginName)
                .component(pluginName)
                .resourceType(AdminResourceEnum.SECOND_MENU.getCode())
                .sort(0)
                .icon(AdminConstants.RESOURCE_PLUGIN_DEFAULT_ICON)
                .isLeaf(Boolean.FALSE)
                .isRoute(0)
                .status(1)
                .perms(StringUtils.EMPTY)
                .build());
    }
    
    /**
     * build new plugin data permission resource.
     *
     * @param pluginResourceId plugin resources id
     * @param pluginName       plugin name
     * @return resource
     */
    public static List<ResourceDO> buildPluginDataPermissionResource(final String pluginResourceId, final String pluginName) {
        return NEW_PLUGIN_DATA_PERMISSION.stream()
                .map(p -> buildPluginButtonResource(pluginResourceId, pluginName, p.dataType, p.operate))
                .collect(Collectors.toList());
        
    }
    
    /**
     * build Plugin Selector Button Resource.
     *
     * @param parentId               parent menu id
     * @param pluginName             plugin name
     * @param configGroupEnum        {@linkplain ConfigGroupEnum}
     * @param adminPluginOperateEnum {@linkplain AdminPluginOperateEnum}
     */
    private static ResourceDO buildPluginButtonResource(final String parentId, final String pluginName,
                                                        final ConfigGroupEnum configGroupEnum, final AdminPluginOperateEnum adminPluginOperateEnum) {
        ResourceDO resourceDO = ResourceDO.buildResourceDO(ResourceDTO.builder()
                .parentId(parentId)
                .name(StringUtils.EMPTY).url(StringUtils.EMPTY).component(StringUtils.EMPTY)
                .resourceType(AdminResourceEnum.THREE_MENU.getCode())
                .isLeaf(Boolean.TRUE)
                .status(1)
                .sort(0)
                .icon(StringUtils.EMPTY)
                .isRoute(0)
                .build());
        switch (configGroupEnum) {
            case SELECTOR:
                resourceDO.setTitle("SHENYU.BUTTON.PLUGIN." + ConfigGroupEnum.SELECTOR.name() + "." + adminPluginOperateEnum.name());
                resourceDO.setPerms("plugin:" + pluginName + "Selector:" + adminPluginOperateEnum.getName());
                break;
            case RULE:
                resourceDO.setTitle("SHENYU.BUTTON.PLUGIN." + ConfigGroupEnum.RULE.name() + "." + adminPluginOperateEnum.name());
                resourceDO.setPerms("plugin:" + pluginName + "Rule:" + adminPluginOperateEnum.getName());
                break;
            case PLUGIN:
                resourceDO.setTitle("SHENYU.BUTTON.PLUGIN." + adminPluginOperateEnum.name());
                resourceDO.setPerms("plugin:" + pluginName + ":" + adminPluginOperateEnum.getName());
                break;
            default:
                break;
        }
        
        return resourceDO;
    }
    
    
    /**
     * get Menu Info.
     *
     * @param metaList {@linkplain List} resource list
     * @return {@linkplain List} menu infos.
     */
    public static List<PermissionMenuVO.MenuInfo> buildMenu(final List<ResourceVO> metaList) {
        
        List<PermissionMenuVO.MenuInfo> retList = new ArrayList<>();
        if (CollectionUtils.isEmpty(metaList)) {
            return retList;
        }
        Map<String, PermissionMenuVO.MenuInfo> menuInfoMap = metaList.stream()
                .map(PermissionMenuVO.MenuInfo::buildMenuInfo)
                .filter(menuInfo -> Objects.nonNull(menuInfo) && StringUtils.isNotEmpty(menuInfo.getId()))
                .collect(Collectors.toMap(PermissionMenuVO.MenuInfo::getId, Function.identity(), (value1, value2) -> value1));
        
        metaList.stream()
                .filter(meta -> Objects.nonNull(meta) && StringUtils.isNotEmpty(meta.getId()))
                .collect(Collectors.toMap(ResourceVO::getParentId, ResourceUtil::convertIds, ListUtil::mergeSet))
                .forEach((parent, children) -> {
                    PermissionMenuVO.MenuInfo menuInfo = menuInfoMap.get(parent);
                    if (CollectionUtils.isNotEmpty(children)) {
                        List<PermissionMenuVO.MenuInfo> targetList = Objects.isNull(menuInfo) ? retList : menuInfo.getChildren();
                        children.forEach(child -> {
                            PermissionMenuVO.MenuInfo data = menuInfoMap.get(child);
                            if (Objects.nonNull(data)) {
                                targetList.add(data);
                            }
                        });
                    }
                });
        
        return retList;
    }
    
    
    /**
     * get delete resource ids.
     *
     * @param resourceIds          resource ids
     * @param allMetaDataResources all resource object
     * @return the list of ids to delete
     */
    public static List<ResourceDO> getDeleteResourceIds(final List<String> resourceIds, final List<ResourceDO> allMetaDataResources) {
        if (CollectionUtils.isEmpty(allMetaDataResources) || CollectionUtils.isEmpty(resourceIds)) {
            return Collections.emptyList();
        }
        final List<ResourceDO> deleteResourceIds = new ArrayList<>();
        final Map<String, ResourceDO> metaMap = ListUtil.toMap(allMetaDataResources, ResourceDO::getId);
        final Map<String, Set<String>> metaChildrenMap = dealChildrenMap(allMetaDataResources);
        
        final Deque<String> cacheDatas = new ArrayDeque<>(resourceIds);
        while (!cacheDatas.isEmpty()) {
            String resourceId = cacheDatas.pollFirst();
            ResourceDO resourceVO = metaMap.get(resourceId);
            Set<String> children = metaChildrenMap.get(resourceId);
            if (Objects.nonNull(resourceVO)) {
                deleteResourceIds.add(resourceVO);
                metaMap.remove(resourceId);
            }
            if (CollectionUtils.isNotEmpty(children)) {
                children.forEach(cacheDatas::addFirst);
                metaChildrenMap.remove(resourceId);
            }
        }
        return deleteResourceIds;
    }
    
    /**
     * convert the list to a map, the key is the parent id, and the value is the set of child ids.
     *
     * @param metaList the list to be converted
     * @return the map
     */
    private static Map<String, Set<String>> dealChildrenMap(final List<ResourceDO> metaList) {
        return metaList.stream()
                .filter(meta -> Objects.nonNull(meta) && StringUtils.isNotEmpty(meta.getId()))
                .collect(Collectors.toMap(ResourceDO::getParentId, ResourceUtil::convertIds, ListUtil::mergeSet));
    }
    
    private static Set<String> convertIds(final ResourceDO resourceDO) {
        Set<String> idSet = new HashSet<>();
        idSet.add(resourceDO.getId());
        return idSet;
    }
    
    private static Set<String> convertIds(final ResourceVO resourceVO) {
        Set<String> idSet = new HashSet<>();
        idSet.add(resourceVO.getId());
        return idSet;
    }
    
    public static class DataPermission {
        
        private final ConfigGroupEnum dataType;
        
        private final AdminPluginOperateEnum operate;
        
        public DataPermission(final ConfigGroupEnum dataType, final AdminPluginOperateEnum operate) {
            this.dataType = dataType;
            this.operate = operate;
        }
    }
    
}
