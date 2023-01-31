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

package org.apache.shenyu.admin.spring;

import com.alibaba.nacos.common.utils.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.entity.ResourceDO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.ResourceService;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.security.SecureRandom;
import java.util.AbstractMap;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.apache.shenyu.common.constant.AdminConstants.DICT_TABLE_FLAG_DESC;
import static org.apache.shenyu.common.constant.AdminConstants.DICT_TABLE_FLAG_DICTCODE;
import static org.apache.shenyu.common.constant.AdminConstants.DICT_TABLE_FLAG_DICTNAME;
import static org.apache.shenyu.common.constant.AdminConstants.DICT_TABLE_FLAG_TYPE;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_RULE_ADD;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_RULE_DELETE;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_RULE_EDIT;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_RULE_QUERY;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_SELECTOR_ADD;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_SELECTOR_DELETE;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_SELECTOR_EDIT;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_SELECTOR_QUERY;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_SYNCHRONIZE;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_TYPE_RULE_ADD;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_TYPE_RULE_DELETE;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_TYPE_RULE_EDIT;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_TYPE_RULE_QUERY;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_TYPE_SELECTOR_ADD;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_TYPE_SELECTOR_DELETE;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_TYPE_SELECTOR_EDIT;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_TYPE_SELECTOR_QUERY;
import static org.apache.shenyu.common.constant.AdminConstants.PLUGIN_TYPE_SYNCHRONIZE;
import static org.apache.shenyu.common.constant.AdminConstants.RESOURCE_PLUGIN_ID;
import static org.apache.shenyu.common.constant.AdminConstants.RESOURCE_PLUGIN_URL_PREFIX;

/**
 * Admin Resource Permission Processor.
 */
@Component
public class ResourcePermissionDataSourceLoader implements ApplicationRunner {

    private static final Logger LOG = LoggerFactory.getLogger(ResourcePermissionDataSourceLoader.class);

    private static final List<String> ICONS = Arrays.asList("border-bottom", "stop", "redo", "highlight", "database",
            "pause", "align-left", "camera", "pic-center", "pic-left", "retweet", "fire", "block", "thunderbolt", "safety", "key");

    private static final Integer ROUTE = 0;

    private static final Integer STATUS = 1;

    private final Random rand = new SecureRandom();

    private final PluginService pluginService;

    private final ResourceService resourceService;

    private final ShenyuDictService shenyuDictService;

    public ResourcePermissionDataSourceLoader(final PluginService pluginService,
                                              final ResourceService resourceService,
                                              final ShenyuDictService shenyuDictService) {
        this.pluginService = pluginService;
        this.resourceService = resourceService;
        this.shenyuDictService = shenyuDictService;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void run(final ApplicationArguments args) throws Exception {
        ShenyuDictVO shenyuInitData = shenyuDictService
                .findByDictCodeName(AdminConstants.DICT_TABLE_FLAG_DICTCODE,
                        AdminConstants.DICT_TABLE_FLAG_DICTNAME);
        String id = null;
        if (Objects.nonNull(shenyuInitData)) {
            if (Boolean.TRUE.toString().equals(shenyuInitData.getDictValue())) {
                return;
            }
            // for reset (update)
            id = shenyuInitData.getId();
        }
        List<PluginData> pluginDataList = pluginService.listAllNotInResource();
        if (CollectionUtils.isEmpty(pluginDataList)) {
            LOG.info("All plugin are permissioned.");
            return;
        }
        List<String> pluginNameList = pluginDataList.stream().filter(Objects::nonNull)
                .map(PluginData::getName).collect(Collectors.toList());
        this.insertResourceBatch(pluginNameList);
        // reset (create or update) status
        resetTableDictStatus(id);
    }

    /**
     * insert Resource for pluginName.
     * @param pluginNameList list of pluginNames
     */
    private void insertResourceBatch(final List<String> pluginNameList) {

        if (CollectionUtils.isEmpty(pluginNameList)) {
            return;
        }
        List<ResourceDO> resourceList = pluginNameList.stream()
                .filter(StringUtils::isNotEmpty)
                .map(pluginName -> ResourceDTO.builder()
                        .parentId(RESOURCE_PLUGIN_ID)
                        .id(UUIDUtils.getInstance().generateShortUuid())
                        .title(pluginName)
                        .name(pluginName)
                        .url(RESOURCE_PLUGIN_URL_PREFIX + pluginName)
                        .component(pluginName)
                        .resourceType(AdminResourceEnum.SECOND_MENU.getCode())
                        .sort(0)
                        .icon(ICONS.get(rand.nextInt(ICONS.size())))
                        .isLeaf(false)
                        .isRoute(ROUTE)
                        .perms(StringUtils.EMPTY)
                        .status(STATUS)
                        .build())
                .map(ResourceDO::buildResourceDO)
                .collect(Collectors.toList());
        resourceService.createResourceBatch(resourceList);
        List<ResourceDO> resourceChildren = resourceList.stream()
                .filter(resourceDO -> Objects.nonNull(resourceDO) && StringUtils.isNotEmpty(resourceDO.getId()))
                .map(resourceDO -> this.prepareChildrenResource(resourceDO.getId(), resourceDO.getTitle()))
                .flatMap(Collection::stream)
                .collect(Collectors.toList());
        resourceService.createResourceBatch(resourceChildren);
    }

    /**
     * insert resource perms.
     *
     * @param parentId  parent id
     * @param pluginName plugin name
     * @return list of {@linkplain ResourceDO}
     */
    private List<ResourceDO> prepareChildrenResource(final String parentId, final String pluginName) {
        return Stream.of(new AbstractMap.SimpleEntry<>(PLUGIN_SELECTOR_ADD, PLUGIN_TYPE_SELECTOR_ADD),
                        new AbstractMap.SimpleEntry<>(PLUGIN_SELECTOR_QUERY, PLUGIN_TYPE_SELECTOR_QUERY),
                        new AbstractMap.SimpleEntry<>(PLUGIN_SELECTOR_EDIT, PLUGIN_TYPE_SELECTOR_EDIT),
                        new AbstractMap.SimpleEntry<>(PLUGIN_SELECTOR_DELETE, PLUGIN_TYPE_SELECTOR_DELETE),
                        new AbstractMap.SimpleEntry<>(PLUGIN_RULE_ADD, PLUGIN_TYPE_RULE_ADD),
                        new AbstractMap.SimpleEntry<>(PLUGIN_RULE_QUERY, PLUGIN_TYPE_RULE_QUERY),
                        new AbstractMap.SimpleEntry<>(PLUGIN_RULE_EDIT, PLUGIN_TYPE_RULE_EDIT),
                        new AbstractMap.SimpleEntry<>(PLUGIN_RULE_DELETE, PLUGIN_TYPE_RULE_DELETE),
                        new AbstractMap.SimpleEntry<>(PLUGIN_SYNCHRONIZE, PLUGIN_TYPE_SYNCHRONIZE))
                .map(simpleEntry -> ResourceDO.buildResourceDO(ResourceDTO.builder()
                        .parentId(parentId)
                        .name(StringUtils.EMPTY)
                        .title(simpleEntry.getKey())
                        .url(StringUtils.EMPTY)
                        .component(StringUtils.EMPTY)
                        .resourceType(AdminResourceEnum.THREE_MENU.getCode())
                        .sort(0)
                        .icon(StringUtils.EMPTY)
                        .isLeaf(true)
                        .isRoute(ROUTE)
                        .perms("plugin:" + pluginName + simpleEntry.getValue())
                        .status(STATUS)
                        .build())).collect(Collectors.toList());

    }

    /**
     * reset table dict status to 'true'.
     * @param id dict id
     */
    private void resetTableDictStatus(final String id) {
        shenyuDictService.createOrUpdate(new ShenyuDictDTO(
                id, DICT_TABLE_FLAG_TYPE, DICT_TABLE_FLAG_DICTCODE,
                DICT_TABLE_FLAG_DICTNAME, Boolean.TRUE.toString(),
                DICT_TABLE_FLAG_DESC, 0, false));
    }
}
