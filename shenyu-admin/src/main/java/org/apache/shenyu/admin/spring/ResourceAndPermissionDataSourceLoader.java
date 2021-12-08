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

import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

import com.alibaba.nacos.common.utils.CollectionUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.service.PermissionService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.ResourceService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

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
public class ResourceAndPermissionDataSourceLoader implements ApplicationRunner {

    private static final Logger LOG = LoggerFactory.getLogger(ResourceAndPermissionDataSourceLoader.class);

    private static final List<String> ICONLIST = Arrays.asList("border-bottom", "stop", "redo", "highlight", "database",
            "pause", "align-left", "camera", "pic-center", "pic-left", "retweet", "fire", "block", "thunderbolt", "safety", "key");

    private static final Integer ROUTE = 0;

    private static final Integer STATUS = 1;

    private final Random rand = new Random();

    private final PluginService pluginService;

    private final ResourceService resourceService;

    private final PermissionService permissionService;

    public ResourceAndPermissionDataSourceLoader(final PluginService pluginService, final ResourceService resourceService,
            final PermissionService permissionService) {
        this.pluginService = pluginService;
        this.resourceService = resourceService;
        this.permissionService = permissionService;
    }

    @Override
    public void run(final ApplicationArguments args) throws Exception {
        init();
    }

    /**
     * init resource table.
     */
    public void init() {
        List<PluginData> pluginDataList = pluginService.listAll();
        if (CollectionUtils.isNotEmpty(pluginDataList)) {
            pluginDataList.stream().filter(pluginData -> !getResource(pluginData.getName())).collect(Collectors.toList())
                    .forEach(item -> insertResource(item.getName(), Integer.parseInt(item.getId())));
        } else {
            LOG.error(ShenyuResultMessage.NOT_FOUND_EXCEPTION);
        }
    }

    /**
     * get resource info by plugin title.
     * @param pluginTitle plugin title
     * @return {@link Boolean}
     */
    private Boolean getResource(final String pluginTitle) {
        return ObjectUtils.notEqual(resourceService.findByTitle(pluginTitle), null);
    }

    /**
     * insert Resource for pluginName.
     * @param pluginName plugin name
     */
    private void insertResource(final String pluginName, final Integer sortId) {
        Integer sort = 0;
        ResourceDTO pluginMenuResourceDTO = ResourceDTO.builder()
                .parentId(RESOURCE_PLUGIN_ID)
                .title(pluginName)
                .name(pluginName)
                .url(RESOURCE_PLUGIN_URL_PREFIX + pluginName)
                .component(pluginName)
                .resourceType(AdminResourceEnum.SECOND_MENU.getCode())
                .sort(sortId)
                .icon(ICONLIST.get(rand.nextInt(ICONLIST.size())))
                .isLeaf(false)
                .isRoute(ROUTE)
                .perms(StringUtils.EMPTY)
                .status(STATUS)
                .build();
        resourceService.createOrUpdate(pluginMenuResourceDTO);
        ResourceVO resourceVO = resourceService.findByTitle(pluginName);
        if (ObjectUtils.notEqual(resourceVO, null)) {
            insertPerms(resourceVO.getId(), PLUGIN_SELECTOR_ADD, pluginName, PLUGIN_TYPE_SELECTOR_ADD, sort++);
            insertPerms(resourceVO.getId(), PLUGIN_SELECTOR_QUERY, pluginName, PLUGIN_TYPE_SELECTOR_QUERY, sort++);
            insertPerms(resourceVO.getId(), PLUGIN_SELECTOR_EDIT, pluginName, PLUGIN_TYPE_SELECTOR_EDIT, sort++);
            insertPerms(resourceVO.getId(), PLUGIN_SELECTOR_DELETE, pluginName, PLUGIN_TYPE_SELECTOR_DELETE, sort++);
            insertPerms(resourceVO.getId(), PLUGIN_RULE_ADD, pluginName, PLUGIN_TYPE_RULE_ADD, sort++);
            insertPerms(resourceVO.getId(), PLUGIN_RULE_QUERY, pluginName, PLUGIN_TYPE_RULE_QUERY, sort++);
            insertPerms(resourceVO.getId(), PLUGIN_RULE_EDIT, pluginName, PLUGIN_TYPE_RULE_EDIT, sort++);
            insertPerms(resourceVO.getId(), PLUGIN_RULE_DELETE, pluginName, PLUGIN_TYPE_RULE_DELETE, sort++);
            insertPerms(resourceVO.getId(), PLUGIN_SYNCHRONIZE, pluginName, PLUGIN_TYPE_SYNCHRONIZE, sort);
        }
    }

    /**
     * insert resource perms.
     * @param title resource title
     * @param type resource type
     */
    private void insertPerms(final String parentId, final String title, final String pluginName, final String type,
            final Integer sort) {
        if (StringUtils.isNotEmpty(title) && StringUtils.isNotEmpty(type)) {
            ResourceDTO resourceDTO = ResourceDTO.builder()
                    .parentId(parentId)
                    .name(StringUtils.EMPTY)
                    .title(PLUGIN_SELECTOR_ADD)
                    .url(StringUtils.EMPTY)
                    .component(StringUtils.EMPTY)
                    .resourceType(AdminResourceEnum.THREE_MENU.getCode())
                    .sort(sort)
                    .icon(StringUtils.EMPTY)
                    .isLeaf(true)
                    .isRoute(ROUTE)
                    .perms("plugin:" + pluginName + type)
                    .status(STATUS)
                    .build();
            resourceService.createOrUpdate(resourceDTO);
        }
    }
}
