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
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.ResourceDTO;
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.vo.ResourceVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.ResourceService;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.AdminResourceEnum;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.stereotype.Component;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.AdminConstants.DICT_TABLE_FLAG_DEFAULT_DICTVALUE;
import static org.apache.shenyu.common.constant.AdminConstants.DICT_TABLE_FLAG_DESC;
import static org.apache.shenyu.common.constant.AdminConstants.DICT_TABLE_FLAG_DICTCODE;
import static org.apache.shenyu.common.constant.AdminConstants.DICT_TABLE_FLAG_DICTNAME;
import static org.apache.shenyu.common.constant.AdminConstants.DICT_TABLE_FLAG_SORT;
import static org.apache.shenyu.common.constant.AdminConstants.DICT_TABLE_FLAG_TRUE_DICTVALUE;
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
public class ResourceAndPermissionDataSourceLoader implements ApplicationRunner {

    private static final Logger LOG = LoggerFactory.getLogger(ResourceAndPermissionDataSourceLoader.class);

    private static final List<String> ICONLIST = Arrays.asList("border-bottom", "stop", "redo", "highlight", "database",
            "pause", "align-left", "camera", "pic-center", "pic-left", "retweet", "fire", "block", "thunderbolt", "safety", "key");

    private static final Integer ROUTE = 0;

    private static final Integer STATUS = 1;

    private final Random rand = new Random();

    private final PluginService pluginService;

    private final ResourceService resourceService;

    private final ShenyuDictService shenyuDictService;

    public ResourceAndPermissionDataSourceLoader(final PluginService pluginService, final ResourceService resourceService,
                                                 final ShenyuDictService shenyuDictService) {
        this.pluginService = pluginService;
        this.resourceService = resourceService;
        this.shenyuDictService = shenyuDictService;
    }

    @Override
    public void run(final ApplicationArguments args) throws Exception {
        if (!checkInitStatus(DICT_TABLE_FLAG_DICTCODE, DICT_TABLE_FLAG_DICTNAME)) {
            init();
        }
    }

    /**
     * init resource table.
     */
    public void init() {
        List<PluginData> pluginDataList = pluginService.listAll();
        if (CollectionUtils.isNotEmpty(pluginDataList)) {
            pluginDataList.stream().filter(pluginData -> getResource(pluginData.getName())).collect(Collectors.toList())
                    .forEach(item -> insertResource(item.getName()));
            updateTableDictStatus(DICT_TABLE_FLAG_TRUE_DICTVALUE,
                    shenyuDictService.findByDictCodeAndDictName(DICT_TABLE_FLAG_DICTCODE, DICT_TABLE_FLAG_DICTNAME).getId());
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
        return Objects.isNull(resourceService.findByTitle(pluginTitle));
    }

    /**
     * insert Resource for pluginName.
     * @param pluginName plugin name
     */
    private void insertResource(final String pluginName) {
        ResourceDTO pluginMenuResourceDTO = ResourceDTO.builder()
                .parentId(RESOURCE_PLUGIN_ID)
                .title(pluginName)
                .name(pluginName)
                .url(RESOURCE_PLUGIN_URL_PREFIX + pluginName)
                .component(pluginName)
                .resourceType(AdminResourceEnum.SECOND_MENU.getCode())
                .sort(0)
                .icon(ICONLIST.get(rand.nextInt(ICONLIST.size())))
                .isLeaf(false)
                .isRoute(ROUTE)
                .perms(StringUtils.EMPTY)
                .status(STATUS)
                .build();
        resourceService.createOrUpdate(pluginMenuResourceDTO);
        ResourceVO resourceVO = resourceService.findByTitle(pluginName);
        if (ObjectUtils.notEqual(resourceVO, null)) {
            insertPerms(resourceVO.getId(), PLUGIN_SELECTOR_ADD, pluginName, PLUGIN_TYPE_SELECTOR_ADD);
            insertPerms(resourceVO.getId(), PLUGIN_SELECTOR_QUERY, pluginName, PLUGIN_TYPE_SELECTOR_QUERY);
            insertPerms(resourceVO.getId(), PLUGIN_SELECTOR_EDIT, pluginName, PLUGIN_TYPE_SELECTOR_EDIT);
            insertPerms(resourceVO.getId(), PLUGIN_SELECTOR_DELETE, pluginName, PLUGIN_TYPE_SELECTOR_DELETE);
            insertPerms(resourceVO.getId(), PLUGIN_RULE_ADD, pluginName, PLUGIN_TYPE_RULE_ADD);
            insertPerms(resourceVO.getId(), PLUGIN_RULE_QUERY, pluginName, PLUGIN_TYPE_RULE_QUERY);
            insertPerms(resourceVO.getId(), PLUGIN_RULE_EDIT, pluginName, PLUGIN_TYPE_RULE_EDIT);
            insertPerms(resourceVO.getId(), PLUGIN_RULE_DELETE, pluginName, PLUGIN_TYPE_RULE_DELETE);
            insertPerms(resourceVO.getId(), PLUGIN_SYNCHRONIZE, pluginName, PLUGIN_TYPE_SYNCHRONIZE);
        }
    }

    /**
     * insert resource perms.
     * @param title resource title
     * @param type resource type
     */
    private void insertPerms(final String parentId, final String title, final String pluginName, final String type) {
        if (StringUtils.isNotEmpty(title) && StringUtils.isNotEmpty(type)) {
            ResourceDTO resourceDTO = ResourceDTO.builder()
                    .parentId(parentId)
                    .name(StringUtils.EMPTY)
                    .title(PLUGIN_SELECTOR_ADD)
                    .url(StringUtils.EMPTY)
                    .component(StringUtils.EMPTY)
                    .resourceType(AdminResourceEnum.THREE_MENU.getCode())
                    .sort(0)
                    .icon(StringUtils.EMPTY)
                    .isLeaf(true)
                    .isRoute(ROUTE)
                    .perms("plugin:" + pluginName + type)
                    .status(STATUS)
                    .build();
            resourceService.createOrUpdate(resourceDTO);
        }
    }

    /**
     * check init status.
     * @param dictCode shenyu dict code
     * @param dictName shenyu dict name
     * @return {@link Boolean}
     */
    private Boolean checkInitStatus(final String dictCode, final String dictName) {
        ShenyuDictVO shenyuDictVO = shenyuDictService.findByDictCodeAndDictName(dictCode, dictName);
        if (Objects.isNull(shenyuDictVO)) {
            updateTableDictStatus(DICT_TABLE_FLAG_DEFAULT_DICTVALUE, null);
            return false;
        }
        return !DICT_TABLE_FLAG_DEFAULT_DICTVALUE.equals(shenyuDictVO.getDictValue());
    }

    /**
     * update table dict value.
     * @param dictValue dict value
     * @param id dict id
     */
    private void updateTableDictStatus(final String dictValue, final String id) {
        shenyuDictService.createOrUpdate(ShenyuDictDTO.builder()
                .id(id)
                .type(DICT_TABLE_FLAG_TYPE)
                .dictCode(DICT_TABLE_FLAG_DICTCODE)
                .dictName(DICT_TABLE_FLAG_DICTNAME)
                .dictValue(dictValue)
                .desc(DICT_TABLE_FLAG_DESC)
                .sort(DICT_TABLE_FLAG_SORT)
                .enabled(false).build());
    }
}
