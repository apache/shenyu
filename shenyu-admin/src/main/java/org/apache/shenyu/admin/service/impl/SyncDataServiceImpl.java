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

package org.apache.shenyu.admin.service.impl;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.listener.DataChangedEvent;
import org.apache.shenyu.admin.model.vo.PluginVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.PluginService;
import org.apache.shenyu.admin.service.RuleService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.SyncDataService;
import org.apache.shenyu.admin.transfer.PluginTransfer;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.SyncDataService}.
 */
@Service
public class SyncDataServiceImpl implements SyncDataService {

    private final AppAuthService appAuthService;

    /**
     * The Plugin service.
     */
    private final PluginService pluginService;

    /**
     * The Selector service.
     */
    private final SelectorService selectorService;

    /**
     * The Rule service.
     */
    private final RuleService ruleService;

    private final ApplicationEventPublisher eventPublisher;

    private final MetaDataService metaDataService;

    public SyncDataServiceImpl(final AppAuthService appAuthService,
                               final PluginService pluginService,
                               final SelectorService selectorService,
                               final RuleService ruleService,
                               final ApplicationEventPublisher eventPublisher,
                               final MetaDataService metaDataService) {
        this.appAuthService = appAuthService;
        this.pluginService = pluginService;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.eventPublisher = eventPublisher;
        this.metaDataService = metaDataService;
    }

    @Override
    public boolean syncAll(final DataEventTypeEnum type) {
        appAuthService.syncData();

        List<PluginData> pluginDataList = pluginService.listAll();
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, type, pluginDataList));

        List<SelectorData> selectorDataList = selectorService.listAll();
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, type, selectorDataList));

        List<RuleData> ruleDataList = ruleService.listAll();
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, type, ruleDataList));

        metaDataService.syncData();

        return true;
    }

    @Override
    public boolean syncPluginData(final String pluginId) {
        PluginVO pluginVO = pluginService.findById(pluginId);
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(PluginTransfer.INSTANCE.mapDataTOVO(pluginVO))));

        List<SelectorData> selectorDataList = selectorService.findByPluginId(pluginId);
        if (CollectionUtils.isEmpty(selectorDataList)) {
            return true;
        }

        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.REFRESH, selectorDataList));

        List<String> selectorIdList = selectorDataList.stream().map(SelectorData::getId)
                .collect(Collectors.toList());
        List<RuleData> allRuleDataList = ruleService.findBySelectorIdList(selectorIdList);

        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.REFRESH, allRuleDataList));

        return true;
    }
}
