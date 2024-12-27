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
import org.apache.shenyu.admin.model.vo.NamespacePluginVO;
import org.apache.shenyu.admin.service.AppAuthService;
import org.apache.shenyu.admin.service.DiscoveryService;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.MetaDataService;
import org.apache.shenyu.admin.service.NamespacePluginService;
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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.SyncDataService}.
 */
@Service
public class SyncDataServiceImpl implements SyncDataService {
    
    private static final Logger LOG = LoggerFactory.getLogger(SyncDataServiceImpl.class);

    private final AppAuthService appAuthService;

    /**
     * The Plugin service.
     */
    private final PluginService pluginService;

    /**
     * The Plugin Namespace service.
     */
    private final NamespacePluginService namespacePluginService;

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

    private final DiscoveryService discoveryService;

    private final DiscoveryUpstreamService discoveryUpstreamService;

    public SyncDataServiceImpl(final AppAuthService appAuthService,
                               final PluginService pluginService,
                               final NamespacePluginService namespacePluginService,
                               final SelectorService selectorService,
                               final RuleService ruleService,
                               final ApplicationEventPublisher eventPublisher,
                               final MetaDataService metaDataService,
                               final DiscoveryUpstreamService discoveryUpstreamService,
                               final DiscoveryService discoveryService) {
        this.appAuthService = appAuthService;
        this.pluginService = pluginService;
        this.namespacePluginService = namespacePluginService;
        this.selectorService = selectorService;
        this.ruleService = ruleService;
        this.eventPublisher = eventPublisher;
        this.metaDataService = metaDataService;
        this.discoveryUpstreamService = discoveryUpstreamService;
        this.discoveryService = discoveryService;
    }

    @Override
    public boolean syncAll(final DataEventTypeEnum type) {
        appAuthService.syncData();

        List<PluginData> pluginDataList = namespacePluginService.listAll();
        Map<String, List<PluginData>> namespacePluginDataList =
                pluginDataList.stream().collect(Collectors.groupingBy(PluginData::getNamespaceId));
        namespacePluginDataList.values().forEach(p -> {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, type, p));
        });

        List<SelectorData> selectorDataList = selectorService.listAll();
        Map<String, List<SelectorData>> namespaceSelectorDataList =
                selectorDataList.stream().collect(Collectors.groupingBy(SelectorData::getNamespaceId));
        namespaceSelectorDataList.values().forEach(s -> {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, type, s));
        });

        List<RuleData> ruleDataList = ruleService.listAll();
        Map<String, List<RuleData>> namespaceRuleDataList =
                ruleDataList.stream().collect(Collectors.groupingBy(RuleData::getNamespaceId));
        namespaceRuleDataList.values().forEach(r -> {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, type, r));
        });

        metaDataService.syncData();
        discoveryService.syncData();
        return true;
    }

    @Override
    public boolean syncAllByNamespaceId(final DataEventTypeEnum type, final String namespaceId) {
        appAuthService.syncDataByNamespaceId(namespaceId);

        List<PluginData> pluginDataList = namespacePluginService.listAll(namespaceId);

        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, type, pluginDataList));

        List<SelectorData> selectorDataList = selectorService.listAllByNamespaceId(namespaceId);
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, type, selectorDataList));

        List<RuleData> ruleDataList = ruleService.listAllByNamespaceId(namespaceId);
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, type, ruleDataList));

        metaDataService.syncDataByNamespaceId(namespaceId);
        discoveryService.syncDataByNamespaceId(namespaceId);
        return true;
    }

    @Override
    public boolean syncPluginData(final String id) {
        NamespacePluginVO namespacePluginVO = namespacePluginService.findById(id);
        if (Objects.isNull(namespacePluginVO) || Objects.isNull(namespacePluginVO.getId())) {
            LOG.error("namespace plugin is not existed");
            return false;
        }
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(PluginTransfer.INSTANCE.mapToData(namespacePluginVO))));

        List<SelectorData> selectorDataList = selectorService.findByPluginIdAndNamespaceId(namespacePluginVO.getPluginId(), namespacePluginVO.getNamespaceId());

        if (!CollectionUtils.isEmpty(selectorDataList)) {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.REFRESH, selectorDataList));

            List<String> selectorIdList = selectorDataList.stream().map(SelectorData::getId)
                    .collect(Collectors.toList());
            for (String selectorId : selectorIdList) {
                discoveryUpstreamService.refreshBySelectorId(selectorId);
            }
            List<RuleData> allRuleDataList = ruleService.findBySelectorIdList(selectorIdList);

            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.REFRESH, allRuleDataList));
        }
        return true;
    }
    
    @Override
    public boolean syncPluginData(final String namespaceId, final String pluginId) {
        
        NamespacePluginVO namespacePluginVO = namespacePluginService.findByNamespaceIdAndPluginId(namespaceId, pluginId);
        if (Objects.isNull(namespacePluginVO) || Objects.isNull(namespacePluginVO.getPluginId())) {
            LOG.error("namespace plugin is not existed");
            return false;
        }
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.PLUGIN, DataEventTypeEnum.UPDATE,
                Collections.singletonList(PluginTransfer.INSTANCE.mapToData(namespacePluginVO))));
        
        List<SelectorData> selectorDataList = selectorService.findByPluginIdAndNamespaceId(namespacePluginVO.getPluginId(), namespacePluginVO.getNamespaceId());
        
        if (!CollectionUtils.isEmpty(selectorDataList)) {
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.REFRESH, selectorDataList));
            
            List<String> selectorIdList = selectorDataList.stream().map(SelectorData::getId)
                    .collect(Collectors.toList());
            for (String selectorId : selectorIdList) {
                discoveryUpstreamService.refreshBySelectorId(selectorId);
            }
            List<RuleData> allRuleDataList = ruleService.findBySelectorIdList(selectorIdList);
            
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.REFRESH, allRuleDataList));
        }
        return true;
    }
}
