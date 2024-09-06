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

package org.apache.shenyu.admin.service.manager.impl;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.DiscoveryUpstreamService;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.apache.shenyu.admin.service.manager.PullSwaggerDocService;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * Load Service Doc Entry.
 */
@Service
public class LoadServiceDocEntryImpl implements LoadServiceDocEntry {
    private static final Logger LOG = LoggerFactory.getLogger(LoadServiceDocEntryImpl.class);

    @SuppressWarnings("unchecked")
    private static Set<String> supportSwaggerPluginSet = Collections.emptySet();

    private final SelectorService selectorService;

    private final DiscoveryUpstreamService discoveryUpstreamService;

    private final PluginMapper pluginMapper;

    private final PullSwaggerDocService pullSwaggerDocService;

    private final ShenyuDictService shenyuDictService;

    public LoadServiceDocEntryImpl(final SelectorService selectorService,
                                   final DiscoveryUpstreamService discoveryUpstreamService,
                                   final PluginMapper pluginMapper,
                                   final PullSwaggerDocService pullSwaggerDocService,
                                   final ShenyuDictService shenyuDictService) {
        this.selectorService = selectorService;
        this.discoveryUpstreamService = discoveryUpstreamService;
        this.pluginMapper = pluginMapper;
        this.pullSwaggerDocService = pullSwaggerDocService;
        this.shenyuDictService = shenyuDictService;
    }

    @Override
    public synchronized void loadApiDocument() {
        if (!isEnabledLoad()) {
            return;
        }
        List<UpstreamInstance> serviceList = this.getAllClusterLastUpdateInstanceList();
        if (CollectionUtils.isEmpty(serviceList)) {
            LOG.info("load api document No service registered.");
            return;
        }
        final Set<UpstreamInstance> currentServices = new HashSet<>(serviceList);
        LOG.info("load api document, serviceList={}", JsonUtils.toJson(currentServices));
        pullSwaggerDocService.pullApiDocument(currentServices);
    }

    @Override
    public void loadDocOnUpstreamChanged(final List<DiscoverySyncData> discoverySyncDataList, final DataEventTypeEnum eventType) {
        if (Objects.nonNull(eventType) && (eventType == DataEventTypeEnum.CREATE || eventType == DataEventTypeEnum.UPDATE)) {
            List<UpstreamInstance> serviceList = this.getLastUpdateInstanceList(discoverySyncDataList);
            if (CollectionUtils.isEmpty(serviceList)) {
                LOG.info("load api document, no service registered.");
                return;
            }
            if (!isEnabledLoad()) {
                return;
            }
            final Set<UpstreamInstance> currentServices = new HashSet<>(serviceList);
            LOG.info("loadDocOnSelectorChanged, serviceList={}", JsonUtils.toJson(currentServices));
            pullSwaggerDocService.pullApiDocument(currentServices);
        }
    }

    private boolean isEnabledLoad() {
        ShenyuDictVO shenyuInitData = shenyuDictService.findByDictCodeName(AdminConstants.DICT_API_DOC_FLAG_DICTCODE, AdminConstants.DICT_API_DOC_FLAG_DICTNAME);
        if (Objects.nonNull(shenyuInitData) && Boolean.TRUE.toString().equals(shenyuInitData.getDictValue())) {
            return true;
        }
        LOG.info("load api document global switch is close.");
        return false;
    }

    private List<UpstreamInstance> getLastUpdateInstanceList(final List<DiscoverySyncData> discoverySyncDataList) {
        if (CollectionUtils.isEmpty(discoverySyncDataList)) {
            LOG.info("getLastUpdateInstanceList, changedList is empty.");
            return Collections.emptyList();
        }
        return discoverySyncDataList.parallelStream()
                .map(this::getClusterLastUpdateInstance)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    private List<UpstreamInstance> getInstances2(final List<DiscoveryUpstreamData> discoveryUpstreamDataList, final String contextPath) {
        List<UpstreamInstance> allInstances = null;
        // Get service instance.
        if (CollectionUtils.isNotEmpty(discoveryUpstreamDataList)) {
            allInstances = new ArrayList<>();
            try {
                allInstances = discoveryUpstreamDataList.stream().map(discoveryUpstreamData -> {
                    String[] upstreamUrlArr = discoveryUpstreamData.getUrl().split(":");
                    UpstreamInstance instance = new UpstreamInstance();
                    instance.setContextPath(contextPath);
                    instance.setEnabled(true);
                    instance.setIp(upstreamUrlArr[0]);
                    instance.setPort(upstreamUrlArr.length == 1 ? 80 : Integer.parseInt(upstreamUrlArr[1]));
                    instance.setHealthy(true);
                    Long startupTime = Optional.ofNullable(discoveryUpstreamData.getDateCreated()).map(Timestamp::getTime).orElse(System.currentTimeMillis());
                    instance.setStartupTime(startupTime);
                    return instance;
                }).collect(Collectors.toList());
            } catch (Exception e) {
                LOG.error("Error getting cluster instance list. contextPath={} error", contextPath, e);
                return Collections.emptyList();
            }
        }
        return allInstances;
    }

    /**
     * Get the last started healthy instance of each cluster.
     *
     * @return List
     */
    private List<UpstreamInstance> getAllClusterLastUpdateInstanceList() {
        List<String> pluginNames = RpcTypeEnum.acquireSupportSwaggers().stream()
                .map(rpcTypeEnum -> PluginNameAdapter.rpcTypeAdapter(rpcTypeEnum.getName()))
                .collect(Collectors.toList());
        final List<PluginDO> pluginDOList = pluginMapper.selectByNames(pluginNames);
        if (CollectionUtils.isEmpty(pluginDOList)) {
            return Collections.emptyList();
        }
        supportSwaggerPluginSet = new HashSet<>(pluginNames);
        List<String> pluginIds = pluginDOList.stream().map(PluginDO::getId).collect(Collectors.toList());
        CommonPager<SelectorVO> commonPager = selectorService.listByPage(new SelectorQuery(pluginIds, null, new PageParameter(1, Integer.MAX_VALUE), SYS_DEFAULT_NAMESPACE_ID));
        List<SelectorVO> clusterList = commonPager.getDataList();
        if (CollectionUtils.isEmpty(clusterList)) {
            LOG.info("getAllClusterLastUpdateInstanceList. Not loaded into available backend services.");
            return Collections.emptyList();
        }
        return clusterList.parallelStream()
                .map(this::getClusterLastUpdateInstance)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    private UpstreamInstance getClusterLastUpdateInstance(final SelectorVO selectorVO) {
        List<UpstreamInstance> allInstances = getInstances(selectorVO.getId(), selectorVO.getName(), selectorVO.getEnabled());
        if (CollectionUtils.isEmpty(allInstances)) {
            return null;
        }
        return getClusterLastUpdateInstance(allInstances);
    }

    private UpstreamInstance getClusterLastUpdateInstance(final DiscoverySyncData discoverySyncData) {
        if (!supportSwaggerPluginSet.contains(discoverySyncData.getPluginName())) {
            LOG.info("getClusterLastUpdateInstance. pluginName={} does not support pulling API documents.", discoverySyncData.getPluginName());
            return null;
        }
        List<UpstreamInstance> allInstances = getInstances2(discoverySyncData.getUpstreamDataList(), discoverySyncData.getSelectorName());
        if (Objects.isNull(allInstances)) {
            return null;
        }
        return getClusterLastUpdateInstance(allInstances);
    }

    private UpstreamInstance getClusterLastUpdateInstance(final List<UpstreamInstance> allInstances) {
        if (CollectionUtils.isEmpty(allInstances)) {
            return null;
        }
        return allInstances.stream()
                .filter(Objects::nonNull)
                .filter(UpstreamInstance::isHealthy)
                .max(Comparator.comparing(UpstreamInstance::getStartupTime))
                .orElse(null);
    }

    private List<UpstreamInstance> getInstances(final String selectorId, final String contextPath, final boolean enabled) {
        List<UpstreamInstance> allInstances = null;
        List<DiscoveryUpstreamData> discoveryUpstreamDataList = discoveryUpstreamService.findBySelectorId(selectorId);
        // Get service instance.
        if (CollectionUtils.isNotEmpty(discoveryUpstreamDataList)) {
            allInstances = new ArrayList<>();
            try {
                allInstances = discoveryUpstreamDataList.stream().map(discoveryUpstreamData -> {
                    String[] upstreamUrlArr = discoveryUpstreamData.getUrl().split(":");
                    UpstreamInstance instance = new UpstreamInstance();
                    instance.setContextPath(contextPath);
                    instance.setEnabled(enabled);
                    instance.setIp(upstreamUrlArr[0]);
                    instance.setPort(upstreamUrlArr.length == 1 ? 80 : Integer.parseInt(upstreamUrlArr[1]));
                    instance.setHealthy(true);
                    Long startupTime = Optional.ofNullable(discoveryUpstreamData.getDateCreated()).map(Timestamp::getTime).orElse(System.currentTimeMillis());
                    instance.setStartupTime(startupTime);
                    return instance;
                }).collect(Collectors.toList());
            } catch (Exception e) {
                LOG.error("Error getting cluster instance list. contextPath={} error", contextPath, e);
                return Collections.emptyList();
            }
        }
        return allInstances;
    }

}
