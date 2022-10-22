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

import com.alibaba.nacos.shaded.com.google.common.collect.Lists;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.service.converter.SelectorHandleConverterFactor;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.apache.shenyu.admin.service.manager.ServiceDocManager;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.common.utils.PluginNameAdapter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * Load Service Doc Entry.
 */
@Service
public class LoadServiceDocEntryImpl implements LoadServiceDocEntry {
    private static final Logger LOG = LoggerFactory.getLogger(LoadServiceDocEntryImpl.class);

    @SuppressWarnings("unchecked")
    private static Map<String, String> supportSwaggerPluginMap = Collections.EMPTY_MAP;

    private final SelectorService selectorService;

    private final SelectorHandleConverterFactor converterFactor;

    private final PluginMapper pluginMapper;

    private final ServiceDocManager serviceDocManager;

    private final ShenyuDictService shenyuDictService;

    public LoadServiceDocEntryImpl(final SelectorService selectorService,
                                   final SelectorHandleConverterFactor converterFactor,
                                   final PluginMapper pluginMapper,
                                   final ServiceDocManager serviceDocManager,
                                   final ShenyuDictService shenyuDictService) {
        this.selectorService = selectorService;
        this.converterFactor = converterFactor;
        this.pluginMapper = pluginMapper;
        this.serviceDocManager = serviceDocManager;
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
        LOG.info("load api document  serviceList={}", JsonUtils.toJson(currentServices));
        serviceDocManager.pullApiDocument(currentServices);
    }

    @Override
    public void loadDocOnSelectorChanged(final List<SelectorData> changedList, final DataEventTypeEnum eventType) {
        if (Objects.nonNull(eventType) && (eventType == DataEventTypeEnum.CREATE || eventType == DataEventTypeEnum.UPDATE)) {
            List<UpstreamInstance> serviceList = this.getLastUpdateInstanceList(changedList);
            if (CollectionUtils.isEmpty(serviceList)) {
                LOG.info("load api document, no service registered.");
                return;
            }
            if (!isEnabledLoad()) {
                return;
            }
            final Set<UpstreamInstance> currentServices = new HashSet<>(serviceList);
            LOG.info("loadDocOnSelectorChanged serviceList={}", JsonUtils.toJson(currentServices));
            serviceDocManager.pullApiDocument(currentServices);
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

    private List<UpstreamInstance> getLastUpdateInstanceList(final List<SelectorData> changedList) {
        if (CollectionUtils.isEmpty(changedList)) {
            LOG.info("getLastUpdateInstanceList, changedList is empty.");
            return Collections.emptyList(); 
        }
        return changedList.parallelStream()
            .map(this::getClusterLastUpdateInstance)
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
    }

    /**
     * Get the last started healthy instance of each cluster.
     *
     * @return List
     */
    private List<UpstreamInstance> getAllClusterLastUpdateInstanceList() {
        List<String> pluginNames = new ArrayList<>();
        RpcTypeEnum.acquireSupportSwaggers().forEach(rpcTypeEnum -> pluginNames.add(PluginNameAdapter.rpcTypeAdapter(rpcTypeEnum.getName())));
        final List<PluginDO> pluginDOList = pluginMapper.selectByNames(pluginNames);
        if (CollectionUtils.isEmpty(pluginDOList)) {
            return Collections.emptyList();
        }
        supportSwaggerPluginMap = pluginDOList.stream().filter(Objects::nonNull)
            .collect(Collectors.toMap(PluginDO::getId, PluginDO::getName, (value1, value2) -> value1));

        CommonPager<SelectorVO> commonPager = selectorService.listByPage(new SelectorQuery(Lists.newArrayList(supportSwaggerPluginMap.keySet()), null, new PageParameter(1, Integer.MAX_VALUE)));
        List<SelectorVO> clusterList = commonPager.getDataList();
        if (CollectionUtils.isEmpty(clusterList)) {
            LOG.info("getAllClusterLastUpdateInstanceList, Not loaded into available backend services.");
            return Collections.emptyList();
        }
        return clusterList.parallelStream()
            .map(this::getClusterLastUpdateInstance)
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
    }

    private UpstreamInstance getClusterLastUpdateInstance(final SelectorVO selectorVO) {
        List<UpstreamInstance> allInstances = getInstances(selectorVO.getPluginId(), selectorVO.getHandle(), selectorVO.getName(), selectorVO.getEnabled());
        if (CollectionUtils.isEmpty(allInstances)) {
            return null;
        }
        return getClusterLastUpdateInstance(allInstances);
    }

    private UpstreamInstance getClusterLastUpdateInstance(final SelectorData selectorData) {
        if (!supportSwaggerPluginMap.containsKey(selectorData.getPluginId())) {
            LOG.info("getClusterLastUpdateInstance. pluginNae={} does not support pulling API documents.", selectorData.getPluginName());
            return null;
        }
        List<UpstreamInstance> allInstances = getInstances(selectorData.getPluginId(), selectorData.getHandle(), selectorData.getName(), selectorData.getEnabled());
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

    private List<UpstreamInstance> getInstances(final String pluginId, final String handle, final String contextPath,
        final boolean enabled) {
        List<UpstreamInstance> allInstances = null;
        // Get service instance.
        if (StringUtils.isNotEmpty(handle)) {
            allInstances = new ArrayList<>();
            try {
                List<CommonUpstream> upstreamList = this.convert(pluginId, handle);
                for (CommonUpstream upstream : upstreamList) {
                    UpstreamInstance instance = new UpstreamInstance();
                    instance.setContextPath(contextPath);
                    String[] upstreamUrlArr = upstream.getUpstreamUrl().split(":");
                    instance.setIp(upstreamUrlArr[0]);
                    instance.setPort(upstreamUrlArr.length == 1 ? 80 : Integer.parseInt(upstreamUrlArr[1]));
                    instance.setEnabled(enabled);
                    instance.setHealthy(true);
                    instance.setStartupTime(upstream.getTimestamp());
                    allInstances.add(instance);
                }
            } catch (Exception e) {
                LOG.error("Error getting cluster instance list. contextPath={} error={}", contextPath, e);
                return null;
            }
        }
        return allInstances;
    }

    private List<CommonUpstream> convert(final String pluginId, final String handle) {
        String pluginName = supportSwaggerPluginMap.get(pluginId);
        return converterFactor.newInstance(pluginName).convertUpstream(handle)
            .stream().filter(CommonUpstream::isStatus)
            .collect(Collectors.toList());
    }

}
