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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;
import javax.annotation.Resource;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.bean.UpstreamInstance;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.query.SelectorQuery;
import org.apache.shenyu.admin.model.vo.SelectorVO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.admin.service.manager.LoadServiceDocEntry;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.selector.CommonUpstream;
import org.apache.shenyu.common.enums.DataEventTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.common.utils.JsonUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

/**
 * Load Service Doc Entry.
 */
@Service
public class LoadServiceDocEntryImpl implements LoadServiceDocEntry {
    private static final Logger LOG = LoggerFactory.getLogger(LoadServiceDocEntryImpl.class);

    @Resource
    private SelectorService selectorService;

    @Resource
    private ServiceDocManagerImpl serviceDocManager;

    @Override
    public synchronized void loadApiDocument() {
        List<UpstreamInstance> serviceList = this.getAllClusterLastUpdateInstanceList();
        if (CollectionUtils.isEmpty(serviceList)) {
            LOG.info("loadApiDocument No service registered.");
            return;
        }
        final Set<UpstreamInstance> currentServices = new HashSet<>(serviceList);
        LOG.info("loadApiDocument  serviceList={}", JsonUtils.toJson(currentServices));
        serviceDocManager.pullApiDocument(currentServices);
    }

    @Override
    public void loadDocOnSelectorChanged(final List<SelectorData> changedList, final DataEventTypeEnum eventType) {
        if (Objects.nonNull(eventType) && (eventType == DataEventTypeEnum.CREATE || eventType == DataEventTypeEnum.UPDATE)) {
            List<UpstreamInstance> serviceList = this.getLastUpdateInstanceList(changedList);
            if (CollectionUtils.isEmpty(serviceList)) {
                LOG.info("loadApiDocument No service registered.");
                return;
            }
            final Set<UpstreamInstance> currentServices = new HashSet<>(serviceList);
            LOG.info("loadDocOnSelectorChanged serviceList={}", JsonUtils.toJson(currentServices));
            serviceDocManager.pullApiDocument(currentServices);
        }
    }

    private List<UpstreamInstance> getLastUpdateInstanceList(final List<SelectorData> changedList) {
        if (CollectionUtils.isEmpty(changedList)) {
            LOG.info("getLastUpdateInstanceList, changedList is empty.");
            return Collections.EMPTY_LIST;
        }
        return changedList.parallelStream()
            .map(service -> {
                return getClusterLastUpdateInstance(service);
            })
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
    }

    /**
     * Get the last started healthy instance of each cluster.
     *
     * @return List
     */
    private List<UpstreamInstance> getAllClusterLastUpdateInstanceList() {
        List<SelectorVO> clusterList = null;
        try {
            CommonPager<SelectorVO> commonPager = selectorService.listByPage(new SelectorQuery("5", null, new PageParameter(1, Integer.MAX_VALUE)));
            clusterList = commonPager.getDataList();
        } catch (Exception e) {
            LOG.error("getAllClusterLastUpdateInstanceList fail. error={}", e);
        }
        if (CollectionUtils.isEmpty(clusterList)) {
            LOG.info("getAllClusterLastUpdateInstanceList, Not loaded into available backend services.");
            return Collections.EMPTY_LIST;
        }
        return clusterList.parallelStream()
            .map(service -> {
                return getClusterLastUpdateInstance(service);
            })
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
    }

    private UpstreamInstance getClusterLastUpdateInstance(final SelectorVO selectorVO) {
        List<UpstreamInstance> allInstances = null;
        // Get service instance.
        String handle = selectorVO.getHandle();
        if (StringUtils.isNotEmpty(handle)) {
            allInstances = new ArrayList<>();
            try {
                List<CommonUpstream> upstreamList = this.convert(handle);
                for (CommonUpstream upstream : upstreamList) {
                    String[] upstreamUrlArr = upstream.getUpstreamUrl().split(":");
                    UpstreamInstance instance = new UpstreamInstance();
                    instance.setContextPath(selectorVO.getName());
                    instance.setIp(upstreamUrlArr[0]);
                    instance.setPort(Integer.parseInt(upstreamUrlArr[1]));
                    instance.setEnabled(selectorVO.getEnabled());
                    instance.setHealthy(true);
                    instance.setStartupTime(upstream.getTimestamp());
                    allInstances.add(instance);
                }
            } catch (Exception e) {
                LOG.error("Error getting cluster instance list. serviceName={} error={}", selectorVO.getName(), e);
                return null;
            }
        }

        return getClusterLastUpdateInstance(allInstances);
    }

    private UpstreamInstance getClusterLastUpdateInstance(final SelectorData selectorData) {
        if (!selectorData.getPluginId().equals("5")) {
            LOG.info("getClusterLastUpdateInstance. pluginNae={} does not support pulling API documents.", selectorData.getPluginName());
            return null;
        }
        List<UpstreamInstance> allInstances = null;
        // Get service instance.
        String handle = selectorData.getHandle();
        if (StringUtils.isNotEmpty(handle)) {
            allInstances = new ArrayList<>();
            try {
                List<CommonUpstream> upstreamList = this.convert(handle);
                for (CommonUpstream upstream : upstreamList) {
                    String[] upstreamUrlArr = upstream.getUpstreamUrl().split(":");
                    UpstreamInstance instance = new UpstreamInstance();
                    instance.setContextPath(selectorData.getName());
                    instance.setIp(upstreamUrlArr[0]);
                    instance.setPort(Integer.parseInt(upstreamUrlArr[1]));
                    instance.setEnabled(selectorData.getEnabled());
                    instance.setHealthy(true);
                    instance.setStartupTime(upstream.getTimestamp());
                    allInstances.add(instance);
                }
            } catch (Exception e) {
                LOG.error("Error getting cluster instance list. serviceName={} error={}", selectorData.getName(), e);
                return null;
            }
        }
        return getClusterLastUpdateInstance(allInstances);
    }

    private UpstreamInstance getClusterLastUpdateInstance(final List<UpstreamInstance> allInstances) {
        if (CollectionUtils.isEmpty(allInstances)) {
            return null;
        }
        return allInstances.stream()
            .filter(UpstreamInstance::isHealthy)
            .filter(Objects::nonNull)
            .max(Comparator.comparing(UpstreamInstance::getStartupTime))
            .orElse(null);
    }

    private List<CommonUpstream> convert(final String handle) {
        return GsonUtils.getInstance().fromList(handle, CommonUpstream.class);
    }

}
