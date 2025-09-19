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

import jakarta.annotation.PreDestroy;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent;
import org.apache.shenyu.admin.model.vo.InstanceDataVisualLineVO;
import org.apache.shenyu.admin.model.vo.InstanceDataVisualVO;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.enums.InstanceStatusEnum;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedDeque;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * This is the client check service.
 */
@Component
public class InstanceCheckService {

    private static final int MAX_HISTORY_SIZE = 20;

    private static final Logger LOG = LoggerFactory.getLogger(InstanceCheckService.class);

    private ScheduledThreadPoolExecutor executor;

    private final int scheduledTime;

    private ConcurrentHashMap<String, InstanceInfoVO> instanceHealthBeatInfo;

    private long instanceHeartBeatTimeOut;

    private long deleteTimeout;

    private InstanceInfoService instanceInfoService;

    private final Map<Integer, Deque<Long>> stateHistoryMap;

    public InstanceCheckService(final InstanceInfoService instanceInfoService) {
        this.scheduledTime = 10;
        this.instanceHealthBeatInfo = new ConcurrentHashMap<>();
        this.instanceHeartBeatTimeOut = 1000 * 20;
        this.deleteTimeout = 1000 * 60;
        this.instanceInfoService = instanceInfoService;
        this.stateHistoryMap = new ConcurrentHashMap<>();
    }

    /**
     * Set up.
     */
    public void setup() {
        this.fetchInstanceData();
        executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-instance-heartbeat-task", false));
        executor.scheduleWithFixedDelay(this::scheduled, 30, scheduledTime, TimeUnit.SECONDS);
        executor.scheduleWithFixedDelay(this::syncDB, 40, scheduledTime, TimeUnit.SECONDS);
    }

    /**
     * fetch instance status data from db.
     */
    public void fetchInstanceData() {
        List<InstanceInfoVO> list = instanceInfoService.list();
        list.forEach(instanceInfoVO -> {
            String instanceKey = getInstanceKey(instanceInfoVO);
            instanceHealthBeatInfo.put(instanceKey, instanceInfoVO);
        });
    }

    public String getInstanceKey(final InstanceInfoVO instanceInfoVO) {
        return instanceInfoVO.getInstanceIp() + ":" + instanceInfoVO.getInstancePort() + "@" + instanceInfoVO.getInstanceType() + "#" + instanceInfoVO.getNamespaceId();
    }

    public String getInstanceKey(final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        return instanceBeatInfoDTO.getInstanceIp() + ":" + instanceBeatInfoDTO.getInstancePort() + "@" + instanceBeatInfoDTO.getInstanceType() + "#" + instanceBeatInfoDTO.getNamespaceId();
    }

    public InstanceInfoVO getInstanceHealthBeatInfo(final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        return instanceHealthBeatInfo.get(getInstanceKey(instanceBeatInfoDTO));
    }

    public InstanceInfoVO getInstanceHealthBeatInfo(final String instanceKey) {
        return instanceHealthBeatInfo.get(instanceKey);
    }

    public void handleBeatInfo(final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        String instanceKey = getInstanceKey(instanceBeatInfoDTO);
        if (instanceHealthBeatInfo.containsKey(instanceKey)) {
            InstanceInfoVO instanceInfoVO = instanceHealthBeatInfo.get(instanceKey);
            instanceInfoVO.setLastHeartBeatTime(System.currentTimeMillis());
        } else {
            InstanceInfoVO instanceInfoVO = new InstanceInfoVO();
            instanceInfoVO.setInstanceIp(instanceBeatInfoDTO.getInstanceIp());
            instanceInfoVO.setInstanceState(1);
            instanceInfoVO.setInstanceInfo(instanceBeatInfoDTO.getInstanceInfo());
            instanceInfoVO.setInstanceType(instanceBeatInfoDTO.getInstanceType());
            instanceInfoVO.setLastHeartBeatTime(System.currentTimeMillis());
            instanceInfoVO.setInstancePort(instanceBeatInfoDTO.getInstancePort());
            instanceInfoVO.setNamespaceId(instanceBeatInfoDTO.getNamespaceId());
            instanceInfoVO.setLastHeartBeatTime(System.currentTimeMillis());
            instanceHealthBeatInfo.put(instanceKey, instanceInfoVO);
        }
    }

    private void scheduled() {
        try {
            doCheck();
        } catch (Exception e) {
            LOG.error("upstream scheduled check error", e);
        }
    }

    private void doCheck() {
        instanceHealthBeatInfo.values().forEach(instance -> {
            if (System.currentTimeMillis() - instance.getLastHeartBeatTime() > instanceHeartBeatTimeOut) {
                if (InstanceStatusEnum.ONLINE.getCode() == instance.getInstanceState()) {
                    LOG.info("[instanceHealthInfo]namespace:{},type:{},Ip:{},Port:{} offline!",
                            instance.getNamespaceId(), instance.getInstanceType(), instance.getInstanceIp(), instance.getInstancePort());
                    instance.setInstanceState(InstanceStatusEnum.OFFLINE.getCode());
                }
            } else {
                LOG.info("[instanceHealthInfo]namespace:{},type:{},Ip:{},Port:{} online!",
                        instance.getNamespaceId(), instance.getInstanceType(), instance.getInstanceIp(), instance.getInstancePort());
                instance.setInstanceState(InstanceStatusEnum.ONLINE.getCode());
            }
            if (System.currentTimeMillis() - instance.getLastHeartBeatTime() > deleteTimeout) {
                if (InstanceStatusEnum.OFFLINE.getCode() == instance.getInstanceState()) {
                    LOG.info("[instanceHealthInfo]namespace:{},type:{},Ip:{},Port:{} deleted!",
                            instance.getNamespaceId(), instance.getInstanceType(), instance.getInstanceIp(), instance.getInstancePort());
                    instance.setInstanceState(InstanceStatusEnum.DELETED.getCode());
                }
            }
            collectStateData();
        });
    }

    public void syncDB() {
        instanceHealthBeatInfo.values().forEach(vo -> {
            instanceInfoService.createOrUpdate(vo);
        });
    }

    /**
     * Close relative resource on container destroy.
     */
    @PreDestroy
    public void close() {
        syncDB();
        instanceHealthBeatInfo.clear();
        executor.shutdown();
    }

    /**
     * listen {@link InstanceInfoReportEvent} instance info report event.
     *
     * @param event event
     */
    @EventListener(InstanceInfoReportEvent.class)
    public void onInstanceInfoReport(final InstanceInfoReportEvent event) {
        InstanceBeatInfoDTO instanceBeatInfoDTO = buildInstanceInfoDTO(event);
        handleBeatInfo(instanceBeatInfoDTO);
    }

    private InstanceBeatInfoDTO buildInstanceInfoDTO(final InstanceInfoReportEvent instanceInfoRegisterDTO) {
        InstanceBeatInfoDTO instanceInfoDTO = new InstanceBeatInfoDTO();
        instanceInfoDTO.setInstanceIp(instanceInfoRegisterDTO.getInstanceIp());
        instanceInfoDTO.setInstancePort(instanceInfoRegisterDTO.getInstancePort());
        instanceInfoDTO.setInstanceType(instanceInfoRegisterDTO.getInstanceType());
        instanceInfoDTO.setInstanceInfo(instanceInfoRegisterDTO.getInstanceInfo());
        instanceInfoDTO.setNamespaceId(instanceInfoRegisterDTO.getNamespaceId());
        return instanceInfoDTO;
    }

    private void collectStateData() {
        if (!CollectionUtils.isEmpty(instanceHealthBeatInfo)) {
            Map<Integer, Long> pieData = instanceHealthBeatInfo.values().stream().collect(Collectors.groupingBy(InstanceInfoVO::getInstanceState, Collectors.counting()));
            updateStateHistory(pieData);
        }
    }

    public InstanceDataVisualVO getInstanceDataVisual(final String namespaceId) {
        InstanceDataVisualVO instanceDataVisualVO = new InstanceDataVisualVO();
        List<InstanceInfoVO> instanceInfoVOS = instanceHealthBeatInfo.values().stream().toList();
        if (StringUtils.isNotBlank(namespaceId)) {
            instanceInfoVOS = instanceInfoVOS.stream().filter(vo -> namespaceId.equals(vo.getNamespaceId())).collect(Collectors.toList());
        }
        Map<Integer, Long> pieData = instanceInfoVOS.stream().collect(Collectors.groupingBy(InstanceInfoVO::getInstanceState, Collectors.counting()));
        List<InstanceDataVisualLineVO> lineList = new ArrayList<>();
        for (Integer state : Arrays.asList(0, 1, 2)) {
            Deque<Long> queue = stateHistoryMap.getOrDefault(state, new ArrayDeque<>(MAX_HISTORY_SIZE));
            List<Long> data = new ArrayList<>(queue);
            while (data.size() < MAX_HISTORY_SIZE) {
                data.add(0, 0L);
            }
            InstanceDataVisualLineVO dto = new InstanceDataVisualLineVO(
                    InstanceStatusEnum.getNameByCode(state),
                    data
            );
            lineList.add(dto);
        }
        List<InstanceDataVisualVO.Entry> pieDataList = pieData.entrySet().stream()
                .map(entry -> {
                    Integer stateCode = entry.getKey();
                    String stateName = InstanceStatusEnum.getNameByCode(stateCode);
                    return new InstanceDataVisualVO.Entry(stateName, entry.getValue());
                })
                .collect(Collectors.toList());
        instanceDataVisualVO.setPieData(pieDataList);
        instanceDataVisualVO.setLineData(lineList);
        return instanceDataVisualVO;
    }

    private void updateStateHistory(final Map<Integer, Long> currentData) {
        ensureStateQueues();
        for (Integer state : Arrays.asList(0, 1, 2)) {
            Long count = currentData.getOrDefault(state, 0L);
            Deque<Long> queue = stateHistoryMap.get(state);
            queue.addLast(count);
            while (queue.size() > MAX_HISTORY_SIZE) {
                queue.removeFirst();
            }
        }
    }

    private void ensureStateQueues() {
        for (Integer state : Arrays.asList(0, 1, 2)) {
            stateHistoryMap.putIfAbsent(state, new ConcurrentLinkedDeque<>());
        }
    }
}
