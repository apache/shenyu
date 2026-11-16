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
import org.apache.commons.lang3.math.NumberUtils;
import org.apache.shenyu.admin.config.properties.InstanceRetentionProperties;
import org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent;
import org.apache.shenyu.admin.model.vo.InstanceDataVisualLineVO;
import org.apache.shenyu.admin.model.vo.InstanceDataVisualVO;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.admin.service.fencing.RegistrationFencingGuard;
import org.apache.shenyu.admin.utils.InstanceInfoUtils;
import org.apache.shenyu.common.concurrent.ShenyuThreadFactory;
import org.apache.shenyu.common.constant.InstanceTypeConstants;
import org.apache.shenyu.common.enums.InstanceStatusEnum;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import java.time.Clock;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.locks.ReentrantLock;
import java.util.stream.Collectors;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;

/**
 * This is the client check service.
 */
@Component
public class InstanceCheckService {

    private static final int MAX_HISTORY_SIZE = 20;

    private static final long DB_CLEANUP_INTERVAL_MS = TimeUnit.HOURS.toMillis(1);

    private static final Logger LOG = LoggerFactory.getLogger(InstanceCheckService.class);

    private ScheduledThreadPoolExecutor executor;

    private ScheduledThreadPoolExecutor syncExecutor;

    private final int scheduledTime;

    private final Clock clock;

    private ConcurrentHashMap<String, InstanceInfoVO> instanceHealthBeatInfo;

    private long instanceHeartBeatTimeOut;

    private long deleteTimeout;

    private InstanceInfoService instanceInfoService;

    private final RegistrationFencingGuard registrationFencingGuard;

    private final InstanceRetentionProperties instanceRetentionProperties;

    private final Map<Integer, Deque<Long>> stateHistoryMap;

    private final ReentrantLock syncLock = new ReentrantLock();

    private final Map<String, Map<Integer, Deque<Long>>> namespaceStateHistoryMap;

    private final Object historyLock = new Object();

    private volatile long lastDbCleanupTime;

    @Autowired
    public InstanceCheckService(final InstanceInfoService instanceInfoService,
                                final RegistrationFencingGuard registrationFencingGuard,
                                final InstanceRetentionProperties instanceRetentionProperties) {
        this(instanceInfoService, registrationFencingGuard, instanceRetentionProperties, Clock.systemDefaultZone());
    }

    // Visible for tests that need a controlled clock.
    InstanceCheckService(final InstanceInfoService instanceInfoService,
                         final RegistrationFencingGuard registrationFencingGuard,
                         final InstanceRetentionProperties instanceRetentionProperties, final Clock clock) {
        this.scheduledTime = 10;
        this.clock = clock;
        this.instanceHealthBeatInfo = new ConcurrentHashMap<>();
        this.instanceHeartBeatTimeOut = 1000 * 20;
        this.deleteTimeout = 1000 * 60;
        this.instanceInfoService = instanceInfoService;
        this.registrationFencingGuard = registrationFencingGuard;
        this.instanceRetentionProperties = instanceRetentionProperties;
        this.stateHistoryMap = new ConcurrentHashMap<>();
        this.namespaceStateHistoryMap = new HashMap<>();
    }

    /**
     * Set up.
     */
    public void setup() {
        closeExecutors();
        this.fetchInstanceData();
        executor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-instance-heartbeat-task", false));
        executor.scheduleWithFixedDelay(this::scheduled, 30, scheduledTime, TimeUnit.SECONDS);
        syncExecutor = new ScheduledThreadPoolExecutor(1, ShenyuThreadFactory.create("scheduled-instance-db-sync-task", false));
        syncExecutor.scheduleWithFixedDelay(this::syncDBSafely, 40, scheduledTime, TimeUnit.SECONDS);
    }

    private void closeExecutors() {
        if (Objects.nonNull(executor)) {
            executor.shutdown();
        }
        if (Objects.nonNull(syncExecutor)) {
            syncExecutor.shutdown();
        }
    }

    /**
     * fetch instance status data from db.
     */
    public void fetchInstanceData() {
        List<InstanceInfoVO> list = instanceInfoService.list();
        list.forEach(instanceInfoVO -> {
            if (Objects.isNull(instanceInfoVO.getInstanceState())) {
                instanceInfoVO.setInstanceState(InstanceStatusEnum.DELETED.getCode());
            }
            if (isProtectedClientObservation(instanceInfoVO.getNamespaceId(), instanceInfoVO.getInstanceIp(),
                    instanceInfoVO.getInstancePort(), instanceInfoVO.getInstanceType())) {
                return;
            }
            String instanceKey = getInstanceKey(instanceInfoVO);
            // a persisted observation is older than any heartbeat already accepted in the live map
            // (heartbeat time is not persisted), so only fill identities that have no live entry
            instanceHealthBeatInfo.putIfAbsent(instanceKey, instanceInfoVO);
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

    /**
     * Fenced business addresses are excluded from the address-only diagnostic cache.
     *
     * @param namespaceId namespace id
     * @param instanceIp instance ip
     * @param instancePort instance port
     * @param instanceType instance type
     * @return whether the observation belongs to a protected business target
     */
    private boolean isProtectedClientObservation(final String namespaceId, final String instanceIp,
                                                 final String instancePort, final String instanceType) {
        if (!InstanceTypeConstants.CLIENT_INSTANCE_TYPE.equals(instanceType)) {
            return false;
        }
        int port = NumberUtils.toInt(instancePort, -1);
        return port > 0 && registrationFencingGuard.isProtectedAddress(namespaceId, instanceIp, port);
    }

    public boolean handleBeatInfo(final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        // every in-repo producer defaults a blank namespace, so treat it as the default namespace
        // rather than an incomplete identity (review feedback on #7776)
        instanceBeatInfoDTO.setNamespaceId(StringUtils.defaultIfBlank(instanceBeatInfoDTO.getNamespaceId(), SYS_DEFAULT_NAMESPACE_ID));
        if (isBlankIdentity(instanceBeatInfoDTO)) {
            LOG.warn("ignored heartbeat report with incomplete identity: ip:{}, port:{}, type:{}, namespace:{}",
                    instanceBeatInfoDTO.getInstanceIp(), instanceBeatInfoDTO.getInstancePort(),
                    instanceBeatInfoDTO.getInstanceType(), instanceBeatInfoDTO.getNamespaceId());
            return false;
        }
        if (isProtectedClientObservation(instanceBeatInfoDTO.getNamespaceId(), instanceBeatInfoDTO.getInstanceIp(),
                instanceBeatInfoDTO.getInstancePort(), instanceBeatInfoDTO.getInstanceType())) {
            // Fenced business instances are observed through their boot session, not the address cache.
            return true;
        }
        String instanceKey = getInstanceKey(instanceBeatInfoDTO);
        long currentTime = clock.millis();
        if (instanceHealthBeatInfo.containsKey(instanceKey)) {
            InstanceInfoVO instanceInfoVO = instanceHealthBeatInfo.get(instanceKey);
            instanceInfoVO.setLastHeartBeatTime(currentTime);
            if (StringUtils.isNotBlank(instanceBeatInfoDTO.getInstanceInfo())) {
                instanceInfoVO.setInstanceInfo(InstanceInfoUtils.normalize(instanceBeatInfoDTO.getInstanceInfo()));
            }
        } else {
            InstanceInfoVO instanceInfoVO = new InstanceInfoVO();
            instanceInfoVO.setInstanceIp(instanceBeatInfoDTO.getInstanceIp());
            instanceInfoVO.setInstanceState(InstanceStatusEnum.ONLINE.getCode());
            instanceInfoVO.setInstanceInfo(InstanceInfoUtils.normalize(instanceBeatInfoDTO.getInstanceInfo()));
            instanceInfoVO.setInstanceType(instanceBeatInfoDTO.getInstanceType());
            instanceInfoVO.setLastHeartBeatTime(currentTime);
            instanceInfoVO.setInstancePort(instanceBeatInfoDTO.getInstancePort());
            instanceInfoVO.setNamespaceId(instanceBeatInfoDTO.getNamespaceId());
            instanceHealthBeatInfo.put(instanceKey, instanceInfoVO);
        }
        return true;
    }

    private boolean isBlankIdentity(final InstanceBeatInfoDTO instanceBeatInfoDTO) {
        return StringUtils.isBlank(instanceBeatInfoDTO.getInstanceIp())
                || StringUtils.isBlank(instanceBeatInfoDTO.getInstancePort())
                || StringUtils.isBlank(instanceBeatInfoDTO.getInstanceType());
    }

    private void scheduled() {
        try {
            doCheck();
        } catch (Exception e) {
            LOG.error("upstream scheduled check error", e);
        }
        try {
            purgeExpiredInstances();
        } catch (Exception e) {
            LOG.error("instance retention cleanup error", e);
        }
    }

    private void doCheck() {
        long currentTime = clock.millis();
        instanceHealthBeatInfo.values().forEach(instance -> {
            if (currentTime - instance.getLastHeartBeatTime() > instanceHeartBeatTimeOut) {
                if (Objects.equals(InstanceStatusEnum.ONLINE.getCode(), instance.getInstanceState())) {
                    LOG.info("[instanceHealthInfo]namespace:{},type:{},Ip:{},Port:{} offline!",
                            instance.getNamespaceId(), instance.getInstanceType(), instance.getInstanceIp(), instance.getInstancePort());
                    instance.setInstanceState(InstanceStatusEnum.OFFLINE.getCode());
                }
            } else {
                LOG.info("[instanceHealthInfo]namespace:{},type:{},Ip:{},Port:{} online!",
                        instance.getNamespaceId(), instance.getInstanceType(), instance.getInstanceIp(), instance.getInstancePort());
                instance.setInstanceState(InstanceStatusEnum.ONLINE.getCode());
            }
            if (currentTime - instance.getLastHeartBeatTime() > deleteTimeout) {
                if (Objects.equals(InstanceStatusEnum.OFFLINE.getCode(), instance.getInstanceState())) {
                    LOG.info("[instanceHealthInfo]namespace:{},type:{},Ip:{},Port:{} deleted!",
                            instance.getNamespaceId(), instance.getInstanceType(), instance.getInstanceIp(), instance.getInstancePort());
                    instance.setInstanceState(InstanceStatusEnum.DELETED.getCode());
                }
            }
        });
        // sample the aggregated state once per scheduled cycle, after all instances are updated
        collectStateData();
    }

    /**
     * Remove instances that stayed in the DELETED state longer than the configured retention period.
     * The cleanup runs only on the node that owns the liveness checks (the master Admin in a cluster),
     * and the database delete is guarded by the same cutoff so a renewed identity is never removed.
     */
    void purgeExpiredInstances() {
        if (!instanceRetentionProperties.isEnabled()) {
            return;
        }
        long now = System.currentTimeMillis();
        long ttlMillis = instanceRetentionProperties.getTtl().toMillis();
        long cutoff = now - ttlMillis;
        if (cutoff < 0) {
            return;
        }
        int purgedCount = 0;
        // iterate the live map instead of copying it: ConcurrentHashMap iteration is weakly consistent,
        // and computeIfPresent re-checks the current value before removing the entry.
        for (Map.Entry<String, InstanceInfoVO> entry : instanceHealthBeatInfo.entrySet()) {
            InstanceInfoVO instance = entry.getValue();
            if (Objects.isNull(instance)) {
                continue;
            }
            String instanceKey = entry.getKey();
            AtomicBoolean purged = new AtomicBoolean(false);
            instanceHealthBeatInfo.computeIfPresent(instanceKey, (key, current) -> {
                if (shouldPurge(current, cutoff)) {
                    purged.set(true);
                    return null;
                }
                return current;
            });
            if (purged.get()) {
                purgedCount++;
                LOG.info("[instanceHealthInfo]namespace:{},type:{},Ip:{},Port:{} purged after retention ttl!",
                        instance.getNamespaceId(), instance.getInstanceType(), instance.getInstanceIp(), instance.getInstancePort());
            }
        }
        if (purgedCount > 0) {
            LOG.info("[instanceHealthInfo]{} expired instance record(s) removed from memory", purgedCount);
        }
        // The in-memory purge needs the liveness cadence, but the bulk delete does not:
        // run the database cleanup at most once per interval instead of on every check cycle.
        if (now - lastDbCleanupTime < DB_CLEANUP_INTERVAL_MS) {
            return;
        }
        lastDbCleanupTime = now;
        int deletedRows = instanceInfoService.deleteExpired(cutoff);
        if (deletedRows > 0) {
            LOG.info("[instanceHealthInfo]{} expired instance record(s) removed from database", deletedRows);
        }
    }

    private boolean shouldPurge(final InstanceInfoVO instance, final long cutoff) {
        return Objects.nonNull(instance.getInstanceState())
                && InstanceStatusEnum.DELETED.getCode() == instance.getInstanceState()
                && instance.getLastHeartBeatTime() < cutoff;
    }

    public void syncDB() {
        syncDB(0, TimeUnit.MILLISECONDS);
    }

    public boolean syncDB(final long timeout, final TimeUnit unit) {
        boolean locked = false;
        try {
            locked = timeout > 0 ? syncLock.tryLock(timeout, unit) : syncLock.tryLock();
            if (!locked) {
                LOG.info("instance syncDB is already running, skip");
                return false;
            }
            doSyncDB();
            return true;
        } catch (InterruptedException e) {
            LOG.warn("syncDB lock acquisition interrupted", e);
            Thread.currentThread().interrupt();
            return false;
        } finally {
            if (locked) {
                syncLock.unlock();
            }
        }
    }

    private void doSyncDB() {
        try {
            List<InstanceInfoVO> snapshot = new ArrayList<>(instanceHealthBeatInfo.values());
            for (InstanceInfoVO vo : snapshot) {
                if (isProtectedClientObservation(vo.getNamespaceId(), vo.getInstanceIp(),
                        vo.getInstancePort(), vo.getInstanceType())) {
                    continue;
                }
                try {
                    instanceInfoService.createOrUpdate(copyInstanceInfoVO(vo));
                } catch (Exception e) {
                    LOG.error("sync instance db error, ip: {}, port: {}", vo.getInstanceIp(), vo.getInstancePort(), e);
                }
            }
        } catch (Exception e) {
            LOG.error("sync instance info to db error", e);
        }
    }

    private InstanceInfoVO copyInstanceInfoVO(final InstanceInfoVO vo) {
        InstanceInfoVO copy = new InstanceInfoVO();
        copy.setId(vo.getId());
        copy.setInstanceIp(vo.getInstanceIp());
        copy.setInstancePort(vo.getInstancePort());
        copy.setInstanceType(vo.getInstanceType());
        copy.setInstanceInfo(vo.getInstanceInfo());
        copy.setNamespaceId(vo.getNamespaceId());
        copy.setInstanceState(vo.getInstanceState());
        copy.setDateCreated(vo.getDateCreated());
        copy.setDateUpdated(vo.getDateUpdated());
        copy.setLastHeartBeatTime(vo.getLastHeartBeatTime());
        return copy;
    }

    private void syncDBSafely() {
        try {
            syncDB();
        } catch (Exception e) {
            LOG.error("sync instance info to database error", e);
        }
    }

    /**
     * Close relative resource on container destroy.
     */
    @PreDestroy
    public void close() {
        try {
            closeExecutors();
            if (!syncDB(5, TimeUnit.SECONDS)) {
                LOG.warn("timed out waiting for instance db sync lock during shutdown, skipping final sync");
            }
        } finally {
            instanceHealthBeatInfo.clear();
        }
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
        final List<InstanceInfoVO> instances =
                new ArrayList<>(instanceHealthBeatInfo.values());

        final Map<Integer, Long> globalCounts = new HashMap<>();
        final Map<String, Map<Integer, Long>> namespaceCounts = new HashMap<>();

        for (InstanceInfoVO instance : instances) {
            final String namespaceId = instance.getNamespaceId();
            Map<Integer, Long> counts = null;

            if (StringUtils.isNotBlank(namespaceId)) {
                counts = namespaceCounts.computeIfAbsent(
                        namespaceId, key -> new HashMap<>());
            }

            final Integer state = instance.getInstanceState();
            if (Objects.nonNull(state)) {
                globalCounts.merge(state, 1L, Long::sum);
                if (Objects.nonNull(counts)) {
                    counts.merge(state, 1L, Long::sum);
                }
            }
        }

        synchronized (historyLock) {
            updateStateHistory(stateHistoryMap, globalCounts);

            namespaceStateHistoryMap.keySet().retainAll(namespaceCounts.keySet());

            namespaceCounts.forEach((namespaceId, counts) -> {
                final Map<Integer, Deque<Long>> history =
                        namespaceStateHistoryMap.computeIfAbsent(
                                namespaceId, key -> new HashMap<>());
                updateStateHistory(history, counts);
            });
        }
    }

    public InstanceDataVisualVO getInstanceDataVisual(final String namespaceId) {
        InstanceDataVisualVO instanceDataVisualVO = new InstanceDataVisualVO();
        List<InstanceInfoVO> instanceInfoVOS = instanceHealthBeatInfo.values().stream().toList();
        if (StringUtils.isNotBlank(namespaceId)) {
            instanceInfoVOS = instanceInfoVOS.stream().filter(vo -> namespaceId.equals(vo.getNamespaceId())).collect(Collectors.toList());
        }
        Map<Integer, Long> pieData = instanceInfoVOS.stream()
                .filter(instance -> Objects.nonNull(instance.getInstanceState()))
                .collect(Collectors.groupingBy(InstanceInfoVO::getInstanceState, Collectors.counting()));
        final List<InstanceDataVisualLineVO> lineList = new ArrayList<>();

        synchronized (historyLock) {
            final Map<Integer, Deque<Long>> history =
                    StringUtils.isBlank(namespaceId)
                            ? stateHistoryMap
                            : namespaceStateHistoryMap.get(namespaceId);

            for (Integer state : Arrays.asList(0, 1, 2)) {
                final Deque<Long> queue =
                        Objects.isNull(history) ? null : history.get(state);

                final List<Long> data = Objects.isNull(queue)
                        ? new ArrayList<>(MAX_HISTORY_SIZE)
                        : new ArrayList<>(queue);

                while (data.size() < MAX_HISTORY_SIZE) {
                    data.add(0, 0L);
                }

                lineList.add(new InstanceDataVisualLineVO(
                        InstanceStatusEnum.getNameByCode(state), data));
            }
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

    private void updateStateHistory(final Map<Integer, Deque<Long>> history,
                                    final Map<Integer, Long> currentData) {
        for (Integer state : Arrays.asList(0, 1, 2)) {
            final Deque<Long> queue = history.computeIfAbsent(
                    state, key -> new ArrayDeque<>(MAX_HISTORY_SIZE));

            queue.addLast(currentData.getOrDefault(state, 0L));

            while (queue.size() > MAX_HISTORY_SIZE) {
                queue.removeFirst();
            }
        }
    }

}
