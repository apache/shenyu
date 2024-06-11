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

package org.apache.shenyu.admin.mode.cluster.impl.jdbc;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.mapper.ClusterMasterMapper;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.model.entity.ClusterMasterDO;
import org.apache.shenyu.admin.transfer.ClusterMasterTransfer;
import org.apache.shenyu.common.timer.Timer;
import org.apache.shenyu.common.timer.TimerTask;
import org.apache.shenyu.common.timer.WheelTimerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.integration.jdbc.lock.JdbcLockRegistry;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.concurrent.locks.Lock;

public class ClusterSelectMasterServiceJdbcImpl implements ClusterSelectMasterService {
    
    private static final Logger LOG = LoggerFactory.getLogger(ClusterSelectMasterServiceJdbcImpl.class);
    
    private static final String MASTER_LOCK_KEY = "shenyu:cluster:master";
    
    private static final String MASTER_ID = "1";
    
    private final ClusterProperties clusterProperties;
    
    private final JdbcLockRegistry jdbcLockRegistry;
    
    private final ClusterMasterMapper clusterMasterMapper;
    
    private final Lock clusterMasterLock;
    
    private final Timer timer;
    
    private TimerTask timerTask;
    
    private volatile boolean locked;
    
    public ClusterSelectMasterServiceJdbcImpl(final ClusterProperties clusterProperties,
                                              final JdbcLockRegistry jdbcLockRegistry,
                                              final ClusterMasterMapper clusterMasterMapper) {
        this.clusterProperties = clusterProperties;
        this.jdbcLockRegistry = jdbcLockRegistry;
        this.clusterMasterMapper = clusterMasterMapper;
        this.clusterMasterLock = jdbcLockRegistry.obtain(MASTER_LOCK_KEY);
        this.timer = WheelTimerFactory.getSharedTimer();
    }
    
    @Override
    public boolean selectMaster() {
        locked = clusterMasterLock.tryLock();
        LOG.info("select master result: {}", locked);
        return locked;
    }
    
    @Override
    public boolean selectMaster(final String masterHost, final String masterPort, final String contextPath) {
        locked = clusterMasterLock.tryLock();
        if (locked) {
            Timestamp now = Timestamp.valueOf(LocalDateTime.now());
            ClusterMasterDO masterDO = ClusterMasterDO.builder()
                    .id(MASTER_ID)
                    .masterHost(masterHost)
                    .masterPort(masterPort)
                    .contextPath(contextPath)
                    .dateCreated(now)
                    .dateUpdated(now)
                    .build();
            try {
                clusterMasterMapper.insert(masterDO);
            } catch (Exception e) {
                clusterMasterMapper.updateSelective(masterDO);
            }
        }
        return locked;
    }
    
    private void healthCheck() {
        try {
            if (!locked) {
                //                this.reconnectBlocking();
                locked = this.selectMaster();
            } else {
                //                this.sendPing();
                //                send(DataEventTypeEnum.CLUSTER.name());
                jdbcLockRegistry.renewLock(MASTER_LOCK_KEY);
                LOG.info("renew master");
            }
        } catch (Exception e) {
            LOG.error("master health check error", e);
        }
    }
    
    @Override
    public boolean checkMasterStatus() throws IllegalStateException {
        if (locked) {
            jdbcLockRegistry.renewLock(MASTER_LOCK_KEY);
        }
        return locked;
    }
    
    @Override
    public boolean releaseMaster() {
        if (locked) {
            clusterMasterLock.unlock();
            locked = false;
            timerTask.cancel();
        }
        return true;
    }
    
    @Override
    public boolean isMaster() {
        if (!clusterProperties.isEnabled()) {
            return true;
        }
        return locked;
    }
    
    @Override
    public ClusterMasterDTO getMaster() {
        ClusterMasterDO masterDO = clusterMasterMapper.selectById(MASTER_ID);
        return Objects.isNull(masterDO) ? new ClusterMasterDTO() : ClusterMasterTransfer.INSTANCE.mapToDTO(masterDO);
    }
    
    @Override
    public String getMasterUrl() {
        ClusterMasterDO master = clusterMasterMapper.selectById(MASTER_ID);
        if (StringUtils.isEmpty(master.getContextPath())) {
            return "http://" + master.getMasterHost() + ":" + master.getMasterPort();
        }
        return "http://" + master.getMasterHost() + ":" + master.getMasterPort() + "/" + master.getContextPath();
    }
}
