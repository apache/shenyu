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
import org.apache.shenyu.admin.jpa.repository.ClusterMasterRepository;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.model.entity.ClusterMasterDO;
import org.apache.shenyu.admin.transfer.ClusterMasterTransfer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.integration.jdbc.lock.JdbcLockRegistry;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Objects;
import java.util.concurrent.locks.Lock;

/**
 * The cluster select master service jdbc impl.
 */
public class ClusterSelectMasterServiceJdbcImpl implements ClusterSelectMasterService {
    
    private static final Logger LOG = LoggerFactory.getLogger(ClusterSelectMasterServiceJdbcImpl.class);
    
    private static final String MASTER_LOCK_KEY = "shenyu_cluster_lock:master";
    
    private static final String MASTER_ID = "1";
    
    private final ClusterProperties clusterProperties;
    
    private final JdbcLockRegistry jdbcLockRegistry;
    
    private final ClusterMasterRepository clusterMasterRepository;
    
    private final Lock clusterMasterLock;
    
    private volatile boolean masterFlag;
    
    public ClusterSelectMasterServiceJdbcImpl(final ClusterProperties clusterProperties,
                                              final JdbcLockRegistry jdbcLockRegistry,
                                              final ClusterMasterRepository clusterMasterRepository) {
        this.clusterProperties = clusterProperties;
        this.jdbcLockRegistry = jdbcLockRegistry;
        this.clusterMasterRepository = clusterMasterRepository;
        this.clusterMasterLock = jdbcLockRegistry.obtain(MASTER_LOCK_KEY);
    }
    
    @Override
    public boolean selectMaster() {
        masterFlag = clusterMasterLock.tryLock();
        LOG.info("select master result: {}", masterFlag);
        return masterFlag;
    }
    
    @Override
    public boolean selectMaster(final String masterHost, final String masterPort, final String contextPath) {
        masterFlag = clusterMasterLock.tryLock();
        if (masterFlag) {
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
                clusterMasterRepository.save(masterDO);
            } catch (Exception e) {
                // the master row already exists, refresh it instead
                clusterMasterRepository.findById(MASTER_ID).ifPresent(persisted -> {
                    persisted.setMasterHost(masterHost);
                    persisted.setMasterPort(masterPort);
                    persisted.setContextPath(contextPath);
                    persisted.setDateUpdated(now);
                    clusterMasterRepository.save(persisted);
                });
            }
        }
        return masterFlag;
    }
    
    @Override
    public boolean checkMasterStatus() throws IllegalStateException {
        if (masterFlag) {
            try {
                jdbcLockRegistry.renewLock(MASTER_LOCK_KEY);
            } catch (IllegalStateException e) {
                masterFlag = false;
                throw e;
            }
        }
        return masterFlag;
    }
    
    @Override
    public boolean releaseMaster() {
        if (masterFlag) {
            clusterMasterLock.unlock();
            masterFlag = false;
        }
        return true;
    }
    
    @Override
    public boolean isMaster() {
        if (!clusterProperties.isEnabled()) {
            return true;
        }
        return masterFlag;
    }
    
    @Override
    public ClusterMasterDTO getMaster() {
        ClusterMasterDO masterDO = clusterMasterRepository.findById(MASTER_ID).orElse(null);
        return Objects.isNull(masterDO) ? new ClusterMasterDTO() : ClusterMasterTransfer.INSTANCE.mapToDTO(masterDO);
    }
    
    @Override
    public String getMasterUrl() {
        ClusterMasterDO master = clusterMasterRepository.findById(MASTER_ID).orElseThrow();
        String contextPath = master.getContextPath();
        if (StringUtils.isEmpty(contextPath)) {
            return clusterProperties.getSchema() + "://" + master.getMasterHost() + ":" + master.getMasterPort();
        }
        if (contextPath.startsWith("/")) {
            return clusterProperties.getSchema() + "://" + master.getMasterHost() + ":" + master.getMasterPort() + contextPath;
        }
        return clusterProperties.getSchema() + "://" + master.getMasterHost() + ":" + master.getMasterPort() + "/" + contextPath;
    }
}
