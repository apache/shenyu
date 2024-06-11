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

package org.apache.shenyu.admin.mode.cluster.impl.zookeeper;

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.integration.zookeeper.lock.ZookeeperLockRegistry;

import java.util.concurrent.locks.Lock;

public class ClusterSelectMasterServiceZookeeperImpl implements ClusterSelectMasterService {
    
    private static final Logger LOG = LoggerFactory.getLogger(ClusterSelectMasterServiceZookeeperImpl.class);
    
    private static final String MASTER_LOCK_KEY = "/shenyu/cluster/master";
    
    private final ClusterProperties clusterProperties;
    
    private final Lock clusterMasterLock;
    
    private volatile boolean locked;
    
    public ClusterSelectMasterServiceZookeeperImpl(final ClusterProperties clusterProperties,
                                                   final ZookeeperLockRegistry zookeeperLockRegistry) {
        this.clusterProperties = clusterProperties;
        this.clusterMasterLock = zookeeperLockRegistry.obtain(MASTER_LOCK_KEY);
    }
    
    @Override
    public boolean selectMaster() {
        locked = clusterMasterLock.tryLock();
        return locked;
    }
    
    @Override
    public boolean selectMaster(final String masterHost, final String masterPort, final String contextPath) {
        return false;
    }
    
    @Override
    public boolean checkMasterStatus() {
        return locked;
    }
    
    @Override
    public boolean releaseMaster() {
        if (locked) {
            clusterMasterLock.unlock();
            locked = false;
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
//        ClusterMasterDO masterDO = clusterMasterMapper.selectById(MASTER_ID);
//        return Objects.isNull(masterDO) ? new ClusterMasterDTO() : ClusterMasterTransfer.INSTANCE.mapToDTO(masterDO);
        return null;
    }
    
    @Override
    public String getMasterUrl() {
//        ClusterMasterDO master = clusterMasterMapper.selectById(MASTER_ID);
//        if (StringUtils.isEmpty(master.getContextPath())) {
//            return "http://" + master.getMasterHost() + ":" + master.getMasterPort();
//        }
//        return "http://" + master.getMasterHost() + ":" + master.getMasterPort() + "/" + master.getContextPath();
        return null;
    }
}
