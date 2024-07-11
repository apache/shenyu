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

import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.service.ClusterSelectMasterService;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.zookeeper.CreateMode;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.integration.zookeeper.lock.ZookeeperLockRegistry;

import java.util.Map;
import java.util.concurrent.locks.Lock;

public class ClusterSelectMasterServiceZookeeperImpl implements ClusterSelectMasterService {
    
    private static final Logger LOG = LoggerFactory.getLogger(ClusterSelectMasterServiceZookeeperImpl.class);
    
    private static final String MASTER_LOCK_KEY = "shenyu_cluster_lock/master";
    
    private static final String MASTER_INFO = "/shenyu_cluster_lock/master/info";
    
    private final ClusterProperties clusterProperties;
    
    private final ClusterZookeeperClient clusterZookeeperClient;
    
    private final Lock clusterMasterLock;
    
    private volatile boolean masterFlag;
    
    public ClusterSelectMasterServiceZookeeperImpl(final ClusterProperties clusterProperties,
                                                   final ZookeeperLockRegistry zookeeperLockRegistry,
                                                   final ClusterZookeeperClient clusterZookeeperClient) {
        this.clusterProperties = clusterProperties;
        this.clusterZookeeperClient = clusterZookeeperClient;
        this.clusterMasterLock = zookeeperLockRegistry.obtain(MASTER_LOCK_KEY);
    }
    
    @Override
    public boolean selectMaster() {
        masterFlag = clusterMasterLock.tryLock();
        return masterFlag;
    }
    
    @Override
    public boolean selectMaster(final String masterHost, final String masterPort, final String contextPath) {
        masterFlag = clusterMasterLock.tryLock();
        if (masterFlag) {
            Map<String, String> masterInfo = Maps.newHashMap();
            masterInfo.put("masterHost", masterHost);
            masterInfo.put("masterPort", masterPort);
            masterInfo.put("contextPath", contextPath);
            clusterZookeeperClient.createOrUpdate(MASTER_INFO, JsonUtils.toJson(masterInfo), CreateMode.PERSISTENT);
        }
        return masterFlag;
    }
    
    @Override
    public boolean checkMasterStatus() {
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
        String masterInfoJson = clusterZookeeperClient.getDirectly(MASTER_INFO);
        return JsonUtils.jsonToObject(masterInfoJson, ClusterMasterDTO.class);
    }
    
    @Override
    public String getMasterUrl() {
        String masterInfoJson = clusterZookeeperClient.getDirectly(MASTER_INFO);
        ClusterMasterDTO master = JsonUtils.jsonToObject(masterInfoJson, ClusterMasterDTO.class);
        if (StringUtils.isEmpty(master.getContextPath())) {
            return clusterProperties.getSchema() + "://" + master.getMasterHost() + ":" + master.getMasterPort();
        }
        return clusterProperties.getSchema() + "://" + master.getMasterHost() + ":" + master.getMasterPort() + "/" + master.getContextPath();
    }
}
