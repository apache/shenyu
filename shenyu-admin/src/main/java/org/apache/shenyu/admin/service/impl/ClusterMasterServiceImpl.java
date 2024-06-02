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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mapper.ClusterMasterMapper;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.model.entity.ClusterMasterDO;
import org.apache.shenyu.admin.service.ClusterMasterService;
import org.apache.shenyu.admin.transfer.ClusterMasterTransfer;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Objects;

@Service
public class ClusterMasterServiceImpl implements ClusterMasterService {

    private static final String MASTER_ID = "1";
    
    private volatile boolean isMaster;
    
    private final ClusterProperties clusterProperties;

    private final ClusterMasterMapper clusterMasterMapper;

    public ClusterMasterServiceImpl(final ClusterProperties clusterProperties,
                                    final ClusterMasterMapper clusterMasterMapper) {
        this.clusterProperties = clusterProperties;
        this.clusterMasterMapper = clusterMasterMapper;
    }

    @Override
    public synchronized void setMaster(final String masterHost, final String masterPort, final String contextPath) {
        this.isMaster = true;
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

    @Override
    public boolean isMaster(final String myHost, final String myPort, final String contextPath) {
        ClusterMasterDO masterDO = ClusterMasterDO.builder()
                .masterHost(myHost)
                .masterPort(myPort)
                .contextPath(contextPath)
                .build();
        return clusterMasterMapper.count(masterDO) > 0;
    }
    
    @Override
    public boolean isMaster() {
        if (!clusterProperties.isEnabled()) {
            return true;
        }
        return isMaster;
    }
    
    @Override
    public synchronized void removeMaster() {
        this.isMaster = false;
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
