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

import okhttp3.Response;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.ClusterMasterMapper;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.model.entity.ClusterMasterDO;
import org.apache.shenyu.admin.service.ClusterMasterService;
import org.apache.shenyu.admin.transfer.ClusterMasterTransfer;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.register.common.type.DataType;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import javax.validation.constraints.NotNull;
import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Objects;

@Service
public class ClusterMasterServiceImpl implements ClusterMasterService {
    
    private static final Logger LOG = LoggerFactory.getLogger(ClusterMasterServiceImpl.class);

    private static final String MASTER_ID = "1";
    
    private volatile boolean isMaster = false;
    
    private final HttpUtils httpUtils = new HttpUtils();

    private final ClusterMasterMapper clusterMasterMapper;
    
    @Value("${server.port}")
    private int port;
    
    @Value("${server.servlet.context-path:}")
    private String contextPath;

    public ClusterMasterServiceImpl(final ClusterMasterMapper clusterMasterMapper) {
        this.clusterMasterMapper = clusterMasterMapper;
    }

    @Override
    public void setMaster(final String masterHost, final String masterPort, final String contextPath) {
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
        return isMaster;
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
    @Override
    public String clusterDataSync(final DataTypeParent syncData) {
        String syncUrl = getSyncUrl(syncData.getType());
        String json = JsonUtils.toJson(syncData);
        LOG.debug("sync dataType: {} to master, data content:{}", syncData.getType().name(), json);
        try (Response response = httpUtils.requestJson(syncUrl,
                json,
                Collections.EMPTY_MAP)) {
            return response.body().toString();
        } catch (Exception e) {
            LOG.error("sync dataType: {} to master error", syncData.getType().name(), e);
            throw new ShenyuException(e);
        }
    }
    
    @NotNull
    private String getSyncUrl(final DataType dataType) {
        String syncUrl = getMasterUrl();
        switch (dataType) {
            case URI:
                syncUrl += Constants.URI_PATH;
                break;
            case META_DATA:
                syncUrl += Constants.META_PATH;
                break;
            case API_DOC:
                syncUrl += Constants.API_DOC_PATH;
                break;
            case DISCOVERY_CONFIG:
                syncUrl += Constants.DISCOVERY_CONFIG_PATH;
                break;
            default:
                break;
        }
        return syncUrl;
    }
}
