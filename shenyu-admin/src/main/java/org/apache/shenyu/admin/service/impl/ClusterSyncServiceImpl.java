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
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.service.ClusterMasterService;
import org.apache.shenyu.admin.service.ClusterSyncService;
import org.apache.shenyu.admin.utils.HttpUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.JsonUtils;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Collections;

@Service
public class ClusterSyncServiceImpl implements ClusterSyncService {
    
    private static final Logger LOG = LoggerFactory.getLogger(ClusterSyncServiceImpl.class);
    
    @Resource
    private ClusterMasterService clusterMasterService;
    
    private final HttpUtils httpUtils = new HttpUtils();
    
    @Override
    public String clusterSync(final DataTypeParent registerDTO) {
        String fullUrl = getFullUrl(registerDTO);
        try {
            LOG.debug("sync dataType: {} to master", registerDTO.getType().name());
            Response response = httpUtils.requestJson(fullUrl,
                    JsonUtils.toJson(registerDTO),
                    Collections.EMPTY_MAP);
            return response.body().toString();
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }
    
    @NotNull
    private String getFullUrl(final DataTypeParent registerDTO) {
        String fullUrl = getMasterUrl();
        switch (registerDTO.getType()) {
            case URI:
                fullUrl += Constants.URI_PATH;
                break;
            case META_DATA:
                fullUrl += Constants.META_PATH;
                break;
            case API_DOC:
                fullUrl += Constants.API_DOC_PATH;
                break;
            case DISCOVERY_CONFIG:
                fullUrl += Constants.DISCOVERY_CONFIG_PATH;
                break;
            default:
                break;
        }
        return fullUrl;
    }
    
    private String getMasterUrl() {
        ClusterMasterDTO master = clusterMasterService.getMaster();
        if (StringUtils.isEmpty(master.getContextPath())) {
            return "http://" + master.getMasterHost() + ":" + master.getMasterPort();
        }
        return "http://" + master.getMasterHost() + ":" + master.getMasterPort() + "/" + master.getContextPath();
    }
}
