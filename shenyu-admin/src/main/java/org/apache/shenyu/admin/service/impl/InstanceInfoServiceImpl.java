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

import org.apache.shenyu.admin.mapper.InstanceInfoMapper;
import org.apache.shenyu.admin.model.dto.InstanceInfoDTO;
import org.apache.shenyu.admin.model.entity.InstanceInfoDO;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceInfoRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.InstanceInfoService}.
 */
@Service
public class InstanceInfoServiceImpl implements InstanceInfoService {
    
    private static final Logger LOG = LoggerFactory.getLogger(InstanceInfoServiceImpl.class);
    
    private final InstanceInfoMapper instanceInfoMapper;
    
    public InstanceInfoServiceImpl(final InstanceInfoMapper instanceInfoMapper) {
        this.instanceInfoMapper = instanceInfoMapper;
    }

    @Override
    public void registerInstanceInfo(final InstanceInfoRegisterDTO instanceInfoRegisterDTO) {
        LOG.info("Instance info registered: {}", GsonUtils.getInstance().toJson(instanceInfoRegisterDTO));
        InstanceInfoDTO instanceInfoDTO = buildInstanceInfoDTO(instanceInfoRegisterDTO);
        InstanceInfoDO instanceInfoDO = InstanceInfoDO.buildInstanceInfoDO(instanceInfoDTO);
        try {
            instanceInfoMapper.insert(instanceInfoDO);
        }catch (Exception e) {
            LOG.error("Failed to register instance info", e);
        }
    }

    @Override
    public InstanceInfoVO createOrUpdate(final InstanceInfoDTO instanceInfoDTO) {
        // Implementation here
        return null;
    }
    
    private InstanceInfoDTO buildInstanceInfoDTO(final InstanceInfoRegisterDTO instanceInfoRegisterDTO) {
        InstanceInfoDTO instanceInfoDTO = new InstanceInfoDTO();
        instanceInfoDTO.setInstanceIp(instanceInfoRegisterDTO.getInstanceIp());
        instanceInfoDTO.setInstanceType(instanceInfoRegisterDTO.getInstanceType());
        instanceInfoDTO.setInstanceInfo(instanceInfoRegisterDTO.getInstanceInfo());
        return instanceInfoDTO;
    }
}
