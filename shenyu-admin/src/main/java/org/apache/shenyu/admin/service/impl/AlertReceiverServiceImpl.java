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

import org.apache.shenyu.admin.mapper.AlertReceiverMapper;
import org.apache.shenyu.admin.model.entity.AlertReceiverDO;
import org.apache.shenyu.admin.service.AlertDispatchService;
import org.apache.shenyu.admin.service.AlertReceiverService;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.util.List;

/**
 * Implementation of the {@link AlertReceiverService}.
 */
@Service
public class AlertReceiverServiceImpl implements AlertReceiverService {
    
    @Autowired
    private AlertReceiverMapper alertReceiverMapper;
    
    @Autowired
    private AlertDispatchService alertDispatchService;
    
    @Override
    public int addReceiver(final AlertReceiverDTO alertReceiverDTO) {
        AlertReceiverDO receiverDO = new AlertReceiverDO();
        BeanUtils.copyProperties(alertReceiverDTO, receiverDO);
        receiverDO.setId(UUIDUtils.getInstance().generateShortUuid());
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        receiverDO.setDateCreated(currentTime);
        receiverDO.setDateUpdated(currentTime);
        alertDispatchService.clearCache();
        return alertReceiverMapper.insert(receiverDO);
    }
    
    @Override
    public int deleteReceiver(final List<String> ids) {
        alertDispatchService.clearCache();
        return alertReceiverMapper.deleteByIds(ids);
    }
    
    @Override
    public int updateReceiver(final AlertReceiverDTO alertReceiverDTO) {
        AlertReceiverDO receiverDO = new AlertReceiverDO();
        BeanUtils.copyProperties(alertReceiverDTO, receiverDO);
        alertDispatchService.clearCache();
        return alertReceiverMapper.updateByPrimaryKey(receiverDO);
    }
    
    @Override
    public List<AlertReceiverDTO> getAll() {
        return alertReceiverMapper.selectAll();
    }
    
    @Override
    public AlertReceiverDTO detail(final String id) {
        AlertReceiverDTO receiverDTO = new AlertReceiverDTO();
        AlertReceiverDO receiverDO = alertReceiverMapper.selectByPrimaryKey(id);
        if (receiverDO != null) {
            BeanUtils.copyProperties(receiverDO, receiverDTO);
            return receiverDTO;
        } else {
            return null;
        }
    }
}
