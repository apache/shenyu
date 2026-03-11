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

import org.apache.shenyu.admin.jpa.repository.AlertReceiverRepository;
import org.apache.shenyu.admin.model.entity.AlertReceiverDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.AlertReceiverQuery;
import org.apache.shenyu.admin.service.AlertDispatchService;
import org.apache.shenyu.admin.service.AlertReceiverService;
import org.apache.shenyu.admin.transfer.AlertTransfer;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.apache.shenyu.common.dto.AlarmContent;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.util.Date;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link AlertReceiverService}.
 */
@Service
public class AlertReceiverServiceImpl implements AlertReceiverService {
    
    private static final String ALERT_TEST_TITLE = "Alarm Test";
    
    private static final String ALERT_TEST_CONTENT = "test send msg! \n This is the test data. It is proved that it can be received successfully";

    private final AlertDispatchService alertDispatchService;

    private final AlertReceiverRepository alertReceiverRepository;

    public AlertReceiverServiceImpl(final AlertDispatchService alertDispatchService, final AlertReceiverRepository alertReceiverRepository) {
        this.alertDispatchService = alertDispatchService;
        this.alertReceiverRepository = alertReceiverRepository;
    }

    @Override
    public void addReceiver(final AlertReceiverDTO alertReceiverDTO) {
        AlertReceiverDO receiverDO = AlertTransfer.INSTANCE.mapToAlertReceiverDO(alertReceiverDTO);
        receiverDO.setId(UUIDUtils.getInstance().generateShortUuid());
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        receiverDO.setDateCreated(currentTime);
        receiverDO.setDateUpdated(currentTime);
        alertReceiverRepository.save(receiverDO);
        alertDispatchService.clearCache();
    }

    @Override
    public void deleteReceiver(final List<String> ids) {
        alertReceiverRepository.deleteAllByIdInBatch(ids);
        alertDispatchService.clearCache();
    }

    @Override
    public void updateReceiver(final AlertReceiverDTO alertReceiverDTO) {
        AlertReceiverDO receiverDO = AlertTransfer.INSTANCE.mapToAlertReceiverDO(alertReceiverDTO);
        alertReceiverRepository.save(receiverDO);
        alertDispatchService.clearCache();
    }
    
    @Override
    public List<AlertReceiverDTO> getAll() {
        return alertReceiverRepository.findAll()
                .stream()
                .map(AlertTransfer.INSTANCE::mapToAlertReceiverDTO)
                .collect(Collectors.toList());
    }
    
    @Override
    public CommonPager<AlertReceiverDTO> listByPage(final AlertReceiverQuery receiverQuery) {
        Page<AlertReceiverDO> page = alertReceiverRepository.pageByDynamicConditions(receiverQuery, PageResultUtils.of(receiverQuery.getPageParameter()));
        return PageResultUtils.result(receiverQuery.getPageParameter(), page, AlertTransfer.INSTANCE::mapToAlertReceiverDTO);
    }
    
    @Override
    public AlertReceiverDTO detail(final String id) {
        Optional<AlertReceiverDO> receiverDO = alertReceiverRepository.findById(id);
        return receiverDO.map(AlertTransfer.INSTANCE::mapToAlertReceiverDTO).orElse(null);
    }
    
    @Override
    public boolean sendTestMsg(final AlertReceiverDTO alertReceiverDTO) {
        AlarmContent content = new AlarmContent.Builder()
                                       .title(ALERT_TEST_TITLE)
                                       .content(ALERT_TEST_CONTENT)
                                       .level((byte) 2)
                                       .dateCreated(new Date())
                                       .dateUpdated(new Date())
                                       .build();
        return alertDispatchService.sendNoticeMsg(alertReceiverDTO, content);
    }
}
