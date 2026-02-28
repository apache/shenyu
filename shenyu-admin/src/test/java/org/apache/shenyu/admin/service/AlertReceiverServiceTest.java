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

package org.apache.shenyu.admin.service;

import com.google.common.collect.Lists;
import org.apache.shenyu.admin.jpa.repository.AlertReceiverRepository;
import org.apache.shenyu.admin.model.entity.AlertReceiverDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageParameter;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.AlertReceiverQuery;
import org.apache.shenyu.admin.service.impl.AlertReceiverServiceImpl;
import org.apache.shenyu.admin.transfer.AlertTransfer;
import org.apache.shenyu.alert.model.AlertReceiverDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.domain.PageImpl;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.apache.shenyu.common.constant.Constants.SYS_DEFAULT_NAMESPACE_ID;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.BDDMockito.given;

/**
 * Test cases for AlertReceiverService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class AlertReceiverServiceTest {

    @InjectMocks
    private AlertReceiverServiceImpl alertReceiverService;

    @Mock
    private AlertReceiverRepository alertReceiverRepository;

    @Mock
    private AlertDispatchService alertDispatchService;

    @BeforeEach
    public void setUp() {
        alertReceiverService = new AlertReceiverServiceImpl(alertDispatchService, alertReceiverRepository);
    }

    @Test
    public void testAddReceiver() {
        alertReceiverService.addReceiver(new AlertReceiverDTO());
    }

    @Test
    public void testDeleteReceiver() {
        alertReceiverService.deleteReceiver(Lists.newArrayList("1"));
    }

    @Test
    public void testUpdateReceiver() {
        alertReceiverService.updateReceiver(new AlertReceiverDTO());
    }

    @Test
    public void testGetAll() {
        AlertReceiverDO alertReceiverDTO = buildAlertReceiverDO("123");
        List<AlertReceiverDO> list = Lists.newArrayList(alertReceiverDTO);
        given(alertReceiverRepository.findAll()).willReturn(list);
        List<AlertReceiverDTO> all = alertReceiverService.getAll();
        assertNotNull(all);
        assertEquals(all.size(), list.size());
    }

    @Test
    public void testListByPage() {
        PageParameter pageParameter = new PageParameter();
        pageParameter.setPageSize(5);
        pageParameter.setTotalCount(10);
        pageParameter.setTotalPage(pageParameter.getTotalCount() / pageParameter.getPageSize());
        AlertReceiverQuery alertReceiverQuery = new AlertReceiverQuery(pageParameter);
        alertReceiverQuery.setNamespaceId(SYS_DEFAULT_NAMESPACE_ID);
        List<AlertReceiverDO> receiverDOList = IntStream.range(0, 10).mapToObj(i -> buildAlertReceiverDO(String.valueOf(i))).collect(Collectors.toList());
        PageImpl<AlertReceiverDO> page = new PageImpl<>(receiverDOList, PageResultUtils.of(pageParameter), receiverDOList.size());
        given(this.alertReceiverRepository.pageByDynamicConditions(alertReceiverQuery, PageResultUtils.of(pageParameter))).willReturn(page);
        final CommonPager<AlertReceiverDTO> commonPager = this.alertReceiverService.listByPage(alertReceiverQuery);
        assertEquals(commonPager.getDataList().size(), receiverDOList.size());
    }

    @Test
    public void testDetail() {
        AlertReceiverDO receiverDO = buildAlertReceiverDO("123");
        given(this.alertReceiverRepository.findById(anyString())).willReturn(Optional.of(receiverDO));
        AlertReceiverDTO receiverDTO = alertReceiverService.detail("123");
        assertNotNull(receiverDTO);
        assertEquals(receiverDTO.getId(), receiverDO.getId());
    }

    @Test
    public void testSendTestMsg() {
        AlertReceiverDTO alertReceiverDTO = buildAlertReceiverDTO("123");
        given(this.alertDispatchService.sendNoticeMsg(any(), any())).willReturn(true);
        boolean sent = alertReceiverService.sendTestMsg(alertReceiverDTO);
        assertTrue(sent);
    }

    private AlertReceiverDO buildAlertReceiverDO(final String id) {
        AlertReceiverDO alertReceiverDO = AlertTransfer.INSTANCE.mapToAlertReceiverDO(buildAlertReceiverDTO(id));
        Timestamp now = Timestamp.valueOf(LocalDateTime.now());
        alertReceiverDO.setDateCreated(now);
        alertReceiverDO.setDateUpdated(now);
        return alertReceiverDO;
    }

    private AlertReceiverDTO buildAlertReceiverDTO(final String id) {
        AlertReceiverDTO alertReceiverDTO = new AlertReceiverDTO();
        alertReceiverDTO.setEnable(true);
        alertReceiverDTO.setId(id);
        return alertReceiverDTO;
    }
}
