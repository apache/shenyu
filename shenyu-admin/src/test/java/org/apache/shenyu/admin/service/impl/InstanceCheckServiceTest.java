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

import org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent;
import org.apache.shenyu.admin.model.vo.InstanceDataVisualVO;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.sql.Timestamp;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class InstanceCheckServiceTest {

    @InjectMocks
    private InstanceCheckService instanceCheckService;

    @Mock
    private InstanceInfoService instanceInfoService;

    private InstanceInfoVO vo;

    @BeforeEach
    void setUp() {
        instanceCheckService = new InstanceCheckService(instanceInfoService);
        vo = buildVO("127.0.0.1", "8080", "grpc", "ns");
    }

    @Test
    void testFetchInstanceDataAndGetInstanceHealthBeatInfo() {
        when(instanceInfoService.list()).thenReturn(Collections.singletonList(vo));
        instanceCheckService.fetchInstanceData();
        String key = instanceCheckService.getInstanceKey(vo);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(key);
        assertNotNull(cached);
        assertEquals(vo.getInstanceIp(), cached.getInstanceIp());
    }

    @Test
    void testGetInstanceKeyForVOAndDTO() {
        String keyVo = instanceCheckService.getInstanceKey(vo);
        InstanceBeatInfoDTO dto = buildDTO("127.0.0.1", "8080", "grpc", "ns");
        String keyDto = instanceCheckService.getInstanceKey(dto);
        assertThat(keyVo, equalTo("127.0.0.1:8080@grpc#ns"));
        assertThat(keyDto, equalTo("127.0.0.1:8080@grpc#ns"));
    }

    @Test
    void testGetInstanceHealthBeatInfoByDTO() {
        InstanceBeatInfoDTO dto = buildDTO("127.0.0.1", "8080", "grpc", "ns");
        instanceCheckService.handleBeatInfo(dto);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(dto);
        assertNotNull(cached);
        assertEquals("127.0.0.1", cached.getInstanceIp());
    }

    @Test
    void testHandleBeatInfoNewAndExisting() throws InterruptedException {
        InstanceBeatInfoDTO dto = buildDTO("127.0.0.1", "8080", "grpc", "ns");
        instanceCheckService.handleBeatInfo(dto);
        InstanceInfoVO first = instanceCheckService.getInstanceHealthBeatInfo(dto);
        assertNotNull(first);
        long firstBeat = first.getLastHeartBeatTime();
        assertThat(firstBeat, greaterThan(0L));

        // Add a small delay to ensure different timestamps
        Thread.sleep(1);
        
        instanceCheckService.handleBeatInfo(dto);
        InstanceInfoVO second = instanceCheckService.getInstanceHealthBeatInfo(dto);
        assertNotNull(second);
        assertThat(second.getLastHeartBeatTime(), greaterThan(firstBeat));
    }

    @Test
    void testSyncDB() {
        InstanceBeatInfoDTO dto = buildDTO("127.0.0.1", "8080", "grpc", "ns");
        instanceCheckService.handleBeatInfo(dto);
        instanceCheckService.syncDB();
        verify(instanceInfoService, times(1)).createOrUpdate(any(InstanceInfoVO.class));
    }

    @Test
    void testOnInstanceInfoReport() {
        InstanceInfoReportEvent event = InstanceInfoReportEvent.builder()
                .instanceIp("10.0.0.1")
                .instancePort("9000")
                .instanceType("http")
                .instanceInfo("info")
                .namespaceId("ns2")
                .build();
        instanceCheckService.onInstanceInfoReport(event);
        InstanceBeatInfoDTO dto = buildDTO("10.0.0.1", "9000", "http", "ns2");
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(dto);
        assertThat(cached, notNullValue());
        assertEquals("10.0.0.1", cached.getInstanceIp());
    }

    @Test
    void testGetInstanceDataVisual() {
        // add two instances, then adjust state of one
        InstanceBeatInfoDTO dto1 = buildDTO("1.1.1.1", "8081", "grpc", "nsA");
        InstanceBeatInfoDTO dto2 = buildDTO("2.2.2.2", "8082", "grpc", "nsB");
        instanceCheckService.handleBeatInfo(dto1);
        instanceCheckService.handleBeatInfo(dto2);
        // mark dto2 as OFFLINE by editing the cached VO
        InstanceInfoVO cached2 = instanceCheckService.getInstanceHealthBeatInfo(dto2);
        cached2.setInstanceState(0);

        InstanceDataVisualVO all = instanceCheckService.getInstanceDataVisual("");
        assertNotNull(all);
        assertThat(all.getPieData(), hasSize(2));
        assertThat(all.getLineData(), hasSize(3));
        all.getLineData().forEach(line -> assertThat(line.getData(), hasSize(20)));

        InstanceDataVisualVO nsAData = instanceCheckService.getInstanceDataVisual("nsA");
        assertNotNull(nsAData);
        assertThat(nsAData.getPieData(), hasSize(1));
    }

    private InstanceInfoVO buildVO(final String ip, final String port, final String type, final String ns) {
        InstanceInfoVO v = new InstanceInfoVO();
        v.setInstanceIp(ip);
        v.setInstancePort(port);
        v.setInstanceType(type);
        v.setInstanceInfo("info");
        v.setNamespaceId(ns);
        v.setInstanceState(1);
        v.setLastHeartBeatTime(System.currentTimeMillis());
        v.setDateCreated(new Timestamp(System.currentTimeMillis()));
        v.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        return v;
    }

    private InstanceBeatInfoDTO buildDTO(final String ip, final String port, final String type, final String ns) {
        InstanceBeatInfoDTO dto = new InstanceBeatInfoDTO();
        dto.setInstanceIp(ip);
        dto.setInstancePort(port);
        dto.setInstanceType(type);
        dto.setInstanceInfo("info");
        dto.setNamespaceId(ns);
        return dto;
    }
}
