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

import org.apache.shenyu.admin.config.properties.InstanceRetentionProperties;
import org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent;
import org.apache.shenyu.admin.model.vo.InstanceDataVisualVO;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.apache.shenyu.common.enums.InstanceStatusEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceBeatInfoDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.NullAndEmptySource;
import org.junit.jupiter.params.provider.ValueSource;
import org.springframework.test.util.ReflectionTestUtils;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.sql.Timestamp;
import java.time.Duration;
import java.util.Arrays;
import java.util.Collections;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.Mockito.never;
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

    @Mock
    private InstanceRetentionProperties instanceRetentionProperties;

    private InstanceInfoVO vo;

    @BeforeEach
    void setUp() {
        instanceCheckService = new InstanceCheckService(instanceInfoService, instanceRetentionProperties);
        vo = buildVO("127.0.0.1", "8080", "grpc", "ns");
    }

    @Test
    void testHeartbeatNormalizesLegacyInformation() {
        String information = "{\"cpu\":4,\"os\":\"Windows\"}";
        InstanceBeatInfoDTO dto = buildDTO("127.0.0.1", "8080", "http", "ns");
        dto.setInstanceInfo(GsonUtils.getInstance().toJson(information));

        instanceCheckService.handleBeatInfo(dto);

        InstanceInfoVO actual = instanceCheckService.getInstanceHealthBeatInfo(dto);
        assertEquals(information, actual.getInstanceInfo());
        assertEquals(1, actual.getInstanceState());
    }

    @Test
    void testCurrentHeartbeatInformationIsPreserved() {
        String information = "{\"cpu\":4}";
        InstanceBeatInfoDTO dto = buildDTO("127.0.0.1", "8080", "http", "ns");
        dto.setInstanceInfo(information);

        instanceCheckService.handleBeatInfo(dto);

        assertEquals(information, instanceCheckService.getInstanceHealthBeatInfo(dto).getInstanceInfo());
    }

    @ParameterizedTest
    @NullAndEmptySource
    @ValueSource(strings = {"{broken}", "[]", "true", "not-json"})
    void testInvalidOptionalInformationDoesNotStopHeartbeats(final String information) {
        InstanceBeatInfoDTO dto = buildDTO("127.0.0.1", "8080", "http", "ns");
        dto.setInstanceInfo(information);

        assertDoesNotThrow(() -> instanceCheckService.handleBeatInfo(dto));

        InstanceInfoVO actual = instanceCheckService.getInstanceHealthBeatInfo(dto);
        assertEquals("{}", actual.getInstanceInfo());
        assertEquals(1, actual.getInstanceState());
        actual.setLastHeartBeatTime(0L);
        assertDoesNotThrow(() -> instanceCheckService.handleBeatInfo(dto));
        assertThat(actual.getLastHeartBeatTime(), greaterThan(0L));
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
    void testFetchInstanceDataNormalizesNullState() {
        vo.setInstanceState(null);
        when(instanceInfoService.list()).thenReturn(Collections.singletonList(vo));

        instanceCheckService.fetchInstanceData();

        assertEquals(0, vo.getInstanceState());
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
    void testHandleBeatInfoRefreshesInstanceInfoOnExistingIdentity() {
        InstanceBeatInfoDTO first = buildDTO("127.0.0.1", "8080", "grpc", "ns");
        first.setInstanceInfo("{\"version\":\"1.0\"}");
        instanceCheckService.handleBeatInfo(first);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(first);
        assertNotNull(cached);
        assertEquals("{\"version\":\"1.0\"}", cached.getInstanceInfo());

        // same identity reports again with changed metadata
        InstanceBeatInfoDTO second = buildDTO("127.0.0.1", "8080", "grpc", "ns");
        second.setInstanceInfo("{\"version\":\"2.0\"}");
        instanceCheckService.handleBeatInfo(second);
        assertEquals("{\"version\":\"2.0\"}", cached.getInstanceInfo());
    }

    @Test
    void testHandleBeatInfoKeepsInstanceInfoWhenBeatCarriesBlank() {
        InstanceBeatInfoDTO first = buildDTO("127.0.0.1", "8080", "grpc", "ns");
        first.setInstanceInfo("{\"version\":\"1.0\"}");
        instanceCheckService.handleBeatInfo(first);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(first);
        assertNotNull(cached);

        InstanceBeatInfoDTO blank = buildDTO("127.0.0.1", "8080", "grpc", "ns");
        blank.setInstanceInfo("");
        instanceCheckService.handleBeatInfo(blank);
        assertEquals("{\"version\":\"1.0\"}", cached.getInstanceInfo());
    }

    @Test
    void testSyncDB() {
        InstanceBeatInfoDTO dto = buildDTO("127.0.0.1", "8080", "grpc", "ns");
        instanceCheckService.handleBeatInfo(dto);
        instanceCheckService.syncDB();
        verify(instanceInfoService, times(1)).createOrUpdate(any(InstanceInfoVO.class));
    }

    @Test
    void testCloseWithoutSetup() {
        assertDoesNotThrow(instanceCheckService::close);
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

    @Test
    void testGetInstanceDataVisualIgnoresNullState() {
        InstanceBeatInfoDTO dto = buildDTO("3.3.3.3", "8083", "grpc", "nsC");
        instanceCheckService.handleBeatInfo(dto);
        instanceCheckService.getInstanceHealthBeatInfo(dto).setInstanceState(null);

        assertDoesNotThrow(() -> instanceCheckService.getInstanceDataVisual(""));
    }

    @Test
    void testDoCheckWithNullStateDoesNotThrow() {
        InstanceBeatInfoDTO dto = buildDTO("4.4.4.4", "8084", "grpc", "nsD");
        instanceCheckService.handleBeatInfo(dto);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(dto);
        cached.setInstanceState(null);

        assertDoesNotThrow(() -> ReflectionTestUtils.invokeMethod(instanceCheckService, "doCheck"));
    }

    @Test
    void testDoCheckAppendsOneHistorySamplePerCycle() {
        InstanceInfoVO first = buildVO("10.0.0.1", "8080", "http", "ns");
        InstanceInfoVO second = buildVO("10.0.0.2", "8080", "http", "ns");
        when(instanceInfoService.list()).thenReturn(Arrays.asList(first, second));
        instanceCheckService.fetchInstanceData();

        ReflectionTestUtils.invokeMethod(instanceCheckService, "doCheck");

        long sampledPoints = instanceCheckService.getInstanceDataVisual("ns").getLineData().stream()
                .flatMap(line -> line.getData().stream())
                .filter(value -> value > 0)
                .count();
        assertEquals(1, sampledPoints, "one scheduled check must append exactly one history sample");
    }

    @Test
    void testRetentionCleanupDisabledByDefault() {
        InstanceBeatInfoDTO dto = buildDTO("5.5.5.5", "8085", "http", "nsE");
        instanceCheckService.handleBeatInfo(dto);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(dto);
        cached.setInstanceState(InstanceStatusEnum.DELETED.getCode());
        cached.setLastHeartBeatTime(System.currentTimeMillis() - Duration.ofDays(30).toMillis());

        instanceCheckService.purgeExpiredInstances();

        assertNotNull(instanceCheckService.getInstanceHealthBeatInfo(dto));
        verify(instanceInfoService, never()).deleteExpired(anyLong());
    }

    @Test
    void testRetentionCleanupRemovesOnlyExpiredDeletedInstances() {
        when(instanceRetentionProperties.isEnabled()).thenReturn(true);
        when(instanceRetentionProperties.getTtl()).thenReturn(Duration.ofMinutes(10));
        InstanceBeatInfoDTO expiredDTO = buildDTO("6.6.6.6", "8086", "http", "nsF");
        InstanceBeatInfoDTO freshDeletedDTO = buildDTO("7.7.7.7", "8087", "http", "nsF");
        InstanceBeatInfoDTO onlineDTO = buildDTO("8.8.8.8", "8088", "http", "nsF");
        instanceCheckService.handleBeatInfo(expiredDTO);
        instanceCheckService.handleBeatInfo(freshDeletedDTO);
        instanceCheckService.handleBeatInfo(onlineDTO);
        InstanceInfoVO expired = instanceCheckService.getInstanceHealthBeatInfo(expiredDTO);
        expired.setInstanceState(InstanceStatusEnum.DELETED.getCode());
        expired.setLastHeartBeatTime(System.currentTimeMillis() - Duration.ofHours(1).toMillis());
        InstanceInfoVO freshDeleted = instanceCheckService.getInstanceHealthBeatInfo(freshDeletedDTO);
        freshDeleted.setInstanceState(InstanceStatusEnum.DELETED.getCode());
        InstanceInfoVO online = instanceCheckService.getInstanceHealthBeatInfo(onlineDTO);

        instanceCheckService.purgeExpiredInstances();

        assertNull(instanceCheckService.getInstanceHealthBeatInfo(expiredDTO));
        assertNotNull(instanceCheckService.getInstanceHealthBeatInfo(freshDeletedDTO));
        assertNotNull(instanceCheckService.getInstanceHealthBeatInfo(onlineDTO));
        verify(instanceInfoService).deleteExpired(anyLong());
    }

    @Test
    void testRetentionCleanupKeepsRenewedIdentity() {
        when(instanceRetentionProperties.isEnabled()).thenReturn(true);
        when(instanceRetentionProperties.getTtl()).thenReturn(Duration.ofMinutes(10));
        InstanceBeatInfoDTO dto = buildDTO("9.9.9.9", "8089", "http", "nsG");
        instanceCheckService.handleBeatInfo(dto);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(dto);
        cached.setInstanceState(InstanceStatusEnum.DELETED.getCode());
        cached.setLastHeartBeatTime(System.currentTimeMillis() - Duration.ofHours(1).toMillis());

        // the instance heartbeats again before the cleanup cycle runs, so its cached record is kept
        instanceCheckService.handleBeatInfo(dto);
        instanceCheckService.purgeExpiredInstances();

        assertNotNull(instanceCheckService.getInstanceHealthBeatInfo(dto));
    }

    @Test
    void testRetentionCleanupRemovesLegacyRowsWithoutHeartbeat() {
        when(instanceRetentionProperties.isEnabled()).thenReturn(true);
        when(instanceRetentionProperties.getTtl()).thenReturn(Duration.ofDays(7));
        InstanceInfoVO legacy = buildVO("11.0.0.1", "8090", "http", "nsH");
        legacy.setInstanceState(InstanceStatusEnum.DELETED.getCode());
        legacy.setLastHeartBeatTime(0L);
        when(instanceInfoService.list()).thenReturn(Collections.singletonList(legacy));
        instanceCheckService.fetchInstanceData();

        instanceCheckService.purgeExpiredInstances();

        assertNull(instanceCheckService.getInstanceHealthBeatInfo("11.0.0.1:8090@http#nsH"));
        verify(instanceInfoService).deleteExpired(anyLong());
    }

    @Test
    void testDatabaseSweepIsThrottledToConfiguredInterval() {
        when(instanceRetentionProperties.isEnabled()).thenReturn(true);
        when(instanceRetentionProperties.getTtl()).thenReturn(Duration.ofDays(7));

        instanceCheckService.purgeExpiredInstances();
        instanceCheckService.purgeExpiredInstances();
        verify(instanceInfoService, times(1)).deleteExpired(anyLong());

        // the database sweep runs again only after the cleanup interval has elapsed
        ReflectionTestUtils.setField(instanceCheckService, "lastDbCleanupTime",
                System.currentTimeMillis() - Duration.ofHours(2).toMillis());
        instanceCheckService.purgeExpiredInstances();
        verify(instanceInfoService, times(2)).deleteExpired(anyLong());
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

