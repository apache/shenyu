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
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.sql.Timestamp;
import java.util.Arrays;
import java.util.Collections;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
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
    void testFetchInstanceDataKeepsLiveHeartbeatOverStaleSnapshot() {
        InstanceInfoVO dbVo = buildVO("127.0.0.1", "8080", "grpc", "ns");
        dbVo.setInstanceState(0);
        when(instanceInfoService.list()).thenReturn(Collections.singletonList(dbVo));

        // a heartbeat for the same identity was accepted before the restore completes
        instanceCheckService.handleBeatInfo(buildDTO("127.0.0.1", "8080", "grpc", "ns"));
        String key = "127.0.0.1:8080@grpc#ns";
        InstanceInfoVO live = instanceCheckService.getInstanceHealthBeatInfo(key);
        assertNotNull(live);
        long liveBeat = live.getLastHeartBeatTime();

        instanceCheckService.fetchInstanceData();

        InstanceInfoVO after = instanceCheckService.getInstanceHealthBeatInfo(key);
        assertNotNull(after);
        assertEquals(liveBeat, after.getLastHeartBeatTime());
        assertEquals(1, after.getInstanceState());
    }

    @Test
    void testFetchInstanceDataRestoresAbsentIdentity() {
        InstanceInfoVO dbVo = buildVO("10.1.1.1", "9090", "http", "ns2");
        when(instanceInfoService.list()).thenReturn(Collections.singletonList(dbVo));

        instanceCheckService.fetchInstanceData();

        assertNotNull(instanceCheckService.getInstanceHealthBeatInfo("10.1.1.1:9090@http#ns2"));
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
    void testBlockedSyncDoesNotBlockLivenessCheck() throws Exception {
        InstanceBeatInfoDTO dto = buildDTO("192.168.1.10", "8080", "http", "ns-test");
        instanceCheckService.handleBeatInfo(dto);
        InstanceInfoVO cached = instanceCheckService.getInstanceHealthBeatInfo(dto);
        assertNotNull(cached);
        assertEquals(InstanceStatusEnum.ONLINE.getCode(), cached.getInstanceState());

        // Expire heartbeat beyond timeout threshold (20 seconds)
        cached.setLastHeartBeatTime(System.currentTimeMillis() - 25000);

        CountDownLatch syncStartedLatch = new CountDownLatch(1);
        CountDownLatch syncUnblockLatch = new CountDownLatch(1);

        Mockito.doAnswer(invocation -> {
            syncStartedLatch.countDown();
            syncUnblockLatch.await(5, TimeUnit.SECONDS);
            return null;
        }).when(instanceInfoService).createOrUpdate(any(InstanceInfoVO.class));

        Thread syncThread = new Thread(() -> instanceCheckService.syncDB());
        syncThread.start();

        boolean started = syncStartedLatch.await(5, TimeUnit.SECONDS);
        assertThat(started, equalTo(true));

        // Independent liveness check must advance state to OFFLINE without being blocked by syncDB
        ReflectionTestUtils.invokeMethod(instanceCheckService, "doCheck");
        assertEquals(InstanceStatusEnum.OFFLINE.getCode(), cached.getInstanceState());

        syncUnblockLatch.countDown();
        syncThread.join(5000);
        assertThat(syncThread.isAlive(), equalTo(false));
    }

    @Test
    void testSetupAndCloseWithMultipleNamespaces() {
        InstanceInfoVO vo1 = buildVO("10.0.0.1", "8080", "http", "ns1");
        InstanceInfoVO vo2 = buildVO("10.0.0.2", "8080", "http", "ns2");
        when(instanceInfoService.list()).thenReturn(Arrays.asList(vo1, vo2));

        assertDoesNotThrow(() -> {
            instanceCheckService.setup();
            instanceCheckService.close();
        });
        verify(instanceInfoService, times(2)).createOrUpdate(any(InstanceInfoVO.class));
    }

    @Test
    void testSyncDBExceptionHandling() {
        InstanceBeatInfoDTO dto = buildDTO("10.0.0.3", "8080", "http", "ns");
        instanceCheckService.handleBeatInfo(dto);
        Mockito.doThrow(new RuntimeException("database failure"))
                .when(instanceInfoService).createOrUpdate(any(InstanceInfoVO.class));

        assertDoesNotThrow(() -> instanceCheckService.syncDB());
    }

    @Test
    void testCloseShutsDownExecutorsWhenFinalSyncFails() {
        InstanceBeatInfoDTO dto = buildDTO("10.0.0.9", "8080", "http", "ns");
        instanceCheckService.handleBeatInfo(dto);
        instanceCheckService.setup();

        Mockito.doThrow(new RuntimeException("db error on close"))
                .when(instanceInfoService).createOrUpdate(any(InstanceInfoVO.class));

        assertDoesNotThrow(() -> instanceCheckService.close());

        ScheduledThreadPoolExecutor exec = (ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(instanceCheckService, "executor");
        ScheduledThreadPoolExecutor syncExec = (ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(instanceCheckService, "syncExecutor");
        assertNotNull(exec);
        assertNotNull(syncExec);
        assertThat(exec.isShutdown(), equalTo(true));
        assertThat(syncExec.isShutdown(), equalTo(true));

        // repeated close should also be safe and not throw
        assertDoesNotThrow(() -> instanceCheckService.close());
    }

    @Test
    void testSyncDBContinuesOnException() {
        InstanceBeatInfoDTO dto1 = buildDTO("127.0.0.1", "8080", "grpc", "ns1");
        InstanceBeatInfoDTO dto2 = buildDTO("127.0.0.2", "8081", "grpc", "ns2");
        instanceCheckService.handleBeatInfo(dto1);
        instanceCheckService.handleBeatInfo(dto2);

        Mockito.doThrow(new RuntimeException("db error"))
                .doNothing()
                .when(instanceInfoService).createOrUpdate(any(InstanceInfoVO.class));

        assertDoesNotThrow(() -> instanceCheckService.syncDB());
        verify(instanceInfoService, times(2)).createOrUpdate(any(InstanceInfoVO.class));
    }

    @Test
    void testSetupClosesExistingExecutorsOnReelection() {
        instanceCheckService.setup();
        ScheduledThreadPoolExecutor firstExec = (ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(instanceCheckService, "executor");
        ScheduledThreadPoolExecutor firstSyncExec = (ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(instanceCheckService, "syncExecutor");
        assertNotNull(firstExec);
        assertNotNull(firstSyncExec);
        assertFalse(firstExec.isShutdown());
        assertFalse(firstSyncExec.isShutdown());

        instanceCheckService.setup();
        assertTrue(firstExec.isShutdown());
        assertTrue(firstSyncExec.isShutdown());

        ScheduledThreadPoolExecutor secondExec = (ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(instanceCheckService, "executor");
        ScheduledThreadPoolExecutor secondSyncExec = (ScheduledThreadPoolExecutor) ReflectionTestUtils.getField(instanceCheckService, "syncExecutor");
        assertNotNull(secondExec);
        assertNotNull(secondSyncExec);
        assertFalse(secondExec.isShutdown());
        assertFalse(secondSyncExec.isShutdown());

        instanceCheckService.close();
    }

    @Test
    void testCloseWithHungSyncDoesNotBlockIndefinitely() throws Exception {
        CountDownLatch lockAcquiredLatch = new CountDownLatch(1);
        CountDownLatch testFinishedLatch = new CountDownLatch(1);
        ReentrantLock lock = (ReentrantLock) ReflectionTestUtils.getField(instanceCheckService, "syncLock");
        assertNotNull(lock);

        Thread holdingThread = new Thread(() -> {
            lock.lock();
            try {
                lockAcquiredLatch.countDown();
                testFinishedLatch.await(5, TimeUnit.SECONDS);
            } catch (InterruptedException ignored) {
                Thread.currentThread().interrupt();
            } finally {
                lock.unlock();
            }
        });
        holdingThread.start();
        assertTrue(lockAcquiredLatch.await(5, TimeUnit.SECONDS));

        long start = System.currentTimeMillis();
        assertDoesNotThrow(() -> instanceCheckService.syncDB(50, TimeUnit.MILLISECONDS));
        long elapsed = System.currentTimeMillis() - start;
        assertThat(elapsed, lessThan(2000L));

        testFinishedLatch.countDown();
        holdingThread.join(5000);
    }

    @Test
    void testCopyInstanceInfoVO() {
        InstanceInfoVO original = new InstanceInfoVO();
        original.setId("test-id");
        original.setInstanceIp("127.0.0.1");
        original.setInstancePort("8080");
        original.setInstanceType("http");
        original.setInstanceInfo("{\"app\":\"test\"}");
        original.setNamespaceId("test-ns");
        original.setInstanceState(InstanceStatusEnum.ONLINE.getCode());
        original.setDateCreated(new Timestamp(System.currentTimeMillis()));
        original.setDateUpdated(new Timestamp(System.currentTimeMillis()));
        original.setLastHeartBeatTime(System.currentTimeMillis());

        InstanceInfoVO copy = ReflectionTestUtils.invokeMethod(instanceCheckService, "copyInstanceInfoVO", original);
        assertNotNull(copy);
        assertEquals(original.getId(), copy.getId());
        assertEquals(original.getInstanceIp(), copy.getInstanceIp());
        assertEquals(original.getInstancePort(), copy.getInstancePort());
        assertEquals(original.getInstanceType(), copy.getInstanceType());
        assertEquals(original.getInstanceInfo(), copy.getInstanceInfo());
        assertEquals(original.getNamespaceId(), copy.getNamespaceId());
        assertEquals(original.getInstanceState(), copy.getInstanceState());
        assertEquals(original.getDateCreated(), copy.getDateCreated());
        assertEquals(original.getDateUpdated(), copy.getDateUpdated());
        assertEquals(original.getLastHeartBeatTime(), copy.getLastHeartBeatTime());
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
