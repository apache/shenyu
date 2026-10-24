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

import org.apache.shenyu.common.enums.InstanceStatusEnum;
import org.apache.shenyu.admin.model.event.instance.InstanceInfoReportEvent;
import org.apache.shenyu.admin.model.vo.InstanceDataVisualVO;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
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
import java.util.Arrays;
import java.util.List;
import java.util.Collections;
import java.util.Map;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertFalse;
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

        InstanceInfoVO cached =
                instanceCheckService.getInstanceHealthBeatInfo(first);
        assertNotNull(cached);

        InstanceBeatInfoDTO blank =
                buildDTO("127.0.0.1", "8080", "grpc", "ns");
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
    void testHistoryIsScopedByNamespace() {
        // Namespace A has two online instances.
        instanceCheckService.handleBeatInfo(
                buildDTO("10.0.0.1", "8080", "http", "nsA"));
        instanceCheckService.handleBeatInfo(
                buildDTO("10.0.0.2", "8080", "http", "nsA"));

        // Namespace B has three online instances.
        instanceCheckService.handleBeatInfo(
                buildDTO("10.0.0.3", "8080", "http", "nsB"));
        instanceCheckService.handleBeatInfo(
                buildDTO("10.0.0.4", "8080", "http", "nsB"));
        instanceCheckService.handleBeatInfo(
                buildDTO("10.0.0.5", "8080", "http", "nsB"));

        // Collect one history sample without waiting for the scheduler.
        ReflectionTestUtils.invokeMethod(instanceCheckService, "collectStateData");

        assertEquals(2L, lastOnlineCount("nsA"));
        assertEquals(3L, lastOnlineCount("nsB"));
        assertEquals(5L, lastOnlineCount(""));
    }

    @Test
    void testHistoryRetainsLatestTwentySamples() {
        for (int index = 1; index <= 25; index++) {
            instanceCheckService.handleBeatInfo(
                    buildDTO("10.1.0." + index, "8080", "http", "nsA"));

            ReflectionTestUtils.invokeMethod(
                    instanceCheckService, "collectStateData");
        }

        final List<Long> history = instanceCheckService
                .getInstanceDataVisual("nsA")
                .getLineData()
                .stream()
                .filter(line -> InstanceStatusEnum.getNameByCode(
                        InstanceStatusEnum.ONLINE.getCode()).equals(line.getName()))
                .findFirst()
                .orElseThrow()
                .getData();

        assertEquals(20, history.size());
        assertEquals(6L, history.get(0).longValue());
        assertEquals(25L, history.get(19).longValue());
        assertEquals(25L, lastOnlineCount(""));
    }

    @Test
    void testHistoryIsRemovedWhenNamespaceHasNoTrackedInstances() {
        instanceCheckService.handleBeatInfo(
                buildDTO("10.2.0.1", "8080", "http", "nsA"));
        instanceCheckService.handleBeatInfo(
                buildDTO("10.2.0.2", "8080", "http", "nsB"));

        ReflectionTestUtils.invokeMethod(
                instanceCheckService, "collectStateData");

        assertEquals(1L, lastOnlineCount("nsA"));
        assertEquals(1L, lastOnlineCount("nsB"));

        final Map<?, ?> instances = (Map<?, ?>) ReflectionTestUtils.getField(
                instanceCheckService, "instanceHealthBeatInfo");

        assertNotNull(instances);

        // Simulate namespace A having no tracked instances.
        instances.entrySet().removeIf(entry ->
                "nsA".equals(((InstanceInfoVO) entry.getValue()).getNamespaceId()));

        ReflectionTestUtils.invokeMethod(
                instanceCheckService, "collectStateData");

        final Map<?, ?> histories = (Map<?, ?>) ReflectionTestUtils.getField(
                instanceCheckService, "namespaceStateHistoryMap");

        assertNotNull(histories);
        assertFalse(histories.containsKey("nsA"));
        assertEquals(1L, lastOnlineCount("nsB"));
        assertEquals(1L, lastOnlineCount(""));

        // Missing namespaces return zero-filled curves without creating histories.
        for (String namespaceId : Arrays.asList("nsA", "missing")) {
            final InstanceDataVisualVO visual =
                    instanceCheckService.getInstanceDataVisual(namespaceId);

            assertTrue(visual.getPieData().isEmpty());
            assertEquals(3, visual.getLineData().size());
            visual.getLineData().forEach(line -> {
                assertEquals(20, line.getData().size());
                assertTrue(line.getData().stream().allMatch(value -> value == 0L));
            });

            assertFalse(histories.containsKey(namespaceId));
        }

        // No instances remain anywhere.
        instances.clear();

        ReflectionTestUtils.invokeMethod(
                instanceCheckService, "collectStateData");

        assertTrue(histories.isEmpty());
        assertEquals(0L, lastOnlineCount(""));
    }

    @Test
    void testHistorySeparatesStatesAcrossNamespaces() {
        final InstanceInfoVO onlineA =
                buildVO("10.3.0.1", "8080", "http", "nsA");
        final InstanceInfoVO offlineA =
                buildVO("10.3.0.2", "8080", "http", "nsA");
        final InstanceInfoVO deletedB =
                buildVO("10.3.0.3", "8080", "http", "nsB");

        onlineA.setInstanceState(InstanceStatusEnum.ONLINE.getCode());
        offlineA.setInstanceState(InstanceStatusEnum.OFFLINE.getCode());
        deletedB.setInstanceState(InstanceStatusEnum.DELETED.getCode());

        when(instanceInfoService.list()).thenReturn(
                Arrays.asList(onlineA, offlineA, deletedB));
        instanceCheckService.fetchInstanceData();

        ReflectionTestUtils.invokeMethod(
                instanceCheckService, "collectStateData");

        assertStateCount("nsA", InstanceStatusEnum.ONLINE, 1L);
        assertStateCount("nsA", InstanceStatusEnum.OFFLINE, 1L);
        assertStateCount("nsA", InstanceStatusEnum.DELETED, 0L);

        assertStateCount("nsB", InstanceStatusEnum.ONLINE, 0L);
        assertStateCount("nsB", InstanceStatusEnum.OFFLINE, 0L);
        assertStateCount("nsB", InstanceStatusEnum.DELETED, 1L);

        assertStateCount("", InstanceStatusEnum.ONLINE, 1L);
        assertStateCount("", InstanceStatusEnum.OFFLINE, 1L);
        assertStateCount("", InstanceStatusEnum.DELETED, 1L);
    }

    private void assertStateCount(final String namespaceId,
                                  final InstanceStatusEnum status,
                                  final long expected) {
        final InstanceDataVisualVO visual =
                instanceCheckService.getInstanceDataVisual(namespaceId);
        final String stateName =
                InstanceStatusEnum.getNameByCode(status.getCode());

        final List<Long> history = visual.getLineData().stream()
                .filter(line -> stateName.equals(line.getName()))
                .findFirst()
                .orElseThrow()
                .getData();

        assertEquals(expected, history.get(history.size() - 1).longValue());

        final long pieCount = visual.getPieData().stream()
                .filter(entry -> stateName.equals(entry.getName()))
                .mapToLong(InstanceDataVisualVO.Entry::getValue)
                .sum();

        assertEquals(expected, pieCount);
    }

    private long lastOnlineCount(final String namespaceId) {
        final List<Long> history = instanceCheckService
                .getInstanceDataVisual(namespaceId)
                .getLineData()
                .stream()
                .filter(line -> InstanceStatusEnum.getNameByCode(
                        InstanceStatusEnum.ONLINE.getCode()).equals(line.getName()))
                .findFirst()
                .orElseThrow()
                .getData();
        return history.get(history.size() - 1);
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
