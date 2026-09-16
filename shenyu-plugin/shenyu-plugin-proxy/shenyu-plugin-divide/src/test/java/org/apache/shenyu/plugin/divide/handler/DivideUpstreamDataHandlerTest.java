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

package org.apache.shenyu.plugin.divide.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.common.utils.UpstreamCheckUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.cache.UpstreamCheckTask;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedStatic;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.test.util.ReflectionTestUtils;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class DivideUpstreamDataHandlerTest {

    private DiscoverySyncData discoverySyncData;

    private DivideUpstreamDataHandler divideUpstreamDataHandler;

    private MockedStatic<UpstreamCheckUtils> mockCheckUtils;

    @BeforeEach
    public void setUp() {
        this.divideUpstreamDataHandler = new DivideUpstreamDataHandler();
        List<DiscoveryUpstreamData> divideUpstreamList = Stream.of(3)
                .map(weight -> DiscoveryUpstreamData.builder()
                        .url("mock-" + weight)
                        .dateUpdated(new Timestamp(System.currentTimeMillis()))
                        .build())
                .collect(Collectors.toList());
        this.discoverySyncData = mock(DiscoverySyncData.class);
        when(discoverySyncData.getSelectorId()).thenReturn("handler");
        when(discoverySyncData.getUpstreamDataList()).thenReturn(divideUpstreamList);

        // mock static
        mockCheckUtils = mockStatic(UpstreamCheckUtils.class);
        mockCheckUtils.when(() -> UpstreamCheckUtils.checkUrl(anyString(), anyInt())).thenReturn(true);
    }

    @AfterEach
    public void tearDown() {
        mockCheckUtils.close();
        UpstreamCacheManager.getInstance().removeByKey("handler");
    }

    /**
     * Handler selector test.
     */
    @Test
    public void handlerDiscoveryUpstreamDataTest() {
        divideUpstreamDataHandler.handlerDiscoveryUpstreamData(discoverySyncData);
        List<Upstream> result = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler");
        assertEquals(discoverySyncData.getUpstreamDataList().get(0).getUrl(), result.get(0).getUrl());
        DiscoverySyncData discoverySyncData = new DiscoverySyncData();
        discoverySyncData.setSelectorId(null);
        divideUpstreamDataHandler.handlerDiscoveryUpstreamData(discoverySyncData);
    }

    /**
     * Plugin named test.
     */
    @Test
    public void pluginNamedTest() {
        assertEquals(divideUpstreamDataHandler.pluginName(), PluginEnum.DIVIDE.getName());
    }

    @ParameterizedTest
    @ValueSource(strings = {
        "{\"warmup\":\"12\",\"gray\":\"true\",\"healthCheckEnabled\":\"false\",\"labels\":{\"release\":\"canary\"}}",
        "{\"warmup\":12,\"gray\":true,\"healthCheckEnabled\":false,\"labels\":{\"release\":\"canary\"}}"
    })
    public void testScalarCompatibilityAndSeparateLegacyGrayView(final String props) {
        publish(List.of(instance("canary:8080", props), instance("stable:8080", "{\"labels\":{\"release\":\"stable\"}}")));
        List<Upstream> upstreams = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler");
        assertEquals(2, upstreams.size());
        List<Upstream> legacy = UpstreamCacheManager.getInstance().findLegacyUpstreamListBySelectorId("handler");
        assertEquals(1, legacy.size());
        Upstream canary = legacy.get(0);
        assertEquals(12, canary.getWarmup());
        assertTrue(canary.isGray());
        assertFalse(canary.isHealthCheckEnabled());
        assertEquals(Map.of("release", "canary"), canary.getMetadata());
    }

    @Test
    public void testLabelUpdatesAndDeletionRetainStatistics() {
        publish(List.of(instance("backend:8080", "{\"labels\":{\"release\":\"canary\",\"region\":\"shanghai\"}}")));
        Upstream original = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler").get(0);
        original.getSucceeded().set(17);
        original.setLag(42);
        publish(List.of(instance("backend:8080", "{\"labels\":{\"release\":\"stable\"}}")));
        Upstream updated = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler").get(0);
        assertSame(original, updated);
        assertEquals(17, updated.getSucceeded().get());
        assertEquals(42, updated.getLag());
        assertFalse(updated.isGray());
        assertEquals(Map.of("release", "stable"), updated.getMetadata());
        publish(List.of(instance("backend:8080", "{\"labels\":{}}")));
        assertTrue(original.getMetadata().isEmpty());
        publish(List.of(instance("backend:8080", "{\"labels\":{\"release\":\"canary\"}}")));
        publish(List.of(instance("backend:8080", "{}")));
        assertTrue(original.getMetadata().isEmpty());
    }

    @ParameterizedTest
    @ValueSource(strings = {"[]", "{\"labels\":[]}", "{\"labels\":null}", "{\"labels\":{\"release\":1}}",
        "{\"labels\":{\"release\":true}}", "{\"labels\":{\"release\":null}}", "{\"labels\":{\"release\":{}}}",
        "{\"gray\":1}", "{\"gray\":\"invalid\"}", "{\"healthCheckEnabled\":null}", "{\"warmup\":1.5}"})
    public void testInvalidUpdateDoesNotPublishPartialState(final String invalidProps) {
        publish(List.of(instance("backend:8080", "{\"gray\":true,\"labels\":{\"release\":\"canary\"}}")));
        final List<Upstream> before = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler");
        assertThrows(RuntimeException.class, () -> publish(List.of(instance("backend:8080", "{}"), instance("bad:8080", invalidProps))));
        assertSame(before, UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler"));
        assertEquals(1, before.size());
        assertTrue(before.get(0).isGray());
        assertEquals(Map.of("release", "canary"), before.get(0).getMetadata());
    }

    @Test
    public void testMissingPropsKeepLegacyDefaults() {
        publish(List.of(instance("backend:8080", null)));
        Upstream upstream = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler").get(0);
        assertEquals(10, upstream.getWarmup());
        assertFalse(upstream.isGray());
        assertTrue(upstream.isHealthCheckEnabled());
        assertTrue(upstream.getMetadata().isEmpty());
    }

    @Test
    public void testLabelsDoNotFilterInstancesWithoutLegacyGray() {
        publish(List.of(instance("canary:8080", "{\"labels\":{\"release\":\"canary\"}}"),
                instance("stable:8080", "{\"labels\":{\"release\":\"stable\"}}")));
        List<Upstream> upstreams = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler");
        assertEquals(2, upstreams.size());
        assertEquals(Map.of("release", "canary"), upstreams.get(0).getMetadata());
        assertEquals(Map.of("release", "stable"), upstreams.get(1).getMetadata());
    }

    @Test
    public void testLabelDeletionAlsoUpdatesUnhealthyInstance() {
        publish(List.of(instance("backend:8080", "{\"labels\":{\"release\":\"canary\"}}")));
        final Upstream original = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId("handler").get(0);
        final UpstreamCheckTask task = (UpstreamCheckTask) ReflectionTestUtils.getField(UpstreamCacheManager.getInstance(), "task");
        original.setHealthy(false);
        original.setLastUnhealthyTimestamp(123);
        original.getSucceeded().set(17);
        task.putToMap(task.getUnhealthyUpstream(), "handler", original);
        task.removeFromMap(task.getHealthyUpstream(), "handler", original);
        publish(List.of(instance("backend:8080", "{}")));
        assertSame(original, task.getUnhealthyUpstream().get("handler").get(0));
        assertTrue(original.getMetadata().isEmpty());
        assertFalse(original.isHealthy());
        assertEquals(123, original.getLastUnhealthyTimestamp());
        assertEquals(17, original.getSucceeded().get());
    }

    private DiscoveryUpstreamData instance(final String url, final String props) {
        return DiscoveryUpstreamData.builder().protocol("http://").url(url).status(0).props(props).build();
    }

    private void publish(final List<DiscoveryUpstreamData> upstreams) {
        when(discoverySyncData.getUpstreamDataList()).thenReturn(upstreams);
        divideUpstreamDataHandler.handlerDiscoveryUpstreamData(discoverySyncData);
    }

}
