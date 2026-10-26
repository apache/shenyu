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

import org.apache.shenyu.admin.config.properties.ObservabilityProperties;
import org.apache.shenyu.admin.model.vo.InstanceDiagnosticsVO;
import org.apache.shenyu.admin.model.vo.InstanceDiagnosticsVO.ObservabilityLink;
import org.apache.shenyu.admin.model.vo.InstanceInfoVO;
import org.apache.shenyu.admin.service.InstanceInfoService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import java.util.List;
import java.util.Objects;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;

@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class InstanceDiagnosticsServiceTest {

    private static final String INSTANCE_ID = "instance-1";

    private static final String INSTANCE_KEY = "10.0.0.1:9195@bootstrap#ns-1";

    private static final long BOOT_TIME = 1_700_000_000_000L;

    @Mock
    private InstanceInfoService instanceInfoService;

    @Mock
    private InstanceCheckService instanceCheckService;

    private ObservabilityProperties properties;

    private InstanceDiagnosticsService service;

    @BeforeEach
    public void setUp() {
        properties = new ObservabilityProperties();
        service = new InstanceDiagnosticsService(instanceInfoService, instanceCheckService, properties);
        given(instanceCheckService.getInstanceKey(any(InstanceInfoVO.class))).willReturn(INSTANCE_KEY);
    }

    @Test
    public void testOneInstanceCorrelatesAcrossMetricsLogsAndTraces() {
        properties.setMetricsUrlTemplate("http://grafana:3000/d/gw?var-namespace={namespace}&var-instance={instanceIp}:{instancePort}&from={startTime}&to={endTime}");
        properties.setLogsUrlTemplate("http://kibana:5601/app/logs#/?query=namespace:{namespace} AND instance:{instanceIp} AND type:{instanceType}&since={startTimeIso}");
        properties.setTraceUrlTemplate("http://jaeger:16686/search?service={instanceType}&tags={namespace}&start={startTime}000&end={endTime}000&boot={bootTime}");
        given(instanceInfoService.findById(INSTANCE_ID)).willReturn(buildInstanceInfo(BOOT_TIME));

        InstanceDiagnosticsVO diagnostics = service.getInstanceDiagnostics(INSTANCE_ID, 1_700_000_100_000L, 1_700_000_200_000L);

        assertNotNull(diagnostics);
        assertEquals(INSTANCE_ID, diagnostics.getInstanceId());
        assertEquals(INSTANCE_KEY, diagnostics.getInstanceKey());
        assertEquals("ns-1", diagnostics.getNamespaceId());
        assertEquals(BOOT_TIME, diagnostics.getBootTime());
        assertEquals(1_700_000_100_000L, diagnostics.getStartTime());
        assertEquals(1_700_000_200_000L, diagnostics.getEndTime());
        // every link carries the same documented correlation identifiers
        for (ObservabilityLink link : diagnostics.getLinks()) {
            assertEquals(ObservabilityLink.STATE_READY, link.getState());
            assertThat(link.getUrl(), containsString("ns-1"));
        }
        assertEquals("http://grafana:3000/d/gw?var-namespace=ns-1&var-instance=10.0.0.1:9195&from=1700000100000&to=1700000200000",
                linkOf(diagnostics, ObservabilityLink.TYPE_METRICS).getUrl());
        assertThat(linkOf(diagnostics, ObservabilityLink.TYPE_LOGS).getUrl(), containsString("instance:10.0.0.1"));
        assertThat(linkOf(diagnostics, ObservabilityLink.TYPE_LOGS).getUrl(), containsString("namespace:ns-1"));
        assertThat(linkOf(diagnostics, ObservabilityLink.TYPE_LOGS).getUrl(), not(containsString(" ")));
        assertThat(linkOf(diagnostics, ObservabilityLink.TYPE_TRACE).getUrl(), containsString("boot=1700000000000"));
    }

    @Test
    public void testNamespaceAndTimeRangeAreUrlEncoded() {
        properties.setMetricsUrlTemplate("http://grafana:3000/d/gw?var-namespace={namespace}&from={startTime}&to={endTime}");
        InstanceInfoVO instanceInfoVO = buildInstanceInfo(BOOT_TIME);
        instanceInfoVO.setNamespaceId("ns with space/slash&special=chars");
        given(instanceInfoService.findById(INSTANCE_ID)).willReturn(instanceInfoVO);

        InstanceDiagnosticsVO diagnostics = service.getInstanceDiagnostics(INSTANCE_ID, null, null);

        ObservabilityLink metrics = linkOf(diagnostics, ObservabilityLink.TYPE_METRICS);
        assertEquals(ObservabilityLink.STATE_READY, metrics.getState());
        assertThat(metrics.getUrl(), containsString("var-namespace=ns+with+space%2Fslash%26special%3Dchars"));
        assertThat(metrics.getUrl(), not(containsString("ns with space/slash&special=chars")));
    }

    @Test
    public void testMissingBackendConfigurationIsReportedAsNotConfigured() {
        given(instanceInfoService.findById(INSTANCE_ID)).willReturn(buildInstanceInfo(BOOT_TIME));

        InstanceDiagnosticsVO diagnostics = service.getInstanceDiagnostics(INSTANCE_ID, null, null);

        assertNotNull(diagnostics);
        for (ObservabilityLink link : diagnostics.getLinks()) {
            assertEquals(ObservabilityLink.STATE_NOT_CONFIGURED, link.getState());
            assertNull(link.getUrl());
        }
    }

    @Test
    public void testLegacyInstanceWithoutCorrelationFields() {
        properties.setMetricsUrlTemplate("http://grafana:3000/d/gw?var-instance={instanceIp}:{instancePort}&from={startTime}&to={endTime}");
        properties.setTraceUrlTemplate("http://jaeger:16686/search?boot={bootTime}");
        // legacy instance info without bootTime
        given(instanceInfoService.findById(INSTANCE_ID)).willReturn(buildInstanceInfo(null));

        InstanceDiagnosticsVO diagnostics = service.getInstanceDiagnostics(INSTANCE_ID, null, null);

        assertNotNull(diagnostics);
        assertNull(diagnostics.getBootTime());
        // links that do not need the boot identity still work for legacy instances
        assertEquals(ObservabilityLink.STATE_READY, linkOf(diagnostics, ObservabilityLink.TYPE_METRICS).getState());
        // links that need the missing correlation field are reported distinctly, not fabricated
        ObservabilityLink trace = linkOf(diagnostics, ObservabilityLink.TYPE_TRACE);
        assertEquals(ObservabilityLink.STATE_MISSING_INSTANCE_DATA, trace.getState());
        assertNull(trace.getUrl());
    }

    @Test
    public void testUnparsableInstanceInfoDoesNotBreakDiagnostics() {
        properties.setMetricsUrlTemplate("http://grafana:3000/d/gw?var-instance={instanceIp}");
        InstanceInfoVO instanceInfoVO = buildInstanceInfo(null);
        instanceInfoVO.setInstanceInfo("not-a-json");
        given(instanceInfoService.findById(INSTANCE_ID)).willReturn(instanceInfoVO);

        InstanceDiagnosticsVO diagnostics = service.getInstanceDiagnostics(INSTANCE_ID, null, null);

        assertNotNull(diagnostics);
        assertNull(diagnostics.getBootTime());
        assertEquals(ObservabilityLink.STATE_READY, linkOf(diagnostics, ObservabilityLink.TYPE_METRICS).getState());
    }

    @Test
    public void testCredentialsAndNonHttpTemplatesAreRejected() {
        properties.setMetricsUrlTemplate("http://user:secret@grafana:3000/d/gw?var-instance={instanceIp}");
        properties.setLogsUrlTemplate("javascript:alert({instanceIp})");
        properties.setTraceUrlTemplate("http://jaeger:16686/search?instance={instanceIp");
        given(instanceInfoService.findById(INSTANCE_ID)).willReturn(buildInstanceInfo(BOOT_TIME));

        InstanceDiagnosticsVO diagnostics = service.getInstanceDiagnostics(INSTANCE_ID, null, null);

        assertEquals(ObservabilityLink.STATE_INVALID_TEMPLATE, linkOf(diagnostics, ObservabilityLink.TYPE_METRICS).getState());
        assertEquals(ObservabilityLink.STATE_INVALID_TEMPLATE, linkOf(diagnostics, ObservabilityLink.TYPE_LOGS).getState());
        assertEquals(ObservabilityLink.STATE_INVALID_TEMPLATE, linkOf(diagnostics, ObservabilityLink.TYPE_TRACE).getState());
        for (ObservabilityLink link : diagnostics.getLinks()) {
            assertNull(link.getUrl());
        }
    }

    @Test
    public void testInstanceNotFound() {
        given(instanceInfoService.findById("missing")).willReturn(null);
        assertNull(service.getInstanceDiagnostics("missing", null, null));
    }

    @Test
    public void testDefaultTimeWindowAndLiveStateMerge() {
        properties.setMetricsUrlTemplate("http://grafana:3000/d/gw?from={startTime}&to={endTime}");
        given(instanceInfoService.findById(INSTANCE_ID)).willReturn(buildInstanceInfo(null));
        InstanceInfoVO beatInfo = new InstanceInfoVO();
        beatInfo.setInstanceState(1);
        beatInfo.setLastHeartBeatTime(1_700_000_300_000L);
        given(instanceCheckService.getInstanceHealthBeatInfo(INSTANCE_KEY)).willReturn(beatInfo);

        long before = System.currentTimeMillis();
        InstanceDiagnosticsVO diagnostics = service.getInstanceDiagnostics(INSTANCE_ID, null, null);

        // live state from the heartbeat cache wins over the persisted row
        assertEquals(1, diagnostics.getInstanceState());
        assertEquals(1_700_000_300_000L, diagnostics.getLastHeartBeatTime());
        // default window is the last 15 minutes ending at now
        assertTrue(diagnostics.getEndTime() >= before);
        assertEquals(15 * 60 * 1000L, diagnostics.getEndTime() - diagnostics.getStartTime());
        assertThat(linkOf(diagnostics, ObservabilityLink.TYPE_METRICS).getUrl(),
                containsString("from=" + diagnostics.getStartTime() + "&to=" + diagnostics.getEndTime()));
    }

    @Test
    public void testDefaultWindowStartsAtBootTimeWhenRecent() {
        long bootTime = System.currentTimeMillis() - 60_000L;
        given(instanceInfoService.findById(INSTANCE_ID)).willReturn(buildInstanceInfo(bootTime));

        InstanceDiagnosticsVO diagnostics = service.getInstanceDiagnostics(INSTANCE_ID, null, null);

        assertEquals(bootTime, diagnostics.getStartTime());
        assertTrue(diagnostics.getEndTime() > diagnostics.getStartTime());
    }

    private ObservabilityLink linkOf(final InstanceDiagnosticsVO diagnostics, final String type) {
        List<ObservabilityLink> links = diagnostics.getLinks().stream().filter(link -> type.equals(link.getType())).toList();
        assertEquals(1, links.size());
        return links.get(0);
    }

    private InstanceInfoVO buildInstanceInfo(final Long bootTime) {
        InstanceInfoVO instanceInfoVO = new InstanceInfoVO();
        instanceInfoVO.setInstanceIp("10.0.0.1");
        instanceInfoVO.setInstancePort("9195");
        instanceInfoVO.setInstanceType("bootstrap");
        instanceInfoVO.setNamespaceId("ns-1");
        instanceInfoVO.setInstanceState(0);
        instanceInfoVO.setLastHeartBeatTime(1_700_000_050_000L);
        instanceInfoVO.setInstanceInfo(Objects.isNull(bootTime)
                ? "{\"arch\":\"x86_64\",\"operatingSystem\":\"macOS\"}"
                : "{\"arch\":\"x86_64\",\"operatingSystem\":\"macOS\",\"bootTime\":" + bootTime + "}");
        return instanceInfoVO;
    }
}
