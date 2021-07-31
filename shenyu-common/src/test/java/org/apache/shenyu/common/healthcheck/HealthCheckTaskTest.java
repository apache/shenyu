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

package org.apache.shenyu.common.healthcheck;

import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.awaitility.Awaitility;
import org.junit.Test;

import java.util.concurrent.TimeUnit;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.junit.Assert.assertTrue;

/**
 * Test Cases for HealthCheckTask.
 */
@Slf4j
public class HealthCheckTaskTest {

    /**
     *  Here to set interval with 50s to avoid running the second time.
     */
    private HealthCheckTask healthCheckTask = new HealthCheckTask(50000);

    @Test(timeout = 30000)
    @SneakyThrows
    public void testRun() {
        // Mock selectorId1~selectorId4 to let it coverage 4 branch of `HealthCheckTask#check` method
        final String selectorId1 = "s1";
        SelectorData selectorData1 = mock(SelectorData.class);
        final String selectorId2 = "s2";
        SelectorData selectorData2 = mock(SelectorData.class);
        final String selectorId3 = "s3";
        SelectorData selectorData3 = mock(SelectorData.class);
        final String selectorId4 = "s4";
        SelectorData selectorData4 = mock(SelectorData.class);
        DivideUpstream divideUpstream = mock(DivideUpstream.class);
        when(selectorData1.getId()).thenReturn(selectorId1);
        when(selectorData2.getId()).thenReturn(selectorId2);
        when(selectorData3.getId()).thenReturn(selectorId3);
        when(selectorData4.getId()).thenReturn(selectorId4);

        // Let it coverage line 165~175
        // We should use powermock or mockito to mock static method of `UpstreamCheckUtils.checkUrl`,
        // But mocked static method is not valid across thread. Because `UpstreamCheckUtils.checkUrl` is called in
        // HealthCheckTask inner thread pool, but mocked in current thread. So we turn to do like below.
        when(divideUpstream.getUpstreamUrl()).thenReturn("");
        when(divideUpstream.isHealthy()).thenReturn(true).thenReturn(false);

        healthCheckTask.triggerAddOne(selectorData1, divideUpstream);
        healthCheckTask.triggerAddOne(selectorData2, divideUpstream);
        healthCheckTask.triggerAddOne(selectorData3, divideUpstream);
        healthCheckTask.triggerAddOne(selectorData4, divideUpstream);
        healthCheckTask.schedule();
        // Wait for the upstream-health-check thread to start.
        Awaitility.await().pollDelay(3, TimeUnit.SECONDS).untilAsserted(() -> {
            assertFalse(healthCheckTask.getCheckStarted().get());
        });
        assertTrue(healthCheckTask.getHealthyUpstream().containsKey(selectorId1));
        // Let it coverage line 151~163
        when(divideUpstream.isHealthy()).thenReturn(false).thenReturn(true);
        // Even if the address could not connect, it will return false, that mean it will not coverage 151~163.
        when(divideUpstream.getUpstreamUrl()).thenReturn("http://www.baidu.com");
        // Manually run one time
        healthCheckTask.run();
        Awaitility.await().pollDelay(1, TimeUnit.SECONDS).untilAsserted(() -> {
            assertFalse(healthCheckTask.getCheckStarted().get());
        });
        assertTrue(healthCheckTask.getUnhealthyUpstream().containsKey(selectorId1));
    }

    @Test
    public void testTriggerRemoveOne() {
        final String selectorId = "s1";
        SelectorData selectorData = mock(SelectorData.class);
        DivideUpstream divideUpstream = mock(DivideUpstream.class);
        when(selectorData.getId()).thenReturn(selectorId);
        healthCheckTask.triggerAddOne(selectorData, divideUpstream);
        healthCheckTask.triggerRemoveOne(selectorData, divideUpstream);
        assertTrue(healthCheckTask.getSelectorCache().containsKey(selectorId));
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(0));

        healthCheckTask.triggerAddOne(selectorData, divideUpstream);
        healthCheckTask.triggerRemoveOne(selectorId, divideUpstream);
        assertTrue(healthCheckTask.getSelectorCache().containsKey(selectorId));
        assertThat(healthCheckTask.getHealthyUpstream().get(selectorId).size(), is(0));
        healthCheckTask.getSelectorCache().clear();
    }

    @Test
    public void testTriggerRemoveAll() {
        final String selectorId = "s1";
        SelectorData selectorData = mock(SelectorData.class);
        DivideUpstream divideUpstream = mock(DivideUpstream.class);
        when(selectorData.getId()).thenReturn(selectorId);
        healthCheckTask.triggerAddOne(selectorData, divideUpstream);
        healthCheckTask.triggerRemoveAll(selectorId);
        assertFalse(healthCheckTask.getSelectorCache().containsKey(selectorId));
        assertFalse(healthCheckTask.getHealthyUpstream().containsKey(selectorId));

        healthCheckTask.triggerAddOne(selectorData, divideUpstream);
        healthCheckTask.triggerRemoveAll(selectorData);
        assertFalse(healthCheckTask.getSelectorCache().containsKey(selectorId));
        assertFalse(healthCheckTask.getHealthyUpstream().containsKey(selectorId));
    }
}
