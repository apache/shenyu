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

package org.apache.shenyu.plugin.shadow.dispatch;

import com.github.tomakehurst.wiremock.WireMockServer;
import com.github.tomakehurst.wiremock.client.ResponseDefinitionBuilder;
import org.apache.shenyu.common.dto.convert.rule.ShadowHandle;
import org.apache.shenyu.plugin.shadow.compare.ShadowComparison;
import org.apache.shenyu.plugin.shadow.compare.ShadowObservation;
import org.apache.shenyu.plugin.shadow.compare.ShadowOutcome;
import org.apache.shenyu.plugin.shadow.report.ShadowReporter;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;
import org.springframework.http.HttpHeaders;

import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;

import static com.github.tomakehurst.wiremock.client.WireMock.aResponse;
import static com.github.tomakehurst.wiremock.client.WireMock.equalTo;
import static com.github.tomakehurst.wiremock.client.WireMock.get;
import static com.github.tomakehurst.wiremock.client.WireMock.getRequestedFor;
import static com.github.tomakehurst.wiremock.client.WireMock.urlEqualTo;
import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.awaitility.Awaitility.await;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * test case for {@link ShadowDispatcher}.
 */
public final class ShadowDispatcherTest {

    private static final String PATH = "/api/orders";

    private static final String JSON_BODY = "{\"a\":1}";

    private static final int SLOW_DELAY_MILLIS = 30_000;

    private WireMockServer server;

    @BeforeEach
    public void setUp() {
        server = new WireMockServer(options().dynamicPort());
        server.start();
    }

    @AfterEach
    public void tearDown() {
        server.stop();
    }

    @Test
    public void testMirrorsAndReportsMatch() {
        server.stubFor(get(urlEqualTo(PATH)).willReturn(jsonResponse(200, JSON_BODY)));
        RecordingReporter reporter = new RecordingReporter();
        ShadowDispatcher dispatcher = dispatcher(handle(), reporter);
        try {
            assertTrue(dispatcher.dispatch(task(), primary(200, JSON_BODY)));
            awaitReport(reporter);
            assertEquals(ShadowOutcome.MATCH, reporter.get(0).getOutcome());
            server.verify(getRequestedFor(urlEqualTo(PATH))
                    .withHeader(ShadowTaskFactory.SHADOW_HEADER, equalTo("true")));
        } finally {
            dispatcher.close();
        }
    }

    @Test
    public void testReportsStatusMismatch() {
        server.stubFor(get(urlEqualTo(PATH)).willReturn(jsonResponse(500, JSON_BODY)));
        RecordingReporter reporter = new RecordingReporter();
        ShadowDispatcher dispatcher = dispatcher(handle(), reporter);
        try {
            assertTrue(dispatcher.dispatch(task(), primary(200, JSON_BODY)));
            awaitReport(reporter);
            assertEquals(ShadowOutcome.MISMATCH, reporter.get(0).getOutcome());
        } finally {
            dispatcher.close();
        }
    }

    @Test
    public void testReportsShadowFailureOnTimeout() {
        server.stubFor(get(urlEqualTo(PATH)).willReturn(jsonResponse(200, JSON_BODY).withFixedDelay(SLOW_DELAY_MILLIS)));
        ShadowHandle handle = handle();
        handle.setTimeoutMillis(200);
        RecordingReporter reporter = new RecordingReporter();
        ShadowDispatcher dispatcher = dispatcher(handle, reporter);
        try {
            assertTrue(dispatcher.dispatch(task(PATH, 200), primary(200, JSON_BODY)));
            awaitReport(reporter);
            assertEquals(ShadowOutcome.SHADOW_FAILED, reporter.get(0).getOutcome());
        } finally {
            dispatcher.close();
        }
    }

    @Test
    @Timeout(10)
    public void testDispatchDoesNotWaitForSlowShadow() {
        // The shadow target answers far later than this test may take. Completing
        // at all proves the caller never waits for the shadow response.
        server.stubFor(get(urlEqualTo(PATH)).willReturn(jsonResponse(200, JSON_BODY)
                .withFixedDelay(SLOW_DELAY_MILLIS)));
        ShadowDispatcher dispatcher = dispatcher(handle(), new RecordingReporter());
        try {
            assertTrue(dispatcher.dispatch(task(), primary(200, JSON_BODY)));
        } finally {
            dispatcher.close();
        }
    }

    @Test
    public void testDropsWhenQueueIsFull() {
        server.stubFor(get(urlEqualTo(PATH)).willReturn(jsonResponse(200, JSON_BODY).withFixedDelay(1000)));
        ShadowHandle handle = handle();
        handle.setMaxConcurrent(1);
        handle.setQueueCapacity(1);
        ShadowDispatcher dispatcher = dispatcher(handle, new RecordingReporter());
        try {
            boolean dropped = false;
            for (int i = 0; i < 5; i++) {
                dropped = dropped || !dispatcher.dispatch(task(), primary(200, JSON_BODY));
            }
            assertTrue(dropped);
        } finally {
            dispatcher.close();
        }
    }

    @Test
    public void testPairsEachShadowResponseWithItsOwnPrimary() {
        String pathA = "/api/a";
        String pathB = "/api/b";
        server.stubFor(get(urlEqualTo(pathA)).willReturn(jsonResponse(200, "{\"a\":1}")));
        server.stubFor(get(urlEqualTo(pathB)).willReturn(jsonResponse(500, "{\"b\":2}")));
        RecordingReporter reporter = new RecordingReporter();
        ShadowDispatcher dispatcher = dispatcher(handle(), reporter);
        try {
            assertTrue(dispatcher.dispatch(task(pathA, 2000), primary(200, "{\"a\":1}")));
            assertTrue(dispatcher.dispatch(task(pathB, 2000), primary(500, "{\"b\":3}")));
            await().atMost(Duration.ofSeconds(10)).until(() -> reporter.size() == 2);
            assertEquals(Set.of("200:200", "500:500"), reporter.statusPairs());
            assertTrue(reporter.outcomes().contains(ShadowOutcome.MATCH));
            assertTrue(reporter.outcomes().contains(ShadowOutcome.MISMATCH));
        } finally {
            dispatcher.close();
        }
    }

    @Test
    public void testCloseRejectsFurtherWork() {
        ShadowDispatcher dispatcher = dispatcher(handle(), new RecordingReporter());
        dispatcher.close();
        assertFalse(dispatcher.dispatch(task(), primary(200, JSON_BODY)));
    }

    private ShadowDispatcher dispatcher(final ShadowHandle handle, final RecordingReporter reporter) {
        return new ShadowDispatcher(ShadowWebClientFactory.create(), handle, "selector_rule", reporter);
    }

    private ShadowHandle handle() {
        ShadowHandle handle = new ShadowHandle();
        handle.setEnabled(true);
        handle.setTarget("http://localhost:" + server.port());
        handle.setSampleRate(1D);
        handle.setTimeoutMillis(2000);
        return handle;
    }

    private ShadowTask task() {
        return task(PATH, 2000);
    }

    private ShadowTask task(final String path, final int timeoutMillis) {
        HttpHeaders headers = new HttpHeaders();
        headers.set(ShadowTaskFactory.SHADOW_HEADER, Boolean.TRUE.toString());
        return new ShadowTask("GET", URI.create("http://localhost:" + server.port() + path),
                headers, new byte[0], timeoutMillis);
    }

    private ShadowObservation primary(final int status, final String body) {
        HttpHeaders headers = new HttpHeaders();
        headers.set(HttpHeaders.CONTENT_TYPE, "application/json");
        return new ShadowObservation(status, headers, body.getBytes(StandardCharsets.UTF_8), false, 0L);
    }

    private ResponseDefinitionBuilder jsonResponse(final int status, final String body) {
        return aResponse().withStatus(status).withHeader("Content-Type", "application/json").withBody(body);
    }

    private void awaitReport(final RecordingReporter reporter) {
        await().atMost(Duration.ofSeconds(10)).until(() -> reporter.size() == 1);
    }

    private static final class RecordingReporter implements ShadowReporter {

        private final List<ShadowComparison> comparisons = new CopyOnWriteArrayList<>();

        private final List<String> statusPairs = new CopyOnWriteArrayList<>();

        @Override
        public void report(final String ruleId, final ShadowComparison comparison,
                           final ShadowObservation primary, final ShadowObservation shadow) {
            comparisons.add(comparison);
            statusPairs.add(primary.getStatus() + ":" + shadow.getStatus());
        }

        private Set<String> statusPairs() {
            return new HashSet<>(statusPairs);
        }

        private int size() {
            return comparisons.size();
        }

        private ShadowComparison get(final int index) {
            return comparisons.get(index);
        }

        private List<ShadowOutcome> outcomes() {
            return comparisons.stream().map(ShadowComparison::getOutcome).collect(Collectors.toList());
        }
    }
}
