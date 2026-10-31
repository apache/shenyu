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

package org.apache.shenyu.k8s.observability;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

/**
 * Test cases for Kubernetes controller observability, resource correlation cache,
 * and diagnostic summary generator.
 */
public final class K8sObservabilityTest {

    private final K8sResourceCorrelationCache correlationCache = K8sResourceCorrelationCache.getInstance();

    private final K8sControllerMetrics controllerMetrics = K8sControllerMetrics.getInstance();

    @BeforeEach
    public void setUp() {
        correlationCache.clear();
        controllerMetrics.resetForTesting();
    }

    /**
     * Test recording accepted resource correlation and finding by selector and rule.
     */
    @Test
    public void testRecordAcceptedCorrelation() {
        final String kind = "HTTPRoute";
        final String namespace = "default";
        final String name = "order-route";
        final String uid = "uid-12345";
        final Long generation = 1L;
        final String resourceVersion = "v1";
        final Set<String> parents = Set.of("default/gateway-1");
        final List<String> selectors = List.of("sel-1", "sel-2");
        final List<String> rules = List.of("rule-1");

        final K8sResourceCorrelation correlation = correlationCache.recordAccepted(
                kind, namespace, name, uid, generation, resourceVersion, parents, selectors, rules);

        Assertions.assertEquals(K8sResourceState.ACCEPTED, correlation.getState());
        Assertions.assertEquals(uid, correlation.getUid());
        Assertions.assertEquals(generation, correlation.getGeneration());
        Assertions.assertEquals(selectors, correlation.getSelectorIds());
        Assertions.assertEquals(rules, correlation.getRuleIds());

        final K8sResourceCorrelation bySelector = correlationCache.findCorrelationBySelectorId("sel-1");
        Assertions.assertTrue(Objects.nonNull(bySelector));
        Assertions.assertEquals(name, bySelector.getName());

        final K8sResourceCorrelation byRule = correlationCache.findCorrelationByRuleId("rule-1");
        Assertions.assertTrue(Objects.nonNull(byRule));
        Assertions.assertEquals(name, byRule.getName());
    }

    /**
     * Test recording rejected resource correlation and state inspection.
     */
    @Test
    public void testRecordRejectedCorrelation() {
        final String kind = "HTTPRoute";
        final String namespace = "prod";
        final String name = "bad-route";
        final String uid = "uid-67890";
        final Long generation = 2L;
        final String resourceVersion = "v2";
        final Set<String> parents = Set.of("prod/gateway-main");
        final String reason = "UnsupportedFilters";
        final String message = "HTTPRoute specifies unsupported filters";

        final K8sResourceCorrelation correlation = correlationCache.recordRejected(
                kind, namespace, name, uid, generation, resourceVersion, parents, reason, message);

        Assertions.assertEquals(K8sResourceState.REJECTED, correlation.getState());
        Assertions.assertEquals(reason, correlation.getRejectionReason());
        Assertions.assertEquals(message, correlation.getRejectionMessage());

        final K8sResourceCorrelation found = correlationCache.getCorrelation(kind, namespace, name);
        Assertions.assertTrue(Objects.nonNull(found));
        Assertions.assertEquals(K8sResourceState.REJECTED, found.getState());
    }

    /**
     * Test recording deletion transitions resource to DELETED state and removes secondary indexes.
     */
    @Test
    public void testRecordDeletedCorrelation() {
        final String kind = "HTTPRoute";
        final String namespace = "default";
        final String name = "temp-route";

        correlationCache.recordAccepted(kind, namespace, name, "uid-temp", 1L, "v1",
                Set.of("default/gw"), List.of("sel-temp"), List.of("rule-temp"));

        correlationCache.recordDeleted(kind, namespace, name);

        final K8sResourceCorrelation found = correlationCache.getCorrelation(kind, namespace, name);
        Assertions.assertTrue(Objects.isNull(found));

        final K8sResourceCorrelation bySelector = correlationCache.findCorrelationBySelectorId("sel-temp");
        Assertions.assertTrue(Objects.isNull(bySelector));
    }

    /**
     * Test controller metrics counters and readiness tracking.
     */
    @Test
    public void testControllerMetricsRecording() {
        controllerMetrics.recordReconcile("HTTPRoute", "success", 1_000_000L);
        controllerMetrics.recordReconcile("HTTPRoute", "success", 2_000_000L);
        controllerMetrics.recordReconcile("HTTPRoute", "rejected", 500_000L);
        controllerMetrics.recordReconcileError("HTTPRoute", "NullPointerException");
        controllerMetrics.recordRejectedRoute("HTTPRoute", "ParentNotFound");
        controllerMetrics.updateWorkQueueDepth("HTTPRouteQueue", 5);
        controllerMetrics.recordWatchRecovery("HTTPRoute");
        controllerMetrics.updateCacheReadiness(true, 0, 0);

        Assertions.assertTrue(controllerMetrics.isCacheReady());
        Assertions.assertEquals(0L, controllerMetrics.getPendingInformers());
        Assertions.assertEquals(0L, controllerMetrics.getPendingWorkItems());

        final Map<String, Long> reconcileCounters = controllerMetrics.getReconcileCountersSnapshot();
        Assertions.assertEquals(2L, reconcileCounters.get("httproute:success"));
        Assertions.assertEquals(1L, reconcileCounters.get("httproute:rejected"));

        final Map<String, Long> rejectedCounters = controllerMetrics.getRejectedRouteCountersSnapshot();
        Assertions.assertEquals(1L, rejectedCounters.get("httproute:parentnotfound"));

        final Map<String, Integer> queueDepths = controllerMetrics.getWorkQueueDepthsSnapshot();
        Assertions.assertEquals(5, queueDepths.get("httproutequeue"));
    }

    /**
     * Test diagnostic summary map generation.
     */
    @Test
    public void testDiagnosticSummaryMapGeneration() {
        controllerMetrics.updateCacheReadiness(true, 0, 0);
        controllerMetrics.recordReconcile("HTTPRoute", "success", 1_500_000L);

        correlationCache.recordAccepted("HTTPRoute", "test", "summary-route",
                "uid-sum", 1L, "v1", Set.of("test/gw"), List.of("s1"), List.of("r1"));

        final Map<String, Object> summary = K8sControllerDiagnosticSummary.generateSummaryMap();
        Assertions.assertNotNull(summary);
        Assertions.assertTrue(summary.containsKey("readiness"));
        Assertions.assertTrue(summary.containsKey("reconciles"));
        Assertions.assertTrue(summary.containsKey("activeRoutes"));
        Assertions.assertTrue(summary.containsKey("rejectedRoutes"));

        final String json = K8sControllerDiagnosticSummary.generateSummaryJson();
        Assertions.assertNotNull(json);
        Assertions.assertTrue(json.contains("summary-route"));
    }
}
