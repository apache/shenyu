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

import org.apache.shenyu.common.utils.GsonUtils;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Builds a bounded, redacted diagnostic snapshot of the Kubernetes controller and
 * resource-to-runtime correlations for gateway diagnostic bundles and operational endpoints.
 */
public final class K8sControllerDiagnosticSummary {

    private static final int MAX_CORRELATION_ENTRIES = 50;

    private K8sControllerDiagnosticSummary() {
    }

    /**
     * Generates a structured map representing the controller diagnostic section.
     *
     * @return diagnostic summary map
     */
    public static Map<String, Object> generateSummaryMap() {
        final K8sControllerMetrics metrics = K8sControllerMetrics.getInstance();
        final K8sResourceCorrelationCache cache = K8sResourceCorrelationCache.getInstance();
        final Map<String, Object> summary = new LinkedHashMap<>();

        // Readiness state
        Map<String, Object> readiness = new LinkedHashMap<>();
        readiness.put("ready", metrics.isCacheReady());
        readiness.put("pendingInformers", metrics.getPendingInformers());
        readiness.put("pendingWorkItems", metrics.getPendingWorkItems());
        summary.put("readiness", readiness);

        // Work queues
        summary.put("workQueues", metrics.getWorkQueueDepthsSnapshot());

        // Reconcile metrics summary
        summary.put("reconciles", metrics.getReconcileCountersSnapshot());
        summary.put("rejectedRouteCounts", metrics.getRejectedRouteCountersSnapshot());

        // Active correlations (bounded)
        List<K8sResourceCorrelation> active = cache.getActiveCorrelations();
        List<Map<String, Object>> activeSummaries = new ArrayList<>();
        int activeLimit = Math.min(active.size(), MAX_CORRELATION_ENTRIES);
        for (int i = 0; i < activeLimit; i++) {
            K8sResourceCorrelation c = active.get(i);
            Map<String, Object> item = new LinkedHashMap<>();
            item.put("kind", c.getKind());
            item.put("namespace", c.getNamespace());
            item.put("name", c.getName());
            item.put("uid", c.getUid());
            item.put("generation", c.getGeneration());
            item.put("state", c.getState().name());
            item.put("parentGateways", c.getParentGateways());
            item.put("selectorIds", c.getSelectorIds());
            item.put("ruleIds", c.getRuleIds());
            activeSummaries.add(item);
        }
        Map<String, Object> activeSection = new LinkedHashMap<>();
        activeSection.put("total", active.size());
        activeSection.put("items", activeSummaries);
        summary.put("activeRoutes", activeSection);

        // Rejected correlations (bounded)
        List<K8sResourceCorrelation> rejected = cache.getRejectedCorrelations();
        List<Map<String, Object>> rejectedSummaries = new ArrayList<>();
        int rejectedLimit = Math.min(rejected.size(), MAX_CORRELATION_ENTRIES);
        for (int i = 0; i < rejectedLimit; i++) {
            K8sResourceCorrelation c = rejected.get(i);
            Map<String, Object> item = new LinkedHashMap<>();
            item.put("kind", c.getKind());
            item.put("namespace", c.getNamespace());
            item.put("name", c.getName());
            item.put("uid", c.getUid());
            item.put("generation", c.getGeneration());
            item.put("state", c.getState().name());
            item.put("rejectionReason", c.getRejectionReason());
            item.put("rejectionMessage", c.getRejectionMessage());
            item.put("parentGateways", c.getParentGateways());
            rejectedSummaries.add(item);
        }
        Map<String, Object> rejectedSection = new LinkedHashMap<>();
        rejectedSection.put("total", rejected.size());
        rejectedSection.put("items", rejectedSummaries);
        summary.put("rejectedRoutes", rejectedSection);

        return summary;
    }

    /**
     * Serializes diagnostic summary map to a JSON string.
     *
     * @return JSON string
     */
    public static String generateSummaryJson() {
        return GsonUtils.getInstance().toJson(generateSummaryMap());
    }
}
