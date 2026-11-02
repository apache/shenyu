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

import com.google.common.collect.Maps;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * In-memory repository correlating Kubernetes resources (Kind, namespace, name, UID, generation)
 * with active runtime gateway configuration (selectors, rules) and bounded failure diagnostics.
 */
public final class K8sResourceCorrelationCache {

    private static final K8sResourceCorrelationCache INSTANCE = new K8sResourceCorrelationCache();

    /** resourceKey ("kind/namespace/name") -> correlation. */
    private final Map<String, K8sResourceCorrelation> correlations = Maps.newConcurrentMap();

    /** selectorId -> resourceKey. */
    private final Map<String, String> selectorToResourceMap = Maps.newConcurrentMap();

    /** ruleId -> resourceKey. */
    private final Map<String, String> ruleToResourceMap = Maps.newConcurrentMap();

    private K8sResourceCorrelationCache() {
    }

    /**
     * Gets singleton instance.
     *
     * @return the cache instance
     */
    public static K8sResourceCorrelationCache getInstance() {
        return INSTANCE;
    }

    /**
     * Formats resource key.
     *
     * @param kind resource kind
     * @param namespace resource namespace
     * @param name resource name
     * @return resource key
     */
    public static String buildResourceKey(final String kind, final String namespace, final String name) {
        return kind + "/" + namespace + "/" + name;
    }

    /**
     * Records an accepted and applied Kubernetes resource.
     *
     * @param kind resource kind
     * @param namespace resource namespace
     * @param name resource name
     * @param uid resource UID
     * @param generation resource generation
     * @param resourceVersion resource version
     * @param parentGateways parent gateways
     * @param selectorIds generated selector IDs
     * @param ruleIds generated rule IDs
     * @return recorded correlation
     */
    public K8sResourceCorrelation recordAccepted(final String kind,
                                                 final String namespace,
                                                 final String name,
                                                 final String uid,
                                                 final Long generation,
                                                 final String resourceVersion,
                                                 final Set<String> parentGateways,
                                                 final List<String> selectorIds,
                                                 final List<String> ruleIds) {
        String key = buildResourceKey(kind, namespace, name);
        // Clean previous selector/rule mappings for this resource
        removeIndexMappings(key);

        K8sResourceCorrelation correlation = K8sResourceCorrelation.builder()
                .kind(kind)
                .namespace(namespace)
                .name(name)
                .uid(uid)
                .generation(generation)
                .resourceVersion(resourceVersion)
                .state(K8sResourceState.ACCEPTED)
                .parentGateways(parentGateways)
                .selectorIds(selectorIds)
                .ruleIds(ruleIds)
                .lastReconciledTime(System.currentTimeMillis())
                .build();

        correlations.put(key, correlation);
        if (Objects.nonNull(selectorIds)) {
            for (String selectorId : selectorIds) {
                selectorToResourceMap.put(selectorId, key);
            }
        }
        if (Objects.nonNull(ruleIds)) {
            for (String ruleId : ruleIds) {
                ruleToResourceMap.put(ruleId, key);
            }
        }
        return correlation;
    }

    /**
     * Records a rejected Kubernetes resource with a bounded failure reason.
     *
     * @param kind resource kind
     * @param namespace resource namespace
     * @param name resource name
     * @param uid resource UID
     * @param generation resource generation
     * @param resourceVersion resource version
     * @param parentGateways parent gateways
     * @param rejectionReason bounded failure reason
     * @param rejectionMessage bounded failure message
     * @return recorded correlation
     */
    public K8sResourceCorrelation recordRejected(final String kind,
                                                 final String namespace,
                                                 final String name,
                                                 final String uid,
                                                 final Long generation,
                                                 final String resourceVersion,
                                                 final Set<String> parentGateways,
                                                 final String rejectionReason,
                                                 final String rejectionMessage) {
        String key = buildResourceKey(kind, namespace, name);
        removeIndexMappings(key);

        K8sResourceCorrelation correlation = K8sResourceCorrelation.builder()
                .kind(kind)
                .namespace(namespace)
                .name(name)
                .uid(uid)
                .generation(generation)
                .resourceVersion(resourceVersion)
                .state(K8sResourceState.REJECTED)
                .parentGateways(parentGateways)
                .rejectionReason(rejectionReason)
                .rejectionMessage(rejectionMessage)
                .selectorIds(Collections.emptyList())
                .ruleIds(Collections.emptyList())
                .lastReconciledTime(System.currentTimeMillis())
                .build();

        correlations.put(key, correlation);
        return correlation;
    }

    /**
     * Records a deleted Kubernetes resource.
     *
     * @param kind resource kind
     * @param namespace resource namespace
     * @param name resource name
     * @return removed correlation if existed
     */
    public K8sResourceCorrelation recordDeleted(final String kind,
                                                final String namespace,
                                                final String name) {
        String key = buildResourceKey(kind, namespace, name);
        removeIndexMappings(key);
        return correlations.remove(key);
    }

    /**
     * Gets correlation by resource coordinates.
     *
     * @param kind resource kind
     * @param namespace resource namespace
     * @param name resource name
     * @return correlation if present
     */
    public K8sResourceCorrelation getCorrelation(final String kind,
                                                 final String namespace,
                                                 final String name) {
        return correlations.get(buildResourceKey(kind, namespace, name));
    }

    /**
     * Reverse lookup: finds the Kubernetes resource that generated a ShenYu selector.
     *
     * @param selectorId the selector ID
     * @return correlation if present
     */
    public K8sResourceCorrelation findCorrelationBySelectorId(final String selectorId) {
        String key = selectorToResourceMap.get(selectorId);
        return Objects.isNull(key) ? null : correlations.get(key);
    }

    /**
     * Reverse lookup: finds the Kubernetes resource that generated a ShenYu rule.
     *
     * @param ruleId the rule ID
     * @return correlation if present
     */
    public K8sResourceCorrelation findCorrelationByRuleId(final String ruleId) {
        String key = ruleToResourceMap.get(ruleId);
        return Objects.isNull(key) ? null : correlations.get(key);
    }

    /**
     * Gets all active (ACCEPTED) resource correlations.
     *
     * @return list of active correlations
     */
    public List<K8sResourceCorrelation> getActiveCorrelations() {
        return correlations.values().stream()
                .filter(c -> c.getState() == K8sResourceState.ACCEPTED)
                .collect(Collectors.toList());
    }

    /**
     * Gets all rejected resource correlations.
     *
     * @return list of rejected correlations
     */
    public List<K8sResourceCorrelation> getRejectedCorrelations() {
        return correlations.values().stream()
                .filter(c -> c.getState() == K8sResourceState.REJECTED)
                .collect(Collectors.toList());
    }

    /**
     * Gets all correlations.
     *
     * @return all correlations
     */
    public Collection<K8sResourceCorrelation> getAllCorrelations() {
        return Collections.unmodifiableCollection(correlations.values());
    }

    /**
     * Clears all cached correlations.
     */
    public void clear() {
        correlations.clear();
        selectorToResourceMap.clear();
        ruleToResourceMap.clear();
    }

    private void removeIndexMappings(final String resourceKey) {
        K8sResourceCorrelation previous = correlations.get(resourceKey);
        if (Objects.nonNull(previous)) {
            for (String selectorId : previous.getSelectorIds()) {
                selectorToResourceMap.remove(selectorId, resourceKey);
            }
            for (String ruleId : previous.getRuleIds()) {
                ruleToResourceMap.remove(ruleId, resourceKey);
            }
        }
    }
}
