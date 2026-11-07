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

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Set;

/**
 * Correlates a Kubernetes custom resource (e.g. HTTPRoute, Ingress) with generated ShenYu gateway
 * selectors, rules, parent gateways, and bounded failure diagnostics.
 */
public final class K8sResourceCorrelation {

    private final String kind;

    private final String namespace;

    private final String name;

    private final String uid;

    private final Long generation;

    private final String resourceVersion;

    private final K8sResourceState state;

    private final String rejectionReason;

    private final String rejectionMessage;

    private final Set<String> parentGateways;

    private final List<String> selectorIds;

    private final List<String> ruleIds;

    private final long lastReconciledTime;

    private K8sResourceCorrelation(final Builder builder) {
        this.kind = builder.kind;
        this.namespace = builder.namespace;
        this.name = builder.name;
        this.uid = builder.uid;
        this.generation = builder.generation;
        this.resourceVersion = builder.resourceVersion;
        this.state = builder.state;
        this.rejectionReason = builder.rejectionReason;
        this.rejectionMessage = builder.rejectionMessage;
        this.parentGateways = Objects.isNull(builder.parentGateways) ? Collections.emptySet() : Set.copyOf(builder.parentGateways);
        this.selectorIds = Objects.isNull(builder.selectorIds) ? Collections.emptyList() : List.copyOf(builder.selectorIds);
        this.ruleIds = Objects.isNull(builder.ruleIds) ? Collections.emptyList() : List.copyOf(builder.ruleIds);
        this.lastReconciledTime = builder.lastReconciledTime;
    }

    /**
     * Resource key formatted as "kind/namespace/name".
     *
     * @return the resource key
     */
    public String resourceKey() {
        return kind + "/" + namespace + "/" + name;
    }

    /**
     * Gets kind.
     *
     * @return the kind
     */
    public String getKind() {
        return kind;
    }

    /**
     * Gets namespace.
     *
     * @return the namespace
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * Gets name.
     *
     * @return the name
     */
    public String getName() {
        return name;
    }

    /**
     * Gets uid.
     *
     * @return the uid
     */
    public String getUid() {
        return uid;
    }

    /**
     * Gets generation.
     *
     * @return the generation
     */
    public Long getGeneration() {
        return generation;
    }

    /**
     * Gets resource version.
     *
     * @return the resource version
     */
    public String getResourceVersion() {
        return resourceVersion;
    }

    /**
     * Gets state.
     *
     * @return the state
     */
    public K8sResourceState getState() {
        return state;
    }

    /**
     * Gets rejection reason.
     *
     * @return the rejection reason
     */
    public String getRejectionReason() {
        return rejectionReason;
    }

    /**
     * Gets rejection message.
     *
     * @return the rejection message
     */
    public String getRejectionMessage() {
        return rejectionMessage;
    }

    /**
     * Gets parent gateways.
     *
     * @return the parent gateways
     */
    public Set<String> getParentGateways() {
        return parentGateways;
    }

    /**
     * Gets selector ids.
     *
     * @return the selector ids
     */
    public List<String> getSelectorIds() {
        return selectorIds;
    }

    /**
     * Gets rule ids.
     *
     * @return the rule ids
     */
    public List<String> getRuleIds() {
        return ruleIds;
    }

    /**
     * Gets last reconciled time.
     *
     * @return the last reconciled time
     */
    public long getLastReconciledTime() {
        return lastReconciledTime;
    }

    /**
     * Creates a new builder.
     *
     * @return the builder
     */
    public static Builder builder() {
        return new Builder();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (Objects.isNull(o) || getClass() != o.getClass()) {
            return false;
        }
        final K8sResourceCorrelation that = (K8sResourceCorrelation) o;
        return Objects.equals(kind, that.kind)
                && Objects.equals(namespace, that.namespace)
                && Objects.equals(name, that.name)
                && Objects.equals(uid, that.uid)
                && Objects.equals(generation, that.generation);
    }

    @Override
    public int hashCode() {
        return Objects.hash(kind, namespace, name, uid, generation);
    }

    @Override
    public String toString() {
        return "K8sResourceCorrelation{"
                + "kind='" + kind + '\''
                + ", namespace='" + namespace + '\''
                + ", name='" + name + '\''
                + ", uid='" + uid + '\''
                + ", generation=" + generation
                + ", state=" + state
                + ", rejectionReason='" + rejectionReason + '\''
                + '}';
    }

    /**
     * Builder for {@link K8sResourceCorrelation}.
     */
    public static final class Builder {

        private String kind;

        private String namespace;

        private String name;

        private String uid;

        private Long generation;

        private String resourceVersion;

        private K8sResourceState state = K8sResourceState.ACCEPTED;

        private String rejectionReason;

        private String rejectionMessage;

        private Set<String> parentGateways;

        private List<String> selectorIds;

        private List<String> ruleIds;

        private long lastReconciledTime = System.currentTimeMillis();

        private Builder() {
        }

        /**
         * Sets kind.
         *
         * @param kind kind
         * @return builder
         */
        public Builder kind(final String kind) {
            this.kind = kind;
            return this;
        }

        /**
         * Sets namespace.
         *
         * @param namespace namespace
         * @return builder
         */
        public Builder namespace(final String namespace) {
            this.namespace = namespace;
            return this;
        }

        /**
         * Sets name.
         *
         * @param name name
         * @return builder
         */
        public Builder name(final String name) {
            this.name = name;
            return this;
        }

        /**
         * Sets uid.
         *
         * @param uid uid
         * @return builder
         */
        public Builder uid(final String uid) {
            this.uid = uid;
            return this;
        }

        /**
         * Sets generation.
         *
         * @param generation generation
         * @return builder
         */
        public Builder generation(final Long generation) {
            this.generation = generation;
            return this;
        }

        /**
         * Sets resource version.
         *
         * @param resourceVersion resource version
         * @return builder
         */
        public Builder resourceVersion(final String resourceVersion) {
            this.resourceVersion = resourceVersion;
            return this;
        }

        /**
         * Sets state.
         *
         * @param state state
         * @return builder
         */
        public Builder state(final K8sResourceState state) {
            this.state = state;
            return this;
        }

        /**
         * Sets rejection reason.
         *
         * @param rejectionReason rejection reason
         * @return builder
         */
        public Builder rejectionReason(final String rejectionReason) {
            this.rejectionReason = rejectionReason;
            return this;
        }

        /**
         * Sets rejection message.
         *
         * @param rejectionMessage rejection message
         * @return builder
         */
        public Builder rejectionMessage(final String rejectionMessage) {
            this.rejectionMessage = rejectionMessage;
            return this;
        }

        /**
         * Sets parent gateways.
         *
         * @param parentGateways parent gateways
         * @return builder
         */
        public Builder parentGateways(final Set<String> parentGateways) {
            this.parentGateways = parentGateways;
            return this;
        }

        /**
         * Sets selector ids.
         *
         * @param selectorIds selector ids
         * @return builder
         */
        public Builder selectorIds(final List<String> selectorIds) {
            this.selectorIds = selectorIds;
            return this;
        }

        /**
         * Sets rule ids.
         *
         * @param ruleIds rule ids
         * @return builder
         */
        public Builder ruleIds(final List<String> ruleIds) {
            this.ruleIds = ruleIds;
            return this;
        }

        /**
         * Sets last reconciled time.
         *
         * @param lastReconciledTime last reconciled time
         * @return builder
         */
        public Builder lastReconciledTime(final long lastReconciledTime) {
            this.lastReconciledTime = lastReconciledTime;
            return this;
        }

        /**
         * Builds correlation.
         *
         * @return the correlation
         */
        public K8sResourceCorrelation build() {
            return new K8sResourceCorrelation(this);
        }
    }
}
