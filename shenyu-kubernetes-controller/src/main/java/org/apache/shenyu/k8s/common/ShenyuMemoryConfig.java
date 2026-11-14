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

package org.apache.shenyu.k8s.common;

import org.apache.commons.lang3.tuple.Pair;
import org.apache.shenyu.common.config.ssl.SslCrtAndKeyStream;

import java.util.List;
import java.util.Objects;

/**
 * The configuration that shenyu can read directly.
 */
public class ShenyuMemoryConfig {

    private Pair<Pair<String, String>, IngressConfiguration> globalDefaultBackend;

    private List<IngressConfiguration> routeConfigList;

    private List<SslCrtAndKeyStream> tlsConfigList;

    /** Whether every backendRef resolved to at least one ready upstream; drives ResolvedRefs. */
    private boolean allBackendsResolved = true;

    /** Reason for ResolvedRefs=False (BackendNotFound, RefNotPermitted or InvalidKind). */
    private String unresolvedReason;

    /** Unsupported filters must surface as Accepted=False/UnsupportedValue with the rule unprogrammed. */
    private boolean hasUnsupportedFilters;

    private String ruleRejectionMessage;

    /**
     * ShenyuMemoryConfig Constructor.
     */
    public ShenyuMemoryConfig() {
    }

    /**
     * Get GlobalDefaultBackend.
     *
     * @return GlobalDefaultBackend
     */
    public Pair<Pair<String, String>, IngressConfiguration> getGlobalDefaultBackend() {
        return globalDefaultBackend;
    }

    /**
     * Set GlobalDefaultBackend.
     *
     * @param globalDefaultBackend GlobalDefaultBackend
     */
    public void setGlobalDefaultBackend(final Pair<Pair<String, String>, IngressConfiguration> globalDefaultBackend) {
        this.globalDefaultBackend = globalDefaultBackend;
    }

    /**
     * Get RouteConfigList.
     *
     * @return RouteConfigList
     */
    public List<IngressConfiguration> getRouteConfigList() {
        return routeConfigList;
    }

    /**
     * Set RouteConfigList.
     *
     * @param routeConfigList RouteConfigList
     */
    public void setRouteConfigList(final List<IngressConfiguration> routeConfigList) {
        this.routeConfigList = routeConfigList;
    }

    /**
     * Get TlsConfigList.
     *
     * @return TlsConfigList
     */
    public List<SslCrtAndKeyStream> getTlsConfigList() {
        return tlsConfigList;
    }

    /**
     * Set TlsConfigList.
     *
     * @param tlsConfigList TlsConfigList
     */
    public void setTlsConfigList(final List<SslCrtAndKeyStream> tlsConfigList) {
        this.tlsConfigList = tlsConfigList;
    }

    public boolean isAllBackendsResolved() {
        return allBackendsResolved;
    }

    public void setAllBackendsResolved(final boolean allBackendsResolved) {
        this.allBackendsResolved = allBackendsResolved;
    }

    public String getUnresolvedReason() {
        return unresolvedReason;
    }

    public void setUnresolvedReason(final String unresolvedReason) {
        this.unresolvedReason = unresolvedReason;
    }

    /**
     * Get the diagnostic used to withdraw the route and set Accepted=False/UnsupportedValue.
     * @return the first rejected rule's diagnostic in declaration order, or null for a valid route
     */
    public String getRuleRejectionMessage() {
        if (hasUnsupportedFilters && Objects.isNull(ruleRejectionMessage)) {
            return "route rules declare filters which are not supported by ShenYu";
        }
        return ruleRejectionMessage;
    }

    /**
     * Set the first rule-level rejection diagnostic from the parser.
     * @param ruleRejectionMessage diagnostic, or null if the parser accepted every rule
     */
    public void setRuleRejectionMessage(final String ruleRejectionMessage) {
        this.ruleRejectionMessage = ruleRejectionMessage;
    }

    public boolean isHasUnsupportedFilters() {
        return hasUnsupportedFilters;
    }

    public void setHasUnsupportedFilters(final boolean hasUnsupportedFilters) {
        this.hasUnsupportedFilters = hasUnsupportedFilters;
    }
}
