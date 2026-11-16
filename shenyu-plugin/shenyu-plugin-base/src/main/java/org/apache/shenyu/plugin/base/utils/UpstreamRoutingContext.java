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

package org.apache.shenyu.plugin.base.utils;

import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Immutable partition resolved for one request, reapplied to refreshed failover candidates.
 */
public final class UpstreamRoutingContext {

    /**
     * Exchange attribute containing the resolved request partition.
     */
    public static final String ATTRIBUTE = UpstreamRoutingContext.class.getName();

    private final String selectorId;

    private final Map<String, String> labels;

    public UpstreamRoutingContext(final String selectorId, final Map<String, String> labels) {
        this.selectorId = selectorId;
        this.labels = Objects.isNull(labels) ? Collections.emptyMap() : Collections.unmodifiableMap(new HashMap<>(labels));
    }

    /**
     * Apply the original partition to the current healthy snapshot without repeating the canary decision.
     * Reject a different selector so its candidates cannot inherit this request's partition.
     *
     * @param currentSelectorId selector owning the latest snapshot
     * @param upstreams latest healthy upstreams
     * @return matching nodes, or no nodes if the selector no longer matches
     */
    public List<Upstream> filter(final String currentSelectorId, final List<Upstream> upstreams) {
        return Objects.equals(selectorId, currentSelectorId) ? UpstreamLabelUtils.filter(upstreams, labels) : Collections.emptyList();
    }
}
