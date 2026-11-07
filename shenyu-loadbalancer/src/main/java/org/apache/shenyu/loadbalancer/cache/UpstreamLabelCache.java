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

package org.apache.shenyu.loadbalancer.cache;

import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Healthy subsets for configured label predicates. Writers hold the health-check task lock;
 * readers use immutable, atomically published lists without taking that lock.
 */
final class UpstreamLabelCache {

    private final Map<String, SelectorLabels> selectors = new ConcurrentHashMap<>();

    void register(final String selectorId, final String owner, final Collection<Map<String, String>> predicates, final List<Upstream> upstreams) {
        Set<Map<String, String>> normalized = new HashSet<>();
        for (Map<String, String> predicate : predicates) {
            if (Objects.nonNull(predicate) && !predicate.isEmpty()) {
                normalized.add(Map.copyOf(predicate));
            }
        }
        if (normalized.isEmpty()) {
            unregister(selectorId, owner, upstreams);
            return;
        }
        SelectorLabels labels = selectors.computeIfAbsent(selectorId, key -> new SelectorLabels());
        if (!normalized.equals(labels.owners.put(owner, normalized))) {
            refresh(selectorId, upstreams);
        }
    }

    void unregister(final String selectorId, final String owner, final List<Upstream> upstreams) {
        SelectorLabels labels = selectors.get(selectorId);
        if (Objects.nonNull(labels) && Objects.nonNull(labels.owners.remove(owner))) {
            if (labels.owners.isEmpty()) {
                selectors.remove(selectorId);
            } else {
                refresh(selectorId, upstreams);
            }
        }
    }

    void remove(final String selectorId) {
        selectors.remove(selectorId);
    }

    void refresh(final String selectorId, final List<Upstream> upstreams) {
        SelectorLabels labels = selectors.get(selectorId);
        if (Objects.isNull(labels)) {
            return;
        }
        Map<Map<String, String>, List<Upstream>> pools = new HashMap<>();
        for (Set<Map<String, String>> predicates : labels.owners.values()) {
            for (Map<String, String> predicate : predicates) {
                pools.computeIfAbsent(predicate, key -> List.copyOf(filter(upstreams, key)));
            }
        }
        labels.pools = Collections.unmodifiableMap(pools);
    }

    List<Upstream> get(final String selectorId, final Map<String, String> predicate) {
        SelectorLabels labels = selectors.get(selectorId);
        return Objects.isNull(labels) ? null : labels.pools.get(predicate);
    }

    static List<Upstream> filter(final List<Upstream> upstreams, final Map<String, String> predicate) {
        List<Upstream> result = new ArrayList<>();
        if (Objects.nonNull(upstreams) && Objects.nonNull(predicate) && !predicate.isEmpty()) {
            for (Upstream upstream : upstreams) {
                if (upstream.getLabels().entrySet().containsAll(predicate.entrySet())) {
                    result.add(upstream);
                }
            }
        }
        return result;
    }

    private static final class SelectorLabels {

        private final Map<String, Set<Map<String, String>>> owners = new HashMap<>();

        private volatile Map<Map<String, String>, List<Upstream>> pools = Collections.emptyMap();
    }
}
