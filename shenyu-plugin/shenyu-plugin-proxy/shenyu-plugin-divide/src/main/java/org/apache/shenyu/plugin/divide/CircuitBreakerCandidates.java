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

package org.apache.shenyu.plugin.divide;

import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker;

import java.util.List;

/**
 * Request-local candidates and their circuit-breaker keys.
 *
 * <p>The cache supplies a mutable list copy for each request. Compact that copy
 * in place, preserving upstream order, identity and load-balancing statistics.
 * Keys are retained only for this request, so configuration changes need no
 * additional cache invalidation.
 */
final class CircuitBreakerCandidates {

    private final String selectorId;

    private final List<Upstream> upstreams;

    private final String[] keys;

    private CircuitBreakerCandidates(final String selectorId, final List<Upstream> upstreams, final String[] keys) {
        this.selectorId = selectorId;
        this.upstreams = upstreams;
        this.keys = keys;
    }

    /**
     * Filter a request's upstream snapshot without allocating another upstream list.
     *
     * @param selectorId the selector id
     * @param upstreams a mutable, request-owned copy from the upstream cache
     * @return candidates with the keys used to check their breaker state
     */
    static CircuitBreakerCandidates filter(final String selectorId, final List<Upstream> upstreams) {
        UpstreamCircuitBreaker breaker = UpstreamCircuitBreaker.getInstance();
        int size = upstreams.size();
        String[] keys = new String[size];
        int allowed = 0;
        for (int index = 0; index < size; index++) {
            Upstream upstream = upstreams.get(index);
            String key = UpstreamCircuitBreaker.buildKey(selectorId, upstream);
            keys[index] = key;
            if (breaker.isRequestAllowed(key)) {
                if (allowed != index) {
                    upstreams.set(allowed, upstream);
                    keys[allowed] = key;
                }
                allowed++;
            }
        }
        if (allowed == 0) {
            // Keep the second state check: a wait window or an in-flight outcome
            // may change while the first pass is running. Do not acquire permits
            // for blocked candidates when ordinary candidates are available.
            for (int index = 0; index < size; index++) {
                if (breaker.tryAcquireHalfOpenProbe(keys[index])) {
                    upstreams.set(0, upstreams.get(index));
                    keys[0] = keys[index];
                    allowed = 1;
                    break;
                }
            }
        }
        if (allowed < size) {
            upstreams.subList(allowed, size).clear();
        }
        return new CircuitBreakerCandidates(selectorId, upstreams, keys);
    }

    /**
     * Get candidates in their original order.
     *
     * @return upstreams allowed by the breaker
     */
    List<Upstream> getUpstreams() {
        return upstreams;
    }

    /**
     * Get the selected upstream's previously computed key.
     *
     * <p>Upstream equality uses protocol and URL, including the copy made for
     * explicit target selection. Equal targets within a selector share a key.
     * A custom load balancer may return a target outside this list; only that
     * target needs a new key.
     *
     * @param upstream the selected upstream
     * @return its breaker key
     */
    String getBreakerKey(final Upstream upstream) {
        // Built-in load balancers return an existing candidate; avoid comparing
        // every preceding candidate's URL before falling back to value equality.
        for (int index = 0; index < upstreams.size(); index++) {
            if (upstreams.get(index) == upstream) {
                return keys[index];
            }
        }
        int index = upstreams.indexOf(upstream);
        return index < 0 ? UpstreamCircuitBreaker.buildKey(selectorId, upstream) : keys[index];
    }
}
