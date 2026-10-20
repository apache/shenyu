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
import org.apache.shenyu.plugin.base.circuitbreaker.UpstreamCircuitBreaker.HalfOpenProbe;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Objects;

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

    private final HalfOpenProbe probe;

    private CircuitBreakerCandidates(final String selectorId, final List<Upstream> upstreams, final String[] keys, final HalfOpenProbe probe) {
        this.selectorId = selectorId;
        this.upstreams = upstreams;
        this.keys = keys;
        this.probe = probe;
    }

    /**
     * Filter a request's upstream snapshot without allocating another upstream list.
     *
     * @param selectorId the selector id
     * @param upstreams a mutable, request-owned copy from the upstream cache
     * @return candidates with the keys used to check their breaker state
     */
    static CircuitBreakerCandidates filter(final String selectorId, final List<Upstream> upstreams) {
        return filter(selectorId, upstreams, null);
    }

    /**
     * Filter candidates while preferring an explicitly requested probe target.
     *
     * @param selectorId the selector id
     * @param upstreams a mutable request-owned snapshot
     * @param requested the explicit target, or null
     * @return ordinary candidates or exactly one acquired recovery probe
     */
    static CircuitBreakerCandidates filter(final String selectorId, final List<Upstream> upstreams, final String requested) {
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
            } else if (Objects.isNull(requested) || requested.equals(upstream.getUrl())) {
                HalfOpenProbe acquired = breaker.acquireHalfOpenProbe(key);
                if (Objects.nonNull(acquired)) {
                    return probeCandidates(selectorId, upstreams, keys, upstream, key, acquired);
                }
            }
        }
        if (allowed == 0 && Objects.nonNull(requested)) {
            // An unavailable explicit target may fall back to another probe only
            // when no ordinary target is available. Reuse the first pass's keys.
            for (int index = 0; index < size; index++) {
                HalfOpenProbe acquired = breaker.acquireHalfOpenProbe(keys[index]);
                if (Objects.nonNull(acquired)) {
                    return probeCandidates(selectorId, upstreams, keys, upstreams.get(index), keys[index], acquired);
                }
            }
        }
        if (allowed < size) {
            upstreams.subList(allowed, size).clear();
        }
        return new CircuitBreakerCandidates(selectorId, upstreams, keys, null);
    }

    private static CircuitBreakerCandidates probeCandidates(final String selectorId, final List<Upstream> upstreams, final String[] keys,
                                                             final Upstream upstream, final String key, final HalfOpenProbe probe) {
        upstreams.set(0, upstream);
        keys[0] = key;
        upstreams.subList(1, upstreams.size()).clear();
        return new CircuitBreakerCandidates(selectorId, upstreams, keys, probe);
    }

    /**
     * Whether this request owns an exclusive recovery probe.
     *
     * @return true if a probe was acquired
     */
    boolean isProbe() {
        return Objects.nonNull(probe);
    }

    /**
     * Attach probe accounting, including cancellation and synchronous failures.
     *
     * @param source the deferred forwarding operation
     * @return the source with probe accounting if required
     */
    Mono<Void> recordProbeOutcome(final Mono<Void> source) {
        return isProbe() ? probe.recordOutcome(source) : source;
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
     * <p>Protocol and URL identify the breaker, including the copy made for
     * explicit target selection. Group and version do not affect this key.
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
        for (int index = 0; index < upstreams.size(); index++) {
            Upstream candidate = upstreams.get(index);
            if (Objects.equals(candidate.getProtocol(), upstream.getProtocol()) && Objects.equals(candidate.getUrl(), upstream.getUrl())) {
                return keys[index];
            }
        }
        return UpstreamCircuitBreaker.buildKey(selectorId, upstream);
    }
}
