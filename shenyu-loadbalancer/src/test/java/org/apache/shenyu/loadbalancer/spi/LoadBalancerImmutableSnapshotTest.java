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

package org.apache.shenyu.loadbalancer.spi;

import org.apache.shenyu.loadbalancer.entity.LoadBalanceData;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.spi.ExtensionLoader;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * Regression test pinning the read-only contract of the healthy upstream snapshot that
 * {@code UpstreamCacheManager} publishes (apache/shenyu#7880).
 *
 * <p>{@code findUpstreamListBySelectorId} and {@code findAllUpstreamListBySelectorId} hand
 * out the {@code List.copyOf} snapshot maintained by the health-check task instead of a
 * per-request mutable {@code ArrayList}. Every load balancer reachable through the SPI
 * therefore receives an immutable list on the request path, where an in-place compaction,
 * element replacement or sort surfaces as an {@link UnsupportedOperationException} at
 * runtime rather than at compile time. This drives every registered implementation with a
 * snapshot built the way the cache builds it and asserts that selection still succeeds and
 * leaves the snapshot's size, order and element identity untouched.
 *
 * <p>Elements remain shared instances: a load balancer may still update the mutable
 * statistics of the {@link Upstream} it picked, exactly as before the snapshot was
 * introduced. Only the list structure is frozen.
 */
public class LoadBalancerImmutableSnapshotTest {

    /**
     * Algorithms registered under META-INF/shenyu for the LoadBalancer SPI. Used as a lower
     * bound so that losing entries from that file cannot silently weaken this test.
     */
    private static final int REGISTERED_ALGORITHMS = 6;

    private static final int SELECTION_ROUNDS = 50;

    @Test
    public void everyRegisteredLoadBalancerSelectsFromTheImmutableSnapshot() {
        for (LoadBalancer loadBalancer : registeredLoadBalancers()) {
            List<Upstream> snapshot = publishedSnapshot();
            List<Upstream> publishedOrder = List.copyOf(snapshot);
            Upstream selected = loadBalancer.select(snapshot, new LoadBalanceData());
            Assertions.assertNotNull(selected, () -> name(loadBalancer) + " returned no upstream");
            Assertions.assertTrue(containsByIdentity(snapshot, selected),
                    () -> name(loadBalancer) + " returned an upstream that is not part of the snapshot");
            assertStructureUnchanged(publishedOrder, snapshot, loadBalancer);
        }
    }

    @Test
    public void repeatedSelectionKeepsTheSnapshotStructureIntact() {
        // Round robin rebuilds its cursor once the pool size differs from the recorded
        // weights, and p2c samples two random entries per call, so a single selection does
        // not reach every branch of those implementations.
        for (LoadBalancer loadBalancer : registeredLoadBalancers()) {
            List<Upstream> snapshot = publishedSnapshot();
            List<Upstream> publishedOrder = List.copyOf(snapshot);
            for (int round = 0; round < SELECTION_ROUNDS; round++) {
                Assertions.assertNotNull(loadBalancer.select(snapshot, new LoadBalanceData()),
                        () -> name(loadBalancer) + " returned no upstream");
            }
            assertStructureUnchanged(publishedOrder, snapshot, loadBalancer);
        }
    }

    @Test
    public void snapshotRejectsEveryInPlaceMutationForm() {
        List<Upstream> snapshot = publishedSnapshot();
        Upstream intruder = upstream("intruder:8080", 10);
        Assertions.assertThrows(UnsupportedOperationException.class, () -> snapshot.add(intruder));
        Assertions.assertThrows(UnsupportedOperationException.class, () -> snapshot.addAll(List.of(intruder)));
        // A half-mutable list such as Arrays.asList tolerates set and replaceAll, so it cannot
        // stand in for the published snapshot; both must be rejected here.
        Assertions.assertThrows(UnsupportedOperationException.class, () -> snapshot.set(0, intruder));
        Assertions.assertThrows(UnsupportedOperationException.class, () -> snapshot.replaceAll(ignored -> intruder));
        Assertions.assertThrows(UnsupportedOperationException.class, () -> snapshot.remove(0));
        Assertions.assertThrows(UnsupportedOperationException.class, () -> snapshot.removeIf(ignored -> true));
        Assertions.assertThrows(UnsupportedOperationException.class, () -> snapshot.retainAll(List.of()));
        Assertions.assertThrows(UnsupportedOperationException.class, snapshot::clear);
        Assertions.assertThrows(UnsupportedOperationException.class,
                () -> snapshot.sort(Comparator.comparing(Upstream::getUrl)));
        // The in-place compaction that broke the circuit-breaker path used a subList view.
        Assertions.assertThrows(UnsupportedOperationException.class, () -> snapshot.subList(0, 1).clear());
        Assertions.assertEquals(publishedSnapshot().size(), snapshot.size(),
                "a rejected mutation must leave the snapshot size alone");
    }

    private List<LoadBalancer> registeredLoadBalancers() {
        List<LoadBalancer> loadBalancers = ExtensionLoader.getExtensionLoader(LoadBalancer.class).getJoins();
        Assertions.assertTrue(loadBalancers.size() >= REGISTERED_ALGORITHMS,
                () -> "expected at least " + REGISTERED_ALGORITHMS + " registered load balancers but found "
                        + loadBalancers.size() + "; the SPI registration file may have lost entries");
        return loadBalancers;
    }

    /**
     * Build a snapshot exactly the way {@code UpstreamCheckTask#publishHealthySnapshot} does,
     * by copying the healthy list. It holds more than one element so that
     * {@code AbstractLoadBalancer#select} descends into {@code doSelect} instead of
     * short-circuiting on a single-entry pool.
     *
     * @return an immutable snapshot of three weighted upstreams
     */
    private List<Upstream> publishedSnapshot() {
        List<Upstream> healthy = new ArrayList<>();
        healthy.add(upstream("alpha:8080", 30));
        healthy.add(upstream("beta:8080", 50));
        healthy.add(upstream("gamma:8080", 70));
        return List.copyOf(healthy);
    }

    private Upstream upstream(final String url, final int weight) {
        return Upstream.builder()
                .protocol("http://")
                .url(url)
                .weight(weight)
                .status(true)
                .build();
    }

    private boolean containsByIdentity(final List<Upstream> snapshot, final Upstream selected) {
        for (Upstream candidate : snapshot) {
            if (candidate == selected) {
                return true;
            }
        }
        return false;
    }

    private void assertStructureUnchanged(final List<Upstream> publishedOrder, final List<Upstream> snapshot,
                                          final LoadBalancer loadBalancer) {
        Assertions.assertEquals(publishedOrder.size(), snapshot.size(),
                () -> name(loadBalancer) + " changed the snapshot size");
        for (int index = 0; index < publishedOrder.size(); index++) {
            final int position = index;
            Assertions.assertSame(publishedOrder.get(position), snapshot.get(position),
                    () -> name(loadBalancer) + " reordered or replaced the snapshot entry at " + position);
        }
    }

    private String name(final LoadBalancer loadBalancer) {
        return loadBalancer.getClass().getSimpleName();
    }
}
