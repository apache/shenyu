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
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * The type abstract load balancer test.
 */
public class AbstractLoadBalancerTest {

    private static final int WEIGHT = 100;

    private static final int WARMUP = 10 * 60 * 1000;

    private final TestLoadBalancer loadBalancer = new TestLoadBalancer();

    @Test
    public void warmupWeightWithinWarmupTest() {
        int weight = loadBalancer.weightOf(upstream(System.currentTimeMillis() - WARMUP / 2));
        assertTrue(weight > 0 && weight < WEIGHT);
    }

    @Test
    public void fullWeightAfterWarmupTest() {
        assertEquals(WEIGHT, loadBalancer.weightOf(upstream(System.currentTimeMillis() - WARMUP - 1000L)));
    }

    @Test
    public void fullWeightWhenUptimeExceedsIntRangeTest() {
        long uptime = (long) Integer.MAX_VALUE + 1000L;
        assertEquals(WEIGHT, loadBalancer.weightOf(upstream(System.currentTimeMillis() - uptime)));
    }

    @Test
    public void fullWeightWhenUptimeLowBitsFallInWarmupTest() {
        long uptime = (1L << 32) + WARMUP / 2;
        assertEquals(WEIGHT, loadBalancer.weightOf(upstream(System.currentTimeMillis() - uptime)));
    }

    private Upstream upstream(final long timestamp) {
        return Upstream.builder()
                .url("upstream")
                .weight(WEIGHT)
                .warmup(WARMUP)
                .timestamp(timestamp)
                .build();
    }

    private static final class TestLoadBalancer extends AbstractLoadBalancer {

        @Override
        protected Upstream doSelect(final List<Upstream> upstreamList, final LoadBalanceData data) {
            return upstreamList.get(0);
        }

        int weightOf(final Upstream upstream) {
            return getWeight(upstream);
        }
    }
}
