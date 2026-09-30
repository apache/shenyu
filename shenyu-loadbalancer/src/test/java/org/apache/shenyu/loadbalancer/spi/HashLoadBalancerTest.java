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

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotSame;
import static org.junit.jupiter.api.Assertions.assertSame;

/**
 * HashLoadBalancer unit test.
 */
class HashLoadBalancerTest {

    @Test
    void doSelectWithSuccess() {
        final HashLoadBalancer hashLoadBalancer = new HashLoadBalancer();
        final List<Upstream> upstreamList = new ArrayList<>();
        upstreamList.add(Upstream.builder().url("http://1.1.1.1/api").build());
        upstreamList.add(Upstream.builder().url("http://2.2.2.2/api").build());
        upstreamList.add(Upstream.builder().url("http://3.3.3.3/api").build());

        Upstream upstream = hashLoadBalancer.doSelect(upstreamList, new LoadBalanceData());
        assertEquals(upstreamList.get(2).getUrl(), upstream.getUrl());
    }

    @Test
    void shouldReuseRingWithoutReturningStaleUpstreamInstances() throws Exception {
        final HashLoadBalancer hashLoadBalancer = new HashLoadBalancer();
        final List<Upstream> first = upstreams();
        final List<Upstream> refreshed = upstreams();

        Upstream firstSelection = hashLoadBalancer.doSelect(first, new LoadBalanceData());
        Upstream refreshedSelection = hashLoadBalancer.doSelect(refreshed, new LoadBalanceData());

        assertEquals(first.indexOf(firstSelection), refreshed.indexOf(refreshedSelection));
        assertNotSame(firstSelection, refreshedSelection);
        assertSame(refreshed.get(refreshed.indexOf(refreshedSelection)), refreshedSelection);
        Field ringCacheField = HashLoadBalancer.class.getDeclaredField("ringCache");
        ringCacheField.setAccessible(true);
        assertEquals(1, ((Map<?, ?>) ringCacheField.get(hashLoadBalancer)).size());
    }

    private List<Upstream> upstreams() {
        final List<Upstream> result = new ArrayList<>();
        result.add(Upstream.builder().url("http://1.1.1.1/api").build());
        result.add(Upstream.builder().url("http://2.2.2.2/api").build());
        result.add(Upstream.builder().url("http://3.3.3.3/api").build());
        return result;
    }

}
