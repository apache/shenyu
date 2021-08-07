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

package org.apache.shenyu.plugin.grpc.loadbalance;

import io.grpc.LoadBalancer;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.mock;

@RunWith(MockitoJUnitRunner.class)
public class RoundRobinLoadBalancerProviderTest {

    private RoundRobinLoadBalancerProvider roundRobinLoadBalancerProvider;

    @Before
    public void setUp() {
        roundRobinLoadBalancerProvider = new RoundRobinLoadBalancerProvider();
    }

    @Test
    public void testIsAvailable() {
        assertTrue(roundRobinLoadBalancerProvider.isAvailable());
    }

    @Test
    public void testGetPriority() {
        assertEquals(roundRobinLoadBalancerProvider.getPriority(), 6);
    }

    @Test
    public void testGetPolicyName() {
        assertEquals(roundRobinLoadBalancerProvider.getPolicyName(), LoadBalancerStrategy.ROUND_ROBIN.getStrategy());
    }

    @Test
    public void testNewLoadBalancer() {
        LoadBalancer.Helper helper = mock(LoadBalancer.Helper.class);
        LoadBalancer loadBalancer = roundRobinLoadBalancerProvider.newLoadBalancer(helper);
        assertNotNull(loadBalancer);
    }
}
