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
import org.apache.shenyu.plugin.grpc.loadbalance.picker.UnitTestReadHelper;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * The Test Case For {@link RandomLoadBalancerProvider}.
 */
@RunWith(MockitoJUnitRunner.class)
public class RandomLoadBalancerProviderTest {

    private RandomLoadBalancerProvider randomLoadBalancerProvider;

    @Before
    public void setUp() {
        randomLoadBalancerProvider = new RandomLoadBalancerProvider();
    }

    @Test
    public void testIsAvailable() {
        assertTrue(randomLoadBalancerProvider.isAvailable());
    }

    @Test
    public void testGetPriority() {
        assertEquals(randomLoadBalancerProvider.getPriority(), 6);
    }

    @Test
    public void testGetPolicyName() {
        assertEquals(randomLoadBalancerProvider.getPolicyName(), LoadBalancerStrategy.RANDOM.getStrategy());
    }

    @Test
    public void testNewLoadBalancer() {
        LoadBalancer.Helper helper = new UnitTestReadHelper();
        final LoadBalancer loadBalancer = randomLoadBalancerProvider.newLoadBalancer(helper);
        assertNotNull(loadBalancer);
    }
}
