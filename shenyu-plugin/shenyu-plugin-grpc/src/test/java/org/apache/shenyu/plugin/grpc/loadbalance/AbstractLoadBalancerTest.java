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

import io.grpc.Attributes;
import io.grpc.EquivalentAddressGroup;
import io.grpc.LoadBalancer;
import io.netty.channel.local.LocalAddress;
import org.apache.shenyu.plugin.grpc.loadbalance.picker.UnitTestIdleHelper;
import org.apache.shenyu.plugin.grpc.loadbalance.picker.UnitTestReadHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;

import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

public class AbstractLoadBalancerTest {
    private RandomLoadBalancerProvider randomLoadBalancerProvider;

    @BeforeEach
    public void setUp() {
        randomLoadBalancerProvider = new RandomLoadBalancerProvider();
    }

    @Test
    public void testHandleResolvedAddresses() {
        LoadBalancer.Helper helper = new UnitTestReadHelper();
        final LoadBalancer loadBalancer = randomLoadBalancerProvider.newLoadBalancer(helper);
        EquivalentAddressGroup group = new EquivalentAddressGroup(new LocalAddress("id"));
        LoadBalancer.ResolvedAddresses resolvedAddresses = mock(LoadBalancer.ResolvedAddresses.class);
        when(resolvedAddresses.getAddresses()).thenReturn(Collections.singletonList(group));
        Attributes attributes = mock(Attributes.class);
        when(attributes.get(GrpcAttributeUtils.APP_NAME)).thenReturn("");
        when(resolvedAddresses.getAttributes()).thenReturn(attributes);
        loadBalancer.handleResolvedAddresses(resolvedAddresses);

        LoadBalancer.Helper idleHelper = new UnitTestIdleHelper();
        final LoadBalancer idleLoadBalancer = randomLoadBalancerProvider.newLoadBalancer(idleHelper);
        idleLoadBalancer.handleResolvedAddresses(resolvedAddresses);
        idleLoadBalancer.shutdown();
    }
}
