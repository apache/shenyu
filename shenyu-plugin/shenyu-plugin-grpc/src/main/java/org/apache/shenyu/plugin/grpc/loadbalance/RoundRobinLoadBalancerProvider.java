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
import io.grpc.LoadBalancerProvider;
import org.apache.shenyu.plugin.grpc.loadbalance.picker.RoundRobinPicker;
import org.apache.shenyu.plugin.grpc.loadbalance.picker.AbstractReadyPicker;

import java.util.List;

/**
 * RoundRobinLoadBalancerProvider.
 */
public class RoundRobinLoadBalancerProvider extends LoadBalancerProvider {

    @Override
    public boolean isAvailable() {
        return true;
    }

    @Override
    public int getPriority() {
        return 6;
    }

    @Override
    public String getPolicyName() {
        return LoadBalancerStrategy.ROUND_ROBIN.getStrategy();
    }

    @Override
    public LoadBalancer newLoadBalancer(final LoadBalancer.Helper helper) {
        return new AbstractLoadBalancer(helper) {
            @Override
            protected AbstractReadyPicker newPicker(final List<Subchannel> list) {
                return new RoundRobinPicker(list);
            }
        };
    }
}
