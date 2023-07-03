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

package org.apache.shenyu.plugin.grpc.loadbalance.picker;

import io.grpc.Attributes;
import io.grpc.ConnectivityState;
import io.grpc.ConnectivityStateInfo;
import io.grpc.EquivalentAddressGroup;
import io.grpc.LoadBalancer;
import io.grpc.ManagedChannel;
import io.grpc.NameResolver;
import org.apache.shenyu.plugin.grpc.loadbalance.SubChannels;
import javax.annotation.Nonnull;
import java.net.SocketAddress;
import java.util.Collections;
import java.util.List;

public class UnitTestReadHelper extends LoadBalancer.Helper {
    @Override
    public ManagedChannel createOobChannel(final EquivalentAddressGroup eag, final String authority) {
        return null;
    }

    @Override
    public void updateBalancingState(@Nonnull final ConnectivityState newState, @Nonnull final LoadBalancer.SubchannelPicker newPicker) {

    }

    @Override
    public NameResolver.Factory getNameResolverFactory() {
        return null;
    }

    @Override
    public String getAuthority() {
        return null;
    }

    @Override
    public LoadBalancer.Subchannel createSubchannel(final LoadBalancer.CreateSubchannelArgs args) {
        return new UnitTestSubchannel(args);
    }

    public static class UnitTestSubchannel extends LoadBalancer.Subchannel {
        private final LoadBalancer.CreateSubchannelArgs args;

        public UnitTestSubchannel(final LoadBalancer.CreateSubchannelArgs args) {
            this.args = args;
        }

        @Override
        public void start(final LoadBalancer.SubchannelStateListener listener) {
            SubChannels.setStateInfo(this, ConnectivityStateInfo.forNonError(ConnectivityState.READY));
        }

        @Override
        public void shutdown() {
        }

        @Override
        public void requestConnection() {
        }

        @Override
        public Attributes getAttributes() {
            return args.getAttributes();
        }

        @Override
        public List<EquivalentAddressGroup> getAllAddresses() {
            return Collections.singletonList(new EquivalentAddressGroup(new SocketAddress() {
            }));
        }
    }
}
