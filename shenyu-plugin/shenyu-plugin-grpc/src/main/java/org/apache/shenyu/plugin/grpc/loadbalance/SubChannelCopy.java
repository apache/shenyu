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

import io.grpc.ConnectivityStateInfo;
import io.grpc.EquivalentAddressGroup;
import io.grpc.LoadBalancer;

import java.util.Objects;

/**
 * SubChannelCopy.
 */
public class SubChannelCopy {

    private final int weight;

    private final String status;

    private final LoadBalancer.Subchannel channel;

    private final EquivalentAddressGroup addressGroup;

    private final ConnectivityStateInfo state;

    /**
     * Instantiates a new Sub channel copy.
     *
     * @param channel the channel
     */
    public SubChannelCopy(final LoadBalancer.Subchannel channel) {
        this.channel = channel;
        this.addressGroup = channel.getAddresses();
        this.weight = SubChannels.getWeight(channel);
        this.state = SubChannels.getStateInfo(channel);
        this.status = SubChannels.getStatus(channel);
    }

    /**
     * Gets weight.
     *
     * @return the weight
     */
    public int getWeight() {
        return weight;
    }

    /**
     * Gets status.
     *
     * @return the status
     */
    public String getStatus() {
        return status;
    }

    /**
     * Gets channel.
     *
     * @return the channel
     */
    public LoadBalancer.Subchannel getChannel() {
        return channel;
    }

    /**
     * Gets address group.
     *
     * @return the address group
     */
    public EquivalentAddressGroup getAddressGroup() {
        return addressGroup;
    }

    /**
     * Gets state.
     *
     * @return the state
     */
    public ConnectivityStateInfo getState() {
        return state;
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        SubChannelCopy that = (SubChannelCopy) o;
        return weight == that.weight && Objects.equals(status, that.status) && Objects.equals(channel, that.channel)
                && Objects.equals(addressGroup, that.addressGroup) && Objects.equals(state, that.state);
    }

    @Override
    public int hashCode() {
        return Objects.hash(weight, status, channel, addressGroup, state);
    }
}
