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

import io.grpc.ConnectivityState;
import io.grpc.LoadBalancer;
import io.grpc.Status;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.plugin.grpc.loadbalance.SubChannelCopy;

import java.util.HashSet;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The AbstractReadyPicker result.
 */
public abstract class AbstractReadyPicker extends AbstractPicker implements Picker {

    private final boolean hasIdleNode;

    private final List<SubChannelCopy> list;

    AbstractReadyPicker(final List<LoadBalancer.Subchannel> list) {
        this.list = list.stream().map(SubChannelCopy::new).collect(Collectors.toList());
        this.hasIdleNode = hasIdleNode();
    }

    private boolean hasIdleNode() {
        return this.list.stream().anyMatch(r -> r.getState().getState() == ConnectivityState.IDLE
                || r.getState().getState() == ConnectivityState.CONNECTING);
    }

    @Override
    public LoadBalancer.PickResult pickSubchannel(final LoadBalancer.PickSubchannelArgs args) {
        final List<SubChannelCopy> list = getSubchannels();
        if (CollectionUtils.isEmpty(list)) {
            return getErrorPickResult();
        }
        SubChannelCopy channel = pick(list);
        return Objects.isNull(channel) ? getErrorPickResult() : LoadBalancer.PickResult.withSubchannel(channel.getChannel());
    }

    /**
     * Choose subChannel.
     *
     * @param list subChannel list
     * @return result subChannel
     */
    protected abstract SubChannelCopy pick(List<SubChannelCopy> list);

    @Override
    public List<SubChannelCopy> getSubchannels() {
        return list.stream().filter(r -> r.getState().getState() == ConnectivityState.READY
                && Boolean.parseBoolean(r.getStatus())).collect(Collectors.toList());
    }

    private LoadBalancer.PickResult getErrorPickResult() {
        if (hasIdleNode) {
            return LoadBalancer.PickResult.withNoResult();
        } else {
            return LoadBalancer.PickResult.withError(
                    Status.UNAVAILABLE.withCause(new NoSuchElementException()).withDescription("can not find the subChannel")
            );
        }
    }

    @Override
    public boolean isEquivalentTo(final AbstractPicker picker) {
        if (!(picker instanceof AbstractReadyPicker)) {
            return false;
        }
        AbstractReadyPicker other = (AbstractReadyPicker) picker;
        // the lists cannot contain duplicate subchannels
        return other == this || (list.size() == other.list.size()
                && new HashSet<>(list).containsAll(other.list));
    }

    @Override
    public String getSubchannelsInfo() {
        final List<String> infos = this.list.stream().map(r -> "Subchannel"
                        + "{ weight=" + r.getWeight()
                        + ", readyState=\"" + r.getState().toString() + "\""
                        + ", address=\"" + r.getChannel().getAddresses() + "\""
                        + "}")
                .collect(Collectors.toList());
        return "[ " + String.join(",", infos) + " ]";
    }
}
