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

import io.grpc.ConnectivityState;
import io.grpc.EquivalentAddressGroup;
import io.grpc.LoadBalancer;
import io.grpc.Status;
import org.apache.shenyu.plugin.grpc.loadbalance.picker.AbstractPicker;
import org.apache.shenyu.plugin.grpc.loadbalance.picker.AbstractReadyPicker;
import org.apache.shenyu.plugin.grpc.loadbalance.picker.EmptyPicker;
import io.grpc.Attributes;
import io.grpc.ConnectivityStateInfo;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.ArrayList;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.HashMap;
import java.util.List;
import java.util.HashSet;
import java.util.Collection;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;
import java.util.stream.Collectors;

import static com.google.common.base.Preconditions.checkNotNull;
import static io.grpc.ConnectivityState.CONNECTING;
import static io.grpc.ConnectivityState.IDLE;
import static io.grpc.ConnectivityState.TRANSIENT_FAILURE;
import static io.grpc.ConnectivityState.SHUTDOWN;
import static io.grpc.ConnectivityState.READY;

/**
 * LoadBalancer.
 */
public abstract class AbstractLoadBalancer extends LoadBalancer {

    private static final Logger LOG = LoggerFactory.getLogger(AbstractLoadBalancer.class);

    private static final Status EMPTY_OK = Status.OK.withDescription("no subchannels ready");

    private final Helper helper;

    private final AtomicReference<String> serviceName = new AtomicReference<>();

    private final Map<EquivalentAddressGroup, Subchannel> subchannels = new ConcurrentHashMap<>();

    private ConnectivityState currentState;

    private AbstractPicker currentPicker = new EmptyPicker(EMPTY_OK);

    public AbstractLoadBalancer(final Helper helper) {
        this.helper = checkNotNull(helper, "helper");
    }

    private String getServiceName() {
        return serviceName.get();
    }

    private void setAttribute(final Attributes attributes) {
        this.serviceName.compareAndSet(null, attributes.get(GrpcAttributeUtils.appName()).toString());
    }

    @Override
    public void handleResolvedAddresses(final ResolvedAddresses resolvedAddresses) {
        setAttribute(resolvedAddresses.getAttributes());
        Set<EquivalentAddressGroup> currentAddrs = subchannels.keySet();
        Map<EquivalentAddressGroup, EquivalentAddressGroup> latestAddrs = stripAttrs(resolvedAddresses.getAddresses());
        Set<EquivalentAddressGroup> removedAddrs = setsDifference(currentAddrs, latestAddrs.keySet());
        for (Map.Entry<EquivalentAddressGroup, EquivalentAddressGroup> latestEntry : latestAddrs.entrySet()) {
            EquivalentAddressGroup strippedAddressGroup = latestEntry.getKey();
            EquivalentAddressGroup originalAddressGroup = latestEntry.getValue();
            Subchannel subchannel;
            Subchannel existingSubchannel = subchannels.get(strippedAddressGroup);
            if (Objects.nonNull(existingSubchannel)) {
                subchannel = existingSubchannel;
                SubChannels.updateAttributes(existingSubchannel, originalAddressGroup.getAttributes());
            } else {
                subchannel = SubChannels.createSubChannel(helper, strippedAddressGroup, originalAddressGroup.getAttributes());
                subchannel.start(state -> processSubchannelState(subchannel, state));
                subchannels.put(strippedAddressGroup, subchannel);
            }
            subchannel.requestConnection();
        }
        List<Subchannel> removedSubchannels = new ArrayList<>();
        for (EquivalentAddressGroup addressGroup : removedAddrs) {
            removedSubchannels.add(subchannels.remove(addressGroup));
        }
        updateBalancingState();
        for (Subchannel removedSubchannel : removedSubchannels) {
            shutdownSubchannel(removedSubchannel);
        }
    }

    private void processSubchannelState(final Subchannel subchannel, final ConnectivityStateInfo stateInfo) {
        if (subchannels.get(stripAttrs(subchannel.getAddresses())) != subchannel) {
            return;
        }
        if (stateInfo.getState() == IDLE) {
            subchannel.requestConnection();
            LOG.info("AbstractLoadBalancer.handleSubchannelState, current state:IDLE, subchannel.requestConnection().");
        }
        final ConnectivityStateInfo originStateInfo = SubChannels.getStateInfo(subchannel);
        if (originStateInfo.getState().equals(TRANSIENT_FAILURE)) {
            if (stateInfo.getState().equals(CONNECTING) || stateInfo.getState().equals(IDLE)) {
                return;
            }
        }
        SubChannels.setStateInfo(subchannel, stateInfo);
        updateBalancingState();
    }

    private Map<EquivalentAddressGroup, EquivalentAddressGroup> stripAttrs(final List<EquivalentAddressGroup> groupList) {
        Map<EquivalentAddressGroup, EquivalentAddressGroup> addrs = new HashMap<>(groupList.size() * 2);
        for (EquivalentAddressGroup group : groupList) {
            addrs.put(stripAttrs(group), group);
        }
        return addrs;
    }

    private static EquivalentAddressGroup stripAttrs(final EquivalentAddressGroup eag) {
        return new EquivalentAddressGroup(eag.getAddresses());
    }

    private <T> Set<T> setsDifference(final Set<T> a, final Set<T> b) {
        Set<T> aCopy = new HashSet<>(a);
        aCopy.removeAll(b);
        return aCopy;
    }

    @Override
    public void shutdown() {
        for (Subchannel subchannel : subchannels.values()) {
            shutdownSubchannel(subchannel);
        }
    }

    private void shutdownSubchannel(final Subchannel subchannel) {
        subchannel.shutdown();
        SubChannels.setStateInfo(subchannel, ConnectivityStateInfo.forNonError(SHUTDOWN));
    }

    @Override
    public void handleNameResolutionError(final Status error) {
        updateBalancingState(TRANSIENT_FAILURE,
                currentPicker instanceof AbstractReadyPicker ? currentPicker : new EmptyPicker(error));
    }

    /**
     * Updates picker with the list of active subchannels (state == READY).
     */
    private void updateBalancingState() {
        final List<Subchannel> activeList = subchannels.values()
                .stream()
                .filter(r -> SubChannels.getStateInfo(r).getState() == READY)
                .collect(Collectors.toList());
        if (activeList.isEmpty()) {
            // No READY subchannels
            boolean isConnecting = false;
            Status aggStatus = EMPTY_OK;
            for (Subchannel subchannel : getSubchannels()) {
                ConnectivityStateInfo stateInfo = SubChannels.getStateInfo(subchannel);
                if (stateInfo.getState() == CONNECTING || stateInfo.getState() == IDLE) {
                    isConnecting = true;
                }
                if (aggStatus == EMPTY_OK || !aggStatus.isOk()) {
                    aggStatus = stateInfo.getStatus();
                }
            }
            updateBalancingState(isConnecting ? CONNECTING : TRANSIENT_FAILURE, new EmptyPicker(aggStatus));
        } else {
            updateBalancingState(READY, newPicker(new ArrayList<>(subchannels.values())));
        }
    }

    private void updateBalancingState(final ConnectivityState state, final AbstractPicker picker) {
        if (state == currentState && picker.isEquivalentTo(currentPicker)) {
            return;
        }
        helper.updateBalancingState(state, picker);
        currentState = state;
        currentPicker = picker;
        LOG.info("AbstractPicker update, serviceName:{}, all subchannels:{}, state:{}", serviceName, picker.getSubchannelsInfo(), state);
    }

    private Collection<Subchannel> getSubchannels() {
        return subchannels.values();
    }

    /**
     * Create new picker.
     *
     * @param list all subchannels
     * @return ReadyPicker
     */
    protected abstract AbstractReadyPicker newPicker(List<Subchannel> list);
}
