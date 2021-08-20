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
import io.grpc.ConnectivityStateInfo;
import io.grpc.EquivalentAddressGroup;
import io.grpc.LoadBalancer;
import io.grpc.ConnectivityState;

/**
 * The grpc SubChannels.
 */
public class SubChannels {

    private static final Attributes.Key<Ref<ConnectivityStateInfo>> STATE_INFO_KEY = Attributes.Key.create("state-info");

    private static final Attributes.Key<Ref<Integer>> WEIGHT_KEY = Attributes.Key.create("weight");

    private static final Attributes.Key<Ref<String>> STATSU_KEY = Attributes.Key.create("status");

    /**
     * CreateSubChannel.
     *
     * @param helper       helper
     * @param addressGroup addressGroup
     * @param attributes   attributes
     * @return LoadBalancer.Subchannel  subchannel
     */
    public static LoadBalancer.Subchannel createSubChannel(final LoadBalancer.Helper helper,
                                                           final EquivalentAddressGroup addressGroup,
                                                           final Attributes attributes) {
        final Attributes newAttributes = attributes.toBuilder()
                .set(STATE_INFO_KEY, new Ref<>(ConnectivityStateInfo.forNonError(ConnectivityState.IDLE)))
                .build();
        return helper.createSubchannel(LoadBalancer.CreateSubchannelArgs
                .newBuilder()
                .setAddresses(addressGroup)
                .setAttributes(newAttributes)
                .build()
        );
    }

    /**
     * Create Attributes.
     *
     * @param weight weight
     * @param status status
     * @return Attributes attributes
     */
    public static Attributes createAttributes(final int weight, final String status) {
        return Attributes.newBuilder()
                .set(WEIGHT_KEY, new Ref<>(weight))
                .set(STATSU_KEY, new Ref<>(status))
                .build();
    }

    /**
     * Get weight. weight
     *
     * @param subchannel subchannel
     * @return int i
     */
    public static int getWeight(final LoadBalancer.Subchannel subchannel) {
        return getAttributeValue(subchannel, WEIGHT_KEY, 0);
    }

    /**
     * Get status.
     *
     * @param subchannel subchannel
     * @return String status
     */
    public static String getStatus(final LoadBalancer.Subchannel subchannel) {
        return getAttributeValue(subchannel, STATSU_KEY, "true");
    }

    /**
     * Get ConnectivityStateInfo.
     *
     * @param subchannel subchannel
     * @return ConnectivityStateInfo info
     */
    public static ConnectivityStateInfo getStateInfo(final LoadBalancer.Subchannel subchannel) {
        return getAttributeValue(subchannel, STATE_INFO_KEY, null);
    }

    /**
     * SetStateInfo.
     *
     * @param subchannel subchannel
     * @param value      value
     */
    public static void setStateInfo(final LoadBalancer.Subchannel subchannel, final ConnectivityStateInfo value) {
        setAttributeValue(subchannel, STATE_INFO_KEY, value);
    }

    private static <T> T getAttributeValue(final LoadBalancer.Subchannel subchannel, final Attributes.Key<Ref<T>> key, final T defaultValue) {
        final Ref<T> ref = subchannel.getAttributes().get(key);
        return ref == null ? defaultValue : ref.value;
    }

    private static <T> void setAttributeValue(final LoadBalancer.Subchannel subchannel, final Attributes.Key<Ref<T>> key, final T newValue) {
        final Ref<T> targetRef = subchannel.getAttributes().get(key);
        if (targetRef != null) {
            targetRef.value = newValue;
        }
    }

    /**
     * Set AttributeValue.
     *
     * @param subchannel    subchannel
     * @param key           key
     * @param newAttributes newAttributes
     * @param <T>           t
     */
    private static <T> void setAttributeValue(final LoadBalancer.Subchannel subchannel, final Attributes.Key<Ref<T>> key, final Attributes newAttributes) {
        final Ref<T> newValueRef = newAttributes.get(key);
        if (newValueRef != null) {
            setAttributeValue(subchannel, key, newValueRef.value);
        }
    }

    /**
     * UpdateAttributes.
     *
     * @param subchannel newAttributes
     * @param attributes attributes
     */
    public static void updateAttributes(final LoadBalancer.Subchannel subchannel, final Attributes attributes) {
        setAttributeValue(subchannel, WEIGHT_KEY, attributes);
        setAttributeValue(subchannel, STATSU_KEY, attributes);
    }

    static final class Ref<T> {

        private T value;

        Ref(final T value) {
            this.value = value;
        }

        public T getValue() {
            return value;
        }

        public void setValue(final T value) {
            this.value = value;
        }
    }
}
