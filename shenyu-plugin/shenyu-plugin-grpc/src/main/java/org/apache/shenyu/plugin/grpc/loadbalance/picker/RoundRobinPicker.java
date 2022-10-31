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

import io.grpc.LoadBalancer;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.plugin.grpc.loadbalance.SubChannelCopy;

import java.util.List;
import java.util.concurrent.atomic.AtomicIntegerFieldUpdater;

/**
 * RoundRobin picker.
 */
public class RoundRobinPicker extends AbstractReadyPicker {

    private static final AtomicIntegerFieldUpdater<RoundRobinPicker> INDEX_UPDATER = AtomicIntegerFieldUpdater.newUpdater(RoundRobinPicker.class, "index");

    /**
     * AtomicIntegerFieldUpdater index.
     */
    @SuppressWarnings("unused")
    private volatile int index;

    public RoundRobinPicker(final List<LoadBalancer.Subchannel> list) {
        super(list);
    }

    @Override
    protected SubChannelCopy pick(final List<SubChannelCopy> list) {
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }
        final int size = list.size();
        if (size == 1) {
            return list.get(0);
        }
        int i = INDEX_UPDATER.incrementAndGet(this);
        if (i >= size) {
            int oldi = i;
            i %= size;
            INDEX_UPDATER.compareAndSet(this, oldi, i);
        }
        return list.get(i);
    }
}
