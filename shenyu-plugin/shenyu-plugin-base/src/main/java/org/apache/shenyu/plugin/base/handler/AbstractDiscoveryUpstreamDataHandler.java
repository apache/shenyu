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

package org.apache.shenyu.plugin.base.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamKey;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * The common discovery upstream data handler.
 *
 * <p>Implements the shared null-check, gray-split and submit flow for discovery upstream data,
 * so that the concrete handlers only provide their upstream conversion and cache access.
 *
 * @param <T> the upstream type
 */
public abstract class AbstractDiscoveryUpstreamDataHandler<T> implements DiscoveryUpstreamDataHandler {

    @Override
    public void handlerDiscoveryUpstreamData(final DiscoverySyncData discoverySyncData) {
        if (Objects.isNull(discoverySyncData) || Objects.isNull(discoverySyncData.getSelectorId())) {
            return;
        }
        final String selectorId = discoverySyncData.getSelectorId();
        final List<T> upstreamList = convertUpstreamList(discoverySyncData.getUpstreamDataList());
        final List<T> grayUpstreamList = upstreamList.stream().filter(this::isGray).collect(Collectors.toList());
        if (grayUpstreamList.isEmpty()) {
            submitUpstreamData(selectorId, upstreamList);
        } else {
            submitUpstreamData(selectorId, grayUpstreamList);
        }
        afterSubmitUpstreamData(selectorId);
    }

    @Override
    public void removeDiscoveryUpstreamData(final DiscoveryUpstreamKey key) {
        if (Objects.isNull(key) || Objects.isNull(key.selectorId())) {
            return;
        }
        removeUpstreamData(key.selectorId());
    }

    protected abstract List<T> convertUpstreamList(List<DiscoveryUpstreamData> upstreamList);

    protected abstract boolean isGray(T upstream);

    protected abstract void submitUpstreamData(String selectorId, List<T> upstreamList);

    protected abstract void removeUpstreamData(String selectorId);

    protected void afterSubmitUpstreamData(final String selectorId) {
    }
}
