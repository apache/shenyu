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

package org.apache.shenyu.plugin.sync.data.websocket.handler;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.DiscoveryUpstreamDataSubscriber;

import java.util.List;

public class DiscoveryUpstreamDataHandler extends AbstractDataHandler<DiscoverySyncData> {

    private final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers;

    public DiscoveryUpstreamDataHandler(final List<DiscoveryUpstreamDataSubscriber> discoveryUpstreamDataSubscribers) {
        this.discoveryUpstreamDataSubscribers = discoveryUpstreamDataSubscribers;
    }

    @Override
    protected List<DiscoverySyncData> convert(final String json) {
        return GsonUtils.getInstance().fromList(json, DiscoverySyncData.class);
    }

    @Override
    protected void doRefresh(final List<DiscoverySyncData> dataList) {
        dataList.forEach(data -> discoveryUpstreamDataSubscribers.forEach(p -> p.onSubscribe(data)));
    }

    @Override
    protected void doUpdate(final List<DiscoverySyncData> dataList) {
        dataList.forEach(data -> discoveryUpstreamDataSubscribers.forEach(p -> p.onSubscribe(data)));
    }

    @Override
    protected void doDelete(final List<DiscoverySyncData> dataList) {
        dataList.forEach(data -> discoveryUpstreamDataSubscribers.forEach(p -> p.unSubscribe(data)));
    }

}
