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

package org.apache.shenyu.admin.model.dto;

import org.apache.shenyu.common.dto.DiscoverySyncData;
import org.apache.shenyu.common.dto.DiscoveryUpstreamData;

import java.sql.Timestamp;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Gateway-only Discovery projection. Persistence identifiers remain in the Admin event.
 *
 * @param selectorId selector identity
 * @param pluginName plugin handler identity
 * @param selectorName selector name used by TCP routing
 * @param namespaceId namespace identity
 * @param upstreamDataList runtime upstream configuration
 */
public record DiscoverySyncPayload(String selectorId, String pluginName, String selectorName,
                                   String namespaceId, List<UpstreamPayload> upstreamDataList) {

    /**
     * Project without mutating the shared event or interpreting plugin-specific props.
     *
     * @param data source Discovery data
     * @return gateway payload
     */
    public static DiscoverySyncPayload from(final DiscoverySyncData data) {
        List<UpstreamPayload> upstreams = Objects.isNull(data.getUpstreamDataList()) ? null
                : data.getUpstreamDataList().stream().map(UpstreamPayload::from).collect(Collectors.toList());
        return new DiscoverySyncPayload(data.getSelectorId(), data.getPluginName(), data.getSelectorName(),
                data.getNamespaceId(), upstreams);
    }

    /**
     * Runtime fields understood by the built-in gateway handlers.
     *
     * @param dateCreated creation timestamp used for load-balancer warmup
     * @param protocol protocol prefix
     * @param url upstream address
     * @param status manual health status
     * @param weight load-balancing weight
     * @param props opaque plugin metadata
     */
    public record UpstreamPayload(Timestamp dateCreated, String protocol, String url, int status, int weight, String props) {

        private static UpstreamPayload from(final DiscoveryUpstreamData data) {
            return Objects.isNull(data) ? null : new UpstreamPayload(data.getDateCreated(), data.getProtocol(),
                    data.getUrl(), data.getStatus(), data.getWeight(), data.getProps());
        }
    }
}
