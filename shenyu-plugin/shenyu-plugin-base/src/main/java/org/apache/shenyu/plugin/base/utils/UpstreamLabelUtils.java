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

package org.apache.shenyu.plugin.base.utils;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * Shared label matching for initial routing and retries.
 */
public final class UpstreamLabelUtils {

    private UpstreamLabelUtils() {
    }

    /**
     * Select upstreams matching every label. Missing partition labels select no nodes.
     *
     * @param upstreams latest healthy upstreams
     * @param labels partition label constraints
     * @return matching upstreams without modifying the supplied list or nodes
     */
    public static List<Upstream> filter(final List<Upstream> upstreams, final Map<String, String> labels) {
        if (CollectionUtils.isEmpty(upstreams) || MapUtils.isEmpty(labels)) {
            return Collections.emptyList();
        }
        return upstreams.stream().filter(upstream -> {
            Map<String, String> metadata = upstream.getMetadata();
            return MapUtils.isNotEmpty(metadata) && metadata.entrySet().containsAll(labels.entrySet());
        })
                .collect(Collectors.toList());
    }
}
