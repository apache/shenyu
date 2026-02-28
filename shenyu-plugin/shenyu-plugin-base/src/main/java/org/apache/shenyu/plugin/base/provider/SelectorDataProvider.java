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

package org.apache.shenyu.plugin.base.provider;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;

import java.util.List;

/**
 * A concrete implementation of the {@code DataProvider} interface that provides SelectorData
 * retrieval functionality within the Apache ShenYu gateway ecosystem.
 *
 * <p>This class implements the Data Provider pattern by serving as a specialized data source
 * for selector information, which is crucial for request routing decisions in the gateway.
 * Selectors define the coarse-grained routing rules that determine how incoming requests
 * are matched to appropriate backend services.</p>
 *
 * <p>In the ShenYu architecture, this provider works within the plugin system to deliver
 * selector configuration data that has been synchronized from the admin dashboard to the
 * gateway's local cache. This enables efficient, in-memory lookup of routing rules during
 * request processing without requiring database queries.</p>
 *
 * <p>The provider leverages the {@link BaseDataCache} singleton instance to access
 * selector information that is maintained current through ShenYu's data synchronization
 * mechanisms (WebSocket, ZooKeeper, Nacos, etc.).</p>
 *
 */
public class SelectorDataProvider implements DataProvider<SelectorData> {

    /**
     * Retrieves a list of SelectorData objects associated with the specified selector name.
     *
     * <p>This method implements the core provider contract by querying the gateway's
     * cache infrastructure for selector configuration data. Selectors contain matching
     * rules and conditions that determine which plugin and backend service should handle
     * incoming requests.</p>
     *
     * <p>In the ShenYu request processing pipeline, selectors work together with rules
     * to form a two-level routing system:
     * <ol>
     *   <li><strong>Selectors</strong> perform coarse-grained routing based on request attributes</li>
     *   <li><strong>Rules</strong> apply fine-grained processing logic within matched selectors</li>
     * </ol>
     * </p>
     *
     * <p>If no selectors are found for the given name, an empty list is returned rather
     * than null, ensuring null-safe operation for consumers of this provider.</p>
     *
     * @param selectorName the name of the selector to retrieve data for; this typically
     *                     corresponds to a plugin name or specific selector identifier
     *                     configured in the ShenYu admin dashboard
     * @return a list of SelectorData objects associated with the specified name, or an
     *         empty list if no selectors are found for the given name (never null)
     *
     */
    @Override
    public List<SelectorData> getData(final String selectorName) {
        List<SelectorData> data = BaseDataCache.getInstance().obtainSelectorData(selectorName);
        return data;
    }
}