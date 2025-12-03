package org.apache.shenyu.plugin.base.provider;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;

import java.util.Collections;
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
    public List<SelectorData> getData(String selectorName) {
        List<SelectorData> data = BaseDataCache.getInstance().obtainSelectorData(selectorName);
        return data != null ? data : Collections.emptyList();
    }
}