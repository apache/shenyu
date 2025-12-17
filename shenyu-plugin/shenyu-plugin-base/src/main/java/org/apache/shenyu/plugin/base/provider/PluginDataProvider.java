package org.apache.shenyu.plugin.base.provider;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;

import java.util.Collections;
import java.util.List;

/**
 * A concrete implementation of the data provider pattern for retrieving plugin data
 * in the ShenYu gateway system.
 * <p>
 * This class is responsible for fetching {@link PluginData} objects from the underlying
 * data cache ({@link BaseDataCache}) based on the provided plugin name. It implements
 * the {@code DataProvider<PluginData>} interface, providing a standardized way to
 * access plugin configuration data throughout the application.
 * </p>
 * <p>
 * In the ShenYu plugin architecture, this class serves as a specific layer in the data
 * access mechanism, interacting with the system's caching infrastructure through an
 * data provider interface to efficiently supply plugin information to
 * upstream services or components that require it.
 * </p>
 *
 * @see org.apache.shenyu.plugin.base.cache.BaseDataCache
 * @see PluginData
 */
public class PluginDataProvider implements DataProvider<PluginData> {

    /**
     * Retrieves plugin data from the base data cache for the specified plugin name.
     * <p>
     * This method queries the internally maintained cache instance. If plugin data
     * matching the given name is found, it is wrapped in a singleton list and returned.
     * If no matching data is found (i.e., the plugin name does not exist in the cache),
     * an empty immutable list is returned.
     * </p>
     *
     * @param pluginName the name of the plugin to retrieve. This parameter should not be null,
     *                   though the current implementation may return an empty list rather than
     *                   throwing an exception for null values.
     * @return an immutable list containing a single {@link PluginData} object if the plugin
     *         is found, otherwise an empty immutable list. The return value will never be null.
     */
    @Override
    public List<PluginData> getData(String pluginName) {
        PluginData data = BaseDataCache.getInstance().obtainPluginData(pluginName);
        return data != null ? Collections.singletonList(data) : Collections.emptyList();
    }
}