package org.apache.shenyu.plugin.base.provider;

import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;

import java.util.Collections;
import java.util.List;

public class PluginDataProvider implements DataProvider<PluginData> {
    @Override
    public List<PluginData> getData(String pluginName) {
        PluginData data = BaseDataCache.getInstance().obtainPluginData(pluginName);
        return data != null ? Collections.singletonList(data) : Collections.emptyList();
    }
}