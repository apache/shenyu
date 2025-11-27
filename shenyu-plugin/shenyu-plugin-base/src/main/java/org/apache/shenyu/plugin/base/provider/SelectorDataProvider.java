package org.apache.shenyu.plugin.base.provider;

import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;

import java.util.Collections;
import java.util.List;

public class SelectorDataProvider implements DataProvider<SelectorData> {
    @Override
    public List<SelectorData> getData(String selectorName) {
        List<SelectorData> data = BaseDataCache.getInstance().obtainSelectorData(selectorName);
        return data != null ? data : Collections.emptyList();
    }
}