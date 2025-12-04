package org.apache.shenyu.plugin.base.provider;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;

import java.util.Collections;
import java.util.List;

public class RuleDataProvider implements DataProvider<RuleData> {
    @Override
    public List<RuleData> getData(String ruleName) {
        List<RuleData> data = BaseDataCache.getInstance().obtainRuleData(ruleName);
        return data != null ? data : Collections.emptyList();
    }
}