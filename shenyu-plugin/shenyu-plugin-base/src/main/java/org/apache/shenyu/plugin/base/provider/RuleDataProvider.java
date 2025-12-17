package org.apache.shenyu.plugin.base.provider;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.plugin.base.cache.BaseDataCache;
import java.util.Collections;
import java.util.List;

/**
 * A data provider implementation for retrieving rule data from the base data cache.
 * This class follows the Data Provider pattern to abstract data access logic and provide
 * a consistent interface for obtaining rule configuration data throughout the Shenyu plugin system.
 *
 * <p>The RuleDataProvider serves as a bridge between the plugin infrastructure and the cached
 * rule data, ensuring efficient data retrieval while maintaining separation of concerns.</p>
 */
public class RuleDataProvider implements DataProvider<RuleData> {

    /**
     * Retrieves rule data from the base data cache based on the specified rule name.
     * This method implements the data provider pattern by delegating to the singleton
     *
     */
    @Override
    public List<RuleData> getData(String ruleName) {
        List<RuleData> data = BaseDataCache.getInstance().obtainRuleData(ruleName);
        return data != null ? data : Collections.emptyList();
    }
}