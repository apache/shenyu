package org.apache.shenyu.plugin.apache.dubbo.cache;

import org.apache.shenyu.common.dto.convert.selector.DubboSelectorHandle;
import org.apache.shenyu.plugin.base.cache.RuleHandleCache;

import java.util.List;

/**
 * The dubbo selector handle cache.
 */
public class ApacheDubboSelectorHandleCache extends RuleHandleCache<String, List<DubboSelectorHandle>> {

    private ApacheDubboSelectorHandleCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ApacheDubboSelectorHandleCache getInstance() {
        return ApacheDubboSelectorHandleCacheInstance.INSTANCE;
    }

    /**
     * The type selector handle cache instance.
     */
    static class ApacheDubboSelectorHandleCacheInstance {
        /**
         * The Instance.
         */
        static final ApacheDubboSelectorHandleCache INSTANCE = new ApacheDubboSelectorHandleCache();
    }
}
