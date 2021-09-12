package org.apache.shenyu.plugin.apache.dubbo.cache;

import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.plugin.base.cache.RuleHandleCache;

/**
 * ApacheDubboRuleHandlerCache.
 */
public class ApacheDubboRuleHandlerCache extends RuleHandleCache<String, DubboRuleHandle> {

    private ApacheDubboRuleHandlerCache() {
    }

    /**
     * Gets instance.
     *
     * @return the instance
     */
    public static ApacheDubboRuleHandlerCache getInstance() {
        return ApacheDubboRuleHandlerCache.ApacheDubboRuleHandlerCacheInstance.INSTANCE;
    }

    /**
     * The type selector handle cache instance.
     */
    static class ApacheDubboRuleHandlerCacheInstance {
        /**
         * The Instance.
         */
        static final ApacheDubboRuleHandlerCache INSTANCE = new ApacheDubboRuleHandlerCache();
    }
}
