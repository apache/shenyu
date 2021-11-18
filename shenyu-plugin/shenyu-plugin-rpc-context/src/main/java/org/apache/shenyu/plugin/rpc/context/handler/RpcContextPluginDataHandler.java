package org.apache.shenyu.plugin.rpc.context.handler;

import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.dto.convert.rule.RequestHandle;
import org.apache.shenyu.common.dto.convert.rule.RpcContextHandle;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.plugin.base.RpcParamTransformPlugin;
import org.apache.shenyu.plugin.base.cache.CommonHandleCache;
import org.apache.shenyu.plugin.base.handler.PluginDataHandler;
import org.apache.shenyu.plugin.base.utils.BeanHolder;
import org.apache.shenyu.plugin.base.utils.CacheKeyUtils;

import java.util.Optional;
import java.util.function.Supplier;

/**
 * Des
 */
public class RpcContextPluginDataHandler implements PluginDataHandler {

    public static final Supplier<CommonHandleCache<String, RpcContextHandle>> CACHED_HANDLE = new BeanHolder(CommonHandleCache::new);

    @Override
    public void handlerRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData.getHandle()).ifPresent(s -> {
            RpcContextHandle rpcContextHandle = GsonUtils.getInstance().fromJson(s, RpcContextHandle.class);
            CACHED_HANDLE.get().cachedHandle(CacheKeyUtils.INST.getKey(ruleData), rpcContextHandle);
        });
    }

    @Override
    public void removeRule(final RuleData ruleData) {
        Optional.ofNullable(ruleData).ifPresent(s ->
                CACHED_HANDLE.get().removeHandle(CacheKeyUtils.INST.getKey(ruleData)));
    }

    @Override
    public String pluginNamed() {
        return null;
    }
}
