package org.apache.shenyu.plugin.apache.dubbo.proxy;

import org.apache.dubbo.common.URL;
import org.apache.dubbo.common.extension.ExtensionLoader;
import org.apache.dubbo.rpc.Invocation;
import org.apache.dubbo.rpc.Invoker;
import org.apache.dubbo.rpc.RpcException;
import org.apache.dubbo.rpc.cluster.LoadBalance;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DubboSelectorHandle;
import org.apache.shenyu.common.utils.CollectionUtils;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.apache.dubbo.cache.ApacheDubboRuleHandlerCache;
import org.apache.shenyu.plugin.apache.dubbo.cache.ApacheDubboSelectorHandleCache;

import java.util.List;

/**
 * ApacheDubboGrayLoadBalance.
 */
public class ApacheDubboGrayLoadBalance implements LoadBalance {

    @Override
    public <T> Invoker<T> select(final List<Invoker<T>> invokers, final URL url, final Invocation invocation) throws RpcException {
        String shenyuSelectorId = invocation.getAttachment(Constants.DUBBO_SELECTOR_ID);
        String shenyuRuleId = invocation.getAttachment(Constants.DUBBO_RULE_ID);
        String remoteAddressIp = invocation.getAttachment(Constants.DUBBO_REMOTE_ADDRESS);
        List<DubboSelectorHandle> dubboSelectorHandles = ApacheDubboSelectorHandleCache.getInstance().obtainHandle(shenyuSelectorId);
        DubboRuleHandle dubboRuleHandle = ApacheDubboRuleHandlerCache.getInstance().obtainHandle(shenyuRuleId);
        // if gray list is not empty,just use load balance to choose one.
        if (CollectionUtils.isNotEmpty(dubboSelectorHandles)){
            Upstream upstream = LoadBalancerFactory.selector(UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(shenyuSelectorId), dubboRuleHandle.getLoadbalance(), remoteAddressIp);
            return invokers.stream().filter(each -> {
                URL eachUrl = each.getUrl();
                return eachUrl.getAddress().equals(upstream.getUrl());
            }).findFirst().orElse(null);
        }
        return ExtensionLoader.getExtensionLoader(LoadBalance.class).getExtension(dubboRuleHandle.getLoadbalance()).select(invokers, url, invocation);
    }
}
