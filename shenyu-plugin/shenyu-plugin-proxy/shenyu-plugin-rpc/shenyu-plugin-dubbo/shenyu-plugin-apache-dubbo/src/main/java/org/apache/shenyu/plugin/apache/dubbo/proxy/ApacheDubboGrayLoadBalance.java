/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.shenyu.plugin.apache.dubbo.proxy;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.dubbo.common.URL;
import org.apache.dubbo.common.constants.CommonConstants;
import org.apache.dubbo.common.extension.ExtensionLoader;
import org.apache.dubbo.rpc.Invocation;
import org.apache.dubbo.rpc.Invoker;
import org.apache.dubbo.rpc.RpcException;
import org.apache.dubbo.rpc.cluster.LoadBalance;
import org.apache.dubbo.rpc.support.RpcUtils;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.rule.impl.DubboRuleHandle;
import org.apache.shenyu.common.dto.convert.selector.DubboUpstream;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;
import org.apache.shenyu.loadbalancer.factory.LoadBalancerFactory;
import org.apache.shenyu.plugin.apache.dubbo.handler.ApacheDubboPluginDataHandler;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * ApacheDubboGrayLoadBalance.
 */
public class ApacheDubboGrayLoadBalance implements LoadBalance {

    @Override
    public <T> Invoker<T> select(final List<Invoker<T>> invokers, final URL url, final Invocation invocation) throws RpcException {
        String shenyuSelectorId = invocation.getAttachment(Constants.DUBBO_SELECTOR_ID);
        String shenyuRuleId = invocation.getAttachment(Constants.DUBBO_RULE_ID);
        String remoteAddressIp = invocation.getAttachment(Constants.DUBBO_REMOTE_ADDRESS);
        List<DubboUpstream> dubboUpstreams = ApacheDubboPluginDataHandler.SELECTOR_CACHED_HANDLE.get().obtainHandle(shenyuSelectorId);
        DubboRuleHandle dubboRuleHandle = ApacheDubboPluginDataHandler.RULE_CACHED_HANDLE.get().obtainHandle(shenyuRuleId);
        // if gray list is not empty,just use load balance to choose one.
        if (CollectionUtils.isNotEmpty(dubboUpstreams)) {
            Upstream upstream = LoadBalancerFactory.selector(UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(shenyuSelectorId), dubboRuleHandle.getLoadBalance(), remoteAddressIp);
            if (Objects.isNull(upstream)) {
                return dubboSelect(invokers, url, invocation);
            }
            if (StringUtils.isBlank(upstream.getUrl()) && StringUtils.isBlank(upstream.getGroup()) && StringUtils.isBlank(upstream.getVersion())) {
                return dubboSelect(invokers, url, invocation);
            }
            // url is the first level, then is group, the version is the lowest.
            final List<Invoker<T>> invokerGrays = invokers.stream().filter(each -> {
                if (StringUtils.isNotBlank(upstream.getUrl())) {
                    URL eachUrl = each.getUrl();
                    return eachUrl.getAddress().equals(upstream.getUrl());
                }
                return true;
            }).filter(each -> {
                if (StringUtils.isNotBlank(upstream.getGroup())) {
                    final URL eachUrl = each.getUrl();
                    return upstream.getGroup().equals(eachUrl.getParameter(Constants.GROUP));
                }
                return true;
            }).filter(each -> {
                if (StringUtils.isNotBlank(upstream.getVersion())) {
                    final URL eachUrl = each.getUrl();
                    return upstream.getVersion().equals(eachUrl.getParameter(Constants.VERSION));
                }
                return true;
            }).collect(Collectors.toList());
            if (CollectionUtils.isEmpty(invokerGrays)) {
                return dubboSelect(invokers, url, invocation);
            }
            return dubboSelect(invokerGrays, url, invocation);
        }
        return dubboSelect(invokers, url, invocation);
    }
    
    private <T> Invoker<T> dubboSelect(final List<Invoker<T>> invokers, final URL url, final Invocation invocation) {
        String loadBalance = Optional.ofNullable(url.getMethodParameter(RpcUtils.getMethodName(invocation), Constants.DUBBO_LOAD_BALANCE)).orElse(CommonConstants.DEFAULT_LOADBALANCE);
        return ExtensionLoader.getExtensionLoader(LoadBalance.class).getExtension(loadBalance).select(invokers, url, invocation);
    }
}
