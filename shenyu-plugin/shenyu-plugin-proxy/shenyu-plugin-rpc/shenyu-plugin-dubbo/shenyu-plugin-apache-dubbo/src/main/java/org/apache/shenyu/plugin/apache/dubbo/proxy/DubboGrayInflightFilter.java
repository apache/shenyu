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
import org.apache.dubbo.common.extension.Activate;
import org.apache.dubbo.rpc.Filter;
import org.apache.dubbo.rpc.Invocation;
import org.apache.dubbo.rpc.Invoker;
import org.apache.dubbo.rpc.Result;
import org.apache.dubbo.rpc.RpcException;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.loadbalancer.cache.UpstreamCacheManager;
import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Keeps the accounting upstream's outstanding request count current for the dubbo gray
 * least-active path, without wrapping the invoker Dubbo receives.
 *
 * <p>The gray load balance marks the upstream it proposed for the current attempt
 * ({@code DUBBO_INFLIGHT_UPSTREAM_URL}). The marker only says that this attempt came from
 * the gray least-active path: it is not the accounting target, because the cluster may
 * reselect a different provider after the load balance returned without calling it again.
 * The filter therefore resolves the invoker the cluster actually committed to (address,
 * group and version) against the selector's upstreams and counts that one. Wrapping the
 * invoker instead would put a foreign object into Dubbo's already-tried list, where
 * raw-directory candidates can never compare equal to it and a failed provider would be
 * retried even when another one is available (#8132).
 *
 * <p>The count is increased when the RPC starts, not when the invoker is selected, and it is
 * released exactly once per attempt, either when the returned result completes or when the
 * invocation throws before producing one. The marker attachment is removed before the call
 * continues towards the remote peer, so it never reaches the provider.
 */
@Activate(group = CommonConstants.CONSUMER)
public class DubboGrayInflightFilter implements Filter {

    @Override
    public Result invoke(final Invoker<?> invoker, final Invocation invocation) throws RpcException {
        // the marker is local bookkeeping and must never travel to the provider; it only
        // says this attempt came from the gray least-active path
        boolean grayAttempt = Objects.nonNull(
                invocation.getObjectAttachments().remove(Constants.DUBBO_INFLIGHT_UPSTREAM_URL));
        if (!grayAttempt) {
            return invoker.invoke(invocation);
        }
        Upstream upstream = matchUpstream(invocation, invoker.getUrl());
        if (Objects.isNull(upstream)) {
            return invoker.invoke(invocation);
        }
        upstream.getInflight().incrementAndGet();
        AtomicBoolean released = new AtomicBoolean();
        Result result;
        try {
            result = invoker.invoke(invocation);
        } catch (RuntimeException e) {
            // RpcException is a RuntimeException; anything thrown before a result exists still releases.
            release(released, upstream);
            throw e;
        }
        if (Objects.isNull(result)) {
            release(released, upstream);
            return null;
        }
        return result.whenCompleteWithContext((response, throwable) -> release(released, upstream));
    }

    private Upstream matchUpstream(final Invocation invocation, final URL providerUrl) {
        String selectorId = invocation.getAttachment(Constants.DUBBO_SELECTOR_ID);
        if (StringUtils.isBlank(selectorId) || Objects.isNull(providerUrl) || StringUtils.isBlank(providerUrl.getAddress())) {
            return null;
        }
        List<Upstream> upstreams = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorId);
        if (CollectionUtils.isEmpty(upstreams)) {
            return null;
        }
        final String address = providerUrl.getAddress();
        final String group = StringUtils.trimToNull(providerUrl.getParameter(Constants.GROUP));
        final String version = StringUtils.trimToNull(providerUrl.getParameter(Constants.VERSION));
        return upstreams.stream()
                .filter(upstream -> address.equals(upstream.getUrl()))
                .filter(upstream -> matchesConfigured(upstream.getGroup(), group))
                .filter(upstream -> matchesConfigured(upstream.getVersion(), version))
                .findFirst()
                .orElse(null);
    }

    /**
     * Whether the committed provider satisfies a configured group or version. A blank
     * configuration is a wildcard, which is how the gray load balance filters invokers.
     *
     * @param configured the configured group or version
     * @param actual the committed provider's group or version
     * @return true when the field does not restrict the provider
     */
    private static boolean matchesConfigured(final String configured, final String actual) {
        return StringUtils.isBlank(configured) || StringUtils.trim(configured).equals(actual);
    }

    private void release(final AtomicBoolean released, final Upstream upstream) {
        if (released.compareAndSet(false, true)) {
            upstream.getInflight().decrementAndGet();
        }
    }
}
