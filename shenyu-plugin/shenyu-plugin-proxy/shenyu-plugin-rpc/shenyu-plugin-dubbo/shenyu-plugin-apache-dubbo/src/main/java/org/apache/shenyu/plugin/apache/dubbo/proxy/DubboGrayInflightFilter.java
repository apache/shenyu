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

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * Keeps the accounting upstream's outstanding request count current for the dubbo gray
 * least-active path, without wrapping the invoker Dubbo receives.
 *
 * <p>The gray load balance marks the logical upstream it selected for the current attempt
 * ({@code DUBBO_INFLIGHT_UPSTREAM_IDENTITY}) with that upstream's full identity. The marker
 * is not the accounting target by itself, but it is honored while the selected upstream still
 * matches the provider the cluster committed to; this keeps overlapping address-only and
 * group/version entries apart. When the cluster reselects a different provider, the filter
 * counts the unique most specific compatible upstream and never attributes an ambiguous
 * overlap by list order. Wrapping the invoker instead would put a foreign object into Dubbo's
 * already-tried list, where raw-directory candidates can never compare equal to it and a
 * failed provider would be retried even when another one is available (#8132).
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
        // the marker is local bookkeeping and must never travel to the provider
        Object marker = invocation.getObjectAttachments().remove(Constants.DUBBO_INFLIGHT_UPSTREAM_IDENTITY);
        if (!(marker instanceof String) || StringUtils.isBlank((String) marker)) {
            return invoker.invoke(invocation);
        }
        Upstream upstream = matchUpstream(invocation, (String) marker, invoker.getUrl());
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

    private Upstream matchUpstream(final Invocation invocation, final String selectedKey, final URL providerUrl) {
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
        final List<Upstream> compatible = new ArrayList<>();
        for (final Upstream upstream : upstreams) {
            if (address.equals(upstream.getUrl())
                    && matchesConfigured(upstream.getGroup(), group)
                    && matchesConfigured(upstream.getVersion(), version)) {
                compatible.add(upstream);
            }
        }
        for (final Upstream upstream : compatible) {
            if (selectedKey.equals(upstream.identity().hashKey())) {
                // the load balance selected this logical upstream and it still describes the
                // committed provider, even if another overlapping entry matches as well
                return upstream;
            }
        }
        return mostSpecific(compatible);
    }

    /**
     * Resolve a reselected provider to a single logical upstream. The entry refining the
     * provider with the most configured group/version fields wins; an ambiguous overlap is
     * not attributed to either entry.
     *
     * @param compatible the upstreams matching the committed provider
     * @return the unique most specific upstream, or null when the match is ambiguous
     */
    private static Upstream mostSpecific(final List<Upstream> compatible) {
        int best = -1;
        Upstream resolved = null;
        for (final Upstream upstream : compatible) {
            int score = 0;
            if (StringUtils.isNotBlank(upstream.getGroup())) {
                score++;
            }
            if (StringUtils.isNotBlank(upstream.getVersion())) {
                score++;
            }
            if (score > best) {
                best = score;
                resolved = upstream;
            } else if (score == best) {
                resolved = null;
            }
        }
        return resolved;
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
