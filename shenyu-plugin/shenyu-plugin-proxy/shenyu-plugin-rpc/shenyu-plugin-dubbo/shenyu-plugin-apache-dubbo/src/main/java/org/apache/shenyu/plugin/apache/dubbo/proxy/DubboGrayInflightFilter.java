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
 * Keeps the selected upstream's outstanding request count current for the dubbo gray
 * least-active path, without wrapping the invoker Dubbo receives.
 *
 * <p>The gray load balance marks the upstream it selected for the current attempt
 * ({@code DUBBO_INFLIGHT_UPSTREAM_URL}); this consumer-side filter runs after the cluster
 * has committed to a provider, so counting here sees every actual attempt, including ones
 * produced by a retry reselection. Wrapping the invoker instead would put a foreign object
 * into Dubbo's already-tried list, where raw-directory candidates can never compare equal
 * to it and a failed provider would be retried even when another one is available (#8132).
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
        Object upstreamUrl = invocation.getObjectAttachment(Constants.DUBBO_INFLIGHT_UPSTREAM_URL);
        // the marker is local bookkeeping and must never travel to the provider
        invocation.getObjectAttachments().remove(Constants.DUBBO_INFLIGHT_UPSTREAM_URL);
        if (Objects.isNull(upstreamUrl)) {
            return invoker.invoke(invocation);
        }
        Upstream upstream = matchUpstream(invocation, String.valueOf(upstreamUrl));
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

    private Upstream matchUpstream(final Invocation invocation, final String upstreamUrl) {
        String selectorId = invocation.getAttachment(Constants.DUBBO_SELECTOR_ID);
        if (StringUtils.isBlank(selectorId)) {
            return null;
        }
        List<Upstream> upstreams = UpstreamCacheManager.getInstance().findUpstreamListBySelectorId(selectorId);
        if (CollectionUtils.isEmpty(upstreams)) {
            return null;
        }
        return upstreams.stream()
                .filter(upstream -> upstreamUrl.equals(upstream.getUrl()))
                .findFirst()
                .orElse(null);
    }

    private void release(final AtomicBoolean released, final Upstream upstream) {
        if (released.compareAndSet(false, true)) {
            upstream.getInflight().decrementAndGet();
        }
    }
}
