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

import org.apache.dubbo.common.URL;
import org.apache.dubbo.rpc.Invocation;
import org.apache.dubbo.rpc.Invoker;
import org.apache.dubbo.rpc.Result;
import org.apache.dubbo.rpc.RpcException;
import org.apache.shenyu.loadbalancer.entity.Upstream;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * An {@link Invoker} that keeps the upstream's outstanding request count up to date.
 *
 * <p>The count is increased when the RPC starts, not when the invoker is selected: Dubbo's cluster
 * layer may pick an invoker and then discard the pick (it was already tried, it is unavailable, or a
 * reselect replaced it), and a selection-time reservation would leak those counts forever. The count
 * is released exactly once per invocation, either when the returned result completes or when the
 * invocation throws before producing one.
 *
 * <p>Every method other than {@link #invoke(Invocation)} is delegated, and {@link #equals(Object)} and
 * {@link #hashCode()} delegate to the wrapped invoker as well. Dubbo's cluster keeps the selected
 * invoker in its "already tried" list and in its connectivity-invalidation set, and compares by
 * identity there; a wrapper that did not delegate equality would make retries select the same provider
 * again and would stop unhealthy providers from being removed.
 */
final class InflightInvoker<T> implements Invoker<T> {

    private final Invoker<T> delegate;

    private final Upstream upstream;

    InflightInvoker(final Invoker<T> delegate, final Upstream upstream) {
        this.delegate = delegate;
        this.upstream = upstream;
    }

    @Override
    public Class<T> getInterface() {
        return delegate.getInterface();
    }

    @Override
    public Result invoke(final Invocation invocation) throws RpcException {
        upstream.getInflight().incrementAndGet();
        // The guard belongs to this call, not to the invoker: an Invoker may be invoked many times, and a
        // shared flag would let the second call's count go unreleased.
        AtomicBoolean released = new AtomicBoolean();
        Result result;
        try {
            result = delegate.invoke(invocation);
        } catch (RuntimeException e) {
            // RpcException is a RuntimeException; anything thrown before a result exists still releases.
            release(released);
            throw e;
        }
        if (Objects.isNull(result)) {
            release(released);
            return null;
        }
        return result.whenCompleteWithContext((response, throwable) -> release(released));
    }

    @Override
    public URL getUrl() {
        return delegate.getUrl();
    }

    @Override
    public boolean isAvailable() {
        return delegate.isAvailable();
    }

    @Override
    public void destroy() {
        delegate.destroy();
    }

    private void release(final AtomicBoolean released) {
        if (released.compareAndSet(false, true)) {
            upstream.getInflight().decrementAndGet();
        }
    }

    @Override
    public String toString() {
        return delegate.toString();
    }

    @Override
    public boolean equals(final Object other) {
        if (this == other) {
            return true;
        }
        if (other instanceof InflightInvoker) {
            return delegate.equals(((InflightInvoker<?>) other).delegate);
        }
        return delegate.equals(other);
    }

    @Override
    public int hashCode() {
        return delegate.hashCode();
    }
}
