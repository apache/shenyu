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

package org.apache.shenyu.plugin.grpc.resolver;

import com.google.common.base.Preconditions;
import com.google.common.collect.Lists;
import io.grpc.Attributes;
import io.grpc.EquivalentAddressGroup;
import io.grpc.NameResolver;
import io.grpc.Status;
import io.grpc.SynchronizationContext;
import io.grpc.internal.SharedResourceHolder;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.dto.convert.selector.GrpcUpstream;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.apache.shenyu.plugin.grpc.loadbalance.GrpcAttributeUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.Executor;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * ShenyuNameResolver.
 */
public class ShenyuNameResolver extends NameResolver implements Consumer<Object> {

    private static final Logger LOG = LoggerFactory.getLogger(ShenyuNameResolver.class);

    private boolean resolving;

    private Listener2 listener;

    private Executor executor;

    private final String appName;

    private final Attributes attributes;

    private final SynchronizationContext syncContext;

    private List<GrpcUpstream> instanceList = Lists.newArrayList();

    private final SharedResourceHolder.Resource<Executor> executorResource;

    public ShenyuNameResolver(final String appName,
                              final Args args,
                              final SharedResourceHolder.Resource<Executor> executorResource) {
        this.appName = appName;
        this.executor = args.getOffloadExecutor();
        this.executorResource = executorResource;
        this.attributes = Attributes.newBuilder().set(GrpcAttributeUtils.APP_NAME, appName).build();
        this.syncContext = Objects.requireNonNull(args.getSynchronizationContext(), "syncContext");
    }

    @Override
    public void start(final Listener2 listener) {
        Preconditions.checkState(Objects.isNull(this.listener), "already started");
        this.executor = SharedResourceHolder.get(this.executorResource);
        this.listener = Preconditions.checkNotNull(listener, "listener");
        ApplicationConfigCache.getInstance().watch(appName, this);
        resolve();
    }

    @Override
    public void accept(final Object o) {
        syncContext.execute(() -> {
            if (Objects.nonNull(this.listener)) {
                resolve();
            }
        });
    }

    @Override
    public void refresh() {
        Preconditions.checkState(Objects.nonNull(this.listener), "not started");
        resolve();
    }

    private void resolve() {
        LOG.info("Scheduled resolve for {}", this.appName);
        if (this.resolving) {
            return;
        }
        this.resolving = true;
        this.executor.execute(new Resolve(this.listener, this.instanceList));
    }

    @Override
    public String getServiceAuthority() {
        return appName;
    }

    @Override
    public void shutdown() {
        this.listener = null;
        if (Objects.nonNull(this.executor)) {
            this.executor = SharedResourceHolder.release(this.executorResource, this.executor);
        }
        this.instanceList = Lists.newArrayList();
    }

    private final class Resolve implements Runnable {

        private final Listener2 savedListener;

        private final List<GrpcUpstream> savedInstanceList;

        Resolve(final Listener2 listener, final List<GrpcUpstream> instanceList) {
            this.savedListener = Objects.requireNonNull(listener, "listener");
            this.savedInstanceList = Objects.requireNonNull(instanceList, "instanceList");
        }

        @Override
        public void run() {
            final AtomicReference<List<GrpcUpstream>> resultContainer = new AtomicReference<>();
            try {
                resultContainer.set(resolveInternal());
            } catch (final Exception e) {
                this.savedListener.onError(Status.UNAVAILABLE.withCause(e)
                        .withDescription("Failed to update server list for " + ShenyuNameResolver.this.appName));
                resultContainer.set(Lists.newArrayList());
            } finally {
                ShenyuNameResolver.this.syncContext.execute(() -> {
                    ShenyuNameResolver.this.resolving = false;
                    final List<GrpcUpstream> newInstanceList = resultContainer.get();
                    if (Objects.nonNull(newInstanceList) && Objects.nonNull(ShenyuNameResolver.this.listener)) {
                        ShenyuNameResolver.this.instanceList = newInstanceList;
                    }
                });
            }
        }

        private List<GrpcUpstream> resolveInternal() {
            final String name = ShenyuNameResolver.this.appName;
            List<GrpcUpstream> grpcUpstreamList = Optional.ofNullable(ApplicationConfigCache.getInstance().getGrpcUpstreamListCache(name)).orElse(Collections.emptyList());
            LOG.info("Got {} candidate servers for {}", grpcUpstreamList.size(), name);

            if (CollectionUtils.isEmpty(grpcUpstreamList)) {
                LOG.info("No servers found for {}", name);
                this.savedListener.onError(Status.UNAVAILABLE.withDescription("No servers found for " + name));
                return Lists.newArrayList();
            }

            if (!needsToUpdateConnections(grpcUpstreamList)) {
                LOG.info("Nothing has changed... skipping update for {}", name);
                return Collections.emptyList();
            }

            LOG.info("Ready to update server list for {}", name);
            final List<EquivalentAddressGroup> targets = grpcUpstreamList.stream()
                    .map(instance -> {
                        LOG.info("Found gRPC server {} for {}", instance.getUpstreamUrl(), name);
                        return ShenyuResolverHelper.convertToEquivalentAddressGroup(instance);
                    }).collect(Collectors.toList());

            this.savedListener.onResult(ResolutionResult.newBuilder()
                    .setAddresses(targets)
                    .setAttributes(attributes)
                    .build());
            LOG.info("Done updating server list for {}", name);

            return grpcUpstreamList;
        }

        private boolean needsToUpdateConnections(final List<GrpcUpstream> newInstanceList) {
            if (!Objects.equals(this.savedInstanceList.size(), newInstanceList.size())) {
                return true;
            }
            return this.savedInstanceList.stream().anyMatch(instance -> !newInstanceList.contains(instance));
        }
    }
}
