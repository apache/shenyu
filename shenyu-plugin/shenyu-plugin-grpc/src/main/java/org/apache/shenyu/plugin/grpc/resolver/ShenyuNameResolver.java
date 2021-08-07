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
import io.grpc.NameResolver;
import io.grpc.Status;
import io.grpc.EquivalentAddressGroup;
import io.grpc.SynchronizationContext;
import io.grpc.internal.SharedResourceHolder;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.plugin.grpc.loadbalance.GrpcAttributeUtils;
import org.apache.shenyu.plugin.grpc.cache.ApplicationConfigCache;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Map;
import java.util.Objects;
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

    private final List<ShenyuServiceInstance> keep = null;

    private List<ShenyuServiceInstance> instanceList = Lists.newArrayList();

    private final SharedResourceHolder.Resource<Executor> executorResource;

    public ShenyuNameResolver(final String appName, final Args args, final SharedResourceHolder.Resource<Executor> executorResource) {
        this.appName = appName;
        this.executor = args.getOffloadExecutor();
        this.executorResource = executorResource;
        this.attributes = Attributes.newBuilder().set(GrpcAttributeUtils.appName(), appName).build();
        this.syncContext = Objects.requireNonNull(args.getSynchronizationContext(), "syncContext");
    }

    @Override
    public void start(final Listener2 listener) {
        Preconditions.checkState(this.listener == null, "already started");
        this.executor = SharedResourceHolder.get(this.executorResource);
        this.listener = Preconditions.checkNotNull(listener, "listener");
        ApplicationConfigCache.getInstance().watch(appName, this);
        resolve();
    }

    @Override
    public void accept(final Object o) {
        syncContext.execute(() -> {
            if (this.listener != null) {
                resolve();
            }
        });
    }

    @Override
    public void refresh() {
        Preconditions.checkState(this.listener != null, "not started");
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
        if (this.executor != null) {
            this.executor = SharedResourceHolder.release(this.executorResource, this.executor);
        }
        this.instanceList = Lists.newArrayList();
    }

    private final class Resolve implements Runnable {

        private final Listener2 savedListener;

        private final List<ShenyuServiceInstance> savedInstanceList;

        Resolve(final Listener2 listener, final List<ShenyuServiceInstance> instanceList) {
            this.savedListener = Objects.requireNonNull(listener, "listener");
            this.savedInstanceList = Objects.requireNonNull(instanceList, "instanceList");
        }

        @Override
        public void run() {
            final AtomicReference<List<ShenyuServiceInstance>> resultContainer = new AtomicReference<>();
            try {
                resultContainer.set(resolveInternal());
            } catch (final Exception e) {
                this.savedListener.onError(Status.UNAVAILABLE.withCause(e)
                        .withDescription("Failed to update server list for " + ShenyuNameResolver.this.appName));
                resultContainer.set(Lists.newArrayList());
            } finally {
                ShenyuNameResolver.this.syncContext.execute(() -> {
                    ShenyuNameResolver.this.resolving = false;
                    final List<ShenyuServiceInstance> newInstanceList = resultContainer.get();
                    if (newInstanceList != keep && ShenyuNameResolver.this.listener != null) {
                        ShenyuNameResolver.this.instanceList = newInstanceList;
                    }
                });
            }
        }

        private List<ShenyuServiceInstance> resolveInternal() {
            final String name = ShenyuNameResolver.this.appName;
            ShenyuServiceInstanceLists shenyuServiceInstanceLists = ApplicationConfigCache.getInstance().get(name);
            List<ShenyuServiceInstance> newInstanceList = shenyuServiceInstanceLists.getCopyInstances();
            LOG.info("Got {} candidate servers for {}", newInstanceList.size(), name);
            if (CollectionUtils.isEmpty(newInstanceList)) {
                LOG.info("No servers found for {}", name);
                this.savedListener.onError(Status.UNAVAILABLE.withDescription("No servers found for " + name));
                return Lists.newArrayList();
            }
            if (!needsToUpdateConnections(newInstanceList)) {
                LOG.info("Nothing has changed... skipping update for {}", name);
                return null;
            }
            LOG.info("Ready to update server list for {}", name);
            final List<EquivalentAddressGroup> targets = newInstanceList.stream()
                    .map(instance -> {
                        LOG.info("Found gRPC server {}:{} for {}", instance.getHost(), instance.getPort(), name);
                        return ShenyuResolverHelper.convertToEquivalentAddressGroup(instance);
                    }).collect(Collectors.toList());
            this.savedListener.onResult(ResolutionResult.newBuilder()
                    .setAddresses(targets)
                    .setAttributes(attributes)
                    .build());
            LOG.info("Done updating server list for {}", name);
            return newInstanceList;
        }

        private boolean needsToUpdateConnections(final List<ShenyuServiceInstance> newInstanceList) {
            if (this.savedInstanceList.size() != newInstanceList.size()) {
                return true;
            }
            for (final ShenyuServiceInstance instance : this.savedInstanceList) {
                final String host = instance.getHost();
                final int port = instance.getPort();
                boolean isSame = newInstanceList.stream().anyMatch(newInstance -> host.equals(newInstance.getHost())
                        && port == newInstance.getPort()
                        && isMetadataEquals(instance.getMetadata(), newInstance.getMetadata()));
                if (!isSame) {
                    return true;
                }
            }
            return false;
        }

        private boolean isMetadataEquals(final Map<String, String> metadata, final Map<String, String> newMetadata) {
            final String[] keys = {"weight", "status"};
            for (String key : keys) {
                final String value = metadata.get(key);
                final String newValue = newMetadata.get(key);
                if (!Objects.equals(value, newValue)) {
                    return false;
                }
            }
            return true;
        }
    }
}
