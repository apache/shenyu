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

package org.apache.shenyu.infra.etcd.client;

import io.etcd.jetcd.ByteSequence;
import io.etcd.jetcd.Client;
import io.etcd.jetcd.KV;
import io.etcd.jetcd.KeyValue;
import io.etcd.jetcd.Watch;
import io.etcd.jetcd.kv.GetResponse;
import io.etcd.jetcd.lease.LeaseKeepAliveResponse;
import io.etcd.jetcd.options.DeleteOption;
import io.etcd.jetcd.options.GetOption;
import io.etcd.jetcd.options.PutOption;
import io.etcd.jetcd.options.WatchOption;
import io.etcd.jetcd.watch.WatchEvent;
import io.grpc.stub.StreamObserver;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static java.nio.charset.StandardCharsets.UTF_8;

/**
 * Etcd client of Bootstrap.
 */
public class EtcdClient {

    private static final Logger LOG = LoggerFactory.getLogger(EtcdClient.class);
    
    private static final int MAX_RETRY_DELAY_SECONDS = 60;
    
    private static final int INITIAL_RETRY_DELAY_SECONDS = 1;

    private final Client client;

    private final long ttl;

    private final long timeout;

    private long globalLeaseId;

    private final ConcurrentHashMap<String, Watch.Watcher> watchCache = new ConcurrentHashMap<>();

    private final ConcurrentHashMap<String, Watch.Watcher> watchChildCache = new ConcurrentHashMap<>();

    private volatile boolean keepAliveRunning = true;

    private volatile StreamObserver<LeaseKeepAliveResponse> currentObserver;

    private final ScheduledExecutorService retryExecutor;

    private volatile ScheduledFuture<?> retryFuture;

    private final AtomicInteger retryCount = new AtomicInteger(0);

    public EtcdClient(final Client client, final long ttl, final long timeout) {

        this.ttl = ttl;
        this.timeout = timeout;
        this.client = client;
        this.retryExecutor = new ScheduledThreadPoolExecutor(1, r -> {
            Thread thread = new Thread(r, "etcd-keepalive-retry");
            thread.setDaemon(true);
            return thread;
        });

        initLease();
    }

    /**
     * close client.
     */
    public void close() {
        keepAliveRunning = false;
        if (Objects.nonNull(retryFuture)) {
            retryFuture.cancel(false);
        }
        retryExecutor.shutdown();
        try {
            if (!retryExecutor.awaitTermination(5, TimeUnit.SECONDS)) {
                retryExecutor.shutdownNow();
            }
        } catch (InterruptedException e) {
            retryExecutor.shutdownNow();
            Thread.currentThread().interrupt();
        }
        this.client.close();
    }

    /**
     * get node value.
     *
     * @param key node name
     * @return string
     */
    public String get(final String key) {
        List<KeyValue> keyValues = null;
        try {
            keyValues = client.getKVClient().get(bytesOf(key)).get().getKvs();
        } catch (InterruptedException | ExecutionException e) {
            LOG.error("get key error, key:{}", key, e);
        }

        if (CollectionUtils.isEmpty(keyValues)) {
            LOG.warn("get key {} is empty", key);
            return null;
        }
        return keyValues.iterator().next().getValue().toString(UTF_8);
    }

    /**
     * get keys by prefix.
     *
     * @param prefix key prefix.
     * @return key valuesMap.
     */
    public Map<String, String> getKeysMapByPrefix(final String prefix) {
        GetOption getOption = GetOption.newBuilder()
                .isPrefix(true)
                .build();
        try {
            return this.client.getKVClient().get(bytesOf(prefix), getOption)
                    .get().getKvs().stream()
                    .collect(Collectors.toMap(e -> e.getKey().toString(UTF_8), e -> e.getValue().toString(UTF_8)));
        } catch (ExecutionException | InterruptedException e) {
            LOG.error("etcd getKeysMapByPrefix key {} error", prefix, e);
            throw new ShenyuException(e);
        }

    }

    /**
     * bytesOf string.
     * 
     * @param val val.
     * @return bytes val.
     */
    public ByteSequence bytesOf(final String val) {
        return ByteSequence.from(val, UTF_8);
    }

    /**
     * get node sub nodes.
     *
     * @param prefix    node prefix.
     * @param separator separator char
     * @return sub nodes
     * @throws ExecutionException   the exception
     * @throws InterruptedException the exception
     */
    public List<String> getChildrenKeys(final String prefix, final String separator)
            throws ExecutionException, InterruptedException {
        ByteSequence prefixByteSequence = bytesOf(prefix);
        GetOption getOption = GetOption.newBuilder()
                .withPrefix(prefixByteSequence)
                .withSortField(GetOption.SortTarget.KEY)
                .withSortOrder(GetOption.SortOrder.ASCEND)
                .build();

        List<KeyValue> keyValues = client.getKVClient()
                .get(prefixByteSequence, getOption)
                .get()
                .getKvs();

        return keyValues.stream()
                .map(e -> getSubNodeKeyName(prefix, e.getKey().toString(UTF_8), separator))
                .distinct()
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    /**
     * get keyPrefix map.
     *
     * @param prefix    key prefix.
     * @param separator separator char
     * @param map       prefix map
     * @return sub map
     */
    public List<String> getChildrenKeysByMap(final String prefix, final String separator,
            final Map<String, String> map) {

        return map.entrySet().stream()
                .filter(e -> e.getKey().contains(prefix))
                .map(e -> getSubNodeKeyName(prefix, e.getKey(), separator))
                .distinct()
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    private String getSubNodeKeyName(final String prefix, final String fullPath, final String separator) {
        if (prefix.length() > fullPath.length()) {
            return null;
        }
        String pathWithoutPrefix = fullPath.substring(prefix.length());
        return pathWithoutPrefix.contains(separator) ? pathWithoutPrefix.substring(1) : pathWithoutPrefix;
    }

    /**
     * subscribe data change.
     *
     * @param key           node name
     * @param updateHandler node value handler of update
     * @param deleteHandler node value handler of delete
     */
    public void watchDataChange(final String key,
            final BiConsumer<String, String> updateHandler,
            final Consumer<String> deleteHandler) {
        Watch.Listener listener = watch(updateHandler, deleteHandler);
        if (!watchCache.containsKey(key)) {
            Watch.Watcher watch = client.getWatchClient().watch(ByteSequence.from(key, UTF_8), listener);
            watchCache.put(key, watch);
        }
    }

    /**
     * subscribe sub node change.
     *
     * @param key           param node name.
     * @param updateHandler sub node handler of update
     * @param deleteHandler sub node delete of delete
     */
    public void watchChildChange(final String key,
            final BiConsumer<String, String> updateHandler,
            final Consumer<String> deleteHandler) {
        Watch.Listener listener = watch(updateHandler, deleteHandler);
        WatchOption option = WatchOption.newBuilder()
                .withPrefix(ByteSequence.from(key, UTF_8))
                .build();
        if (!watchChildCache.containsKey(key)) {
            Watch.Watcher watch = client.getWatchClient().watch(ByteSequence.from(key, UTF_8), option, listener);
            watchChildCache.put(key, watch);
        }
    }

    private Watch.Listener watch(final BiConsumer<String, String> updateHandler,
            final Consumer<String> deleteHandler) {
        return Watch.listener(response -> {
            for (WatchEvent event : response.getEvents()) {
                String path = event.getKeyValue().getKey().toString(UTF_8);
                String value = event.getKeyValue().getValue().toString(UTF_8);
                switch (event.getEventType()) {
                    case PUT:
                        Optional.ofNullable(updateHandler).ifPresent(handler -> handler.accept(path, value));
                        continue;
                    case DELETE:
                        Optional.ofNullable(deleteHandler).ifPresent(handler -> handler.accept(path));
                        continue;
                    default:
                }
            }
        }, throwable -> {
            LOG.error("etcd watch error {}", throwable.getMessage(), throwable);
            throw new ShenyuException(throwable);
        });
    }

    /**
     * cancel subscribe.
     *
     * @param key node name
     */
    public void watchClose(final String key) {
        if (watchCache.containsKey(key)) {
            watchCache.get(key).close();
            watchCache.remove(key);
        }
        if (watchChildCache.containsKey(key)) {
            watchChildCache.get(key).close();
            watchChildCache.remove(key);
        }
    }

    /**
     * check node exists.
     * 
     * @param key node name
     * @return bool
     */
    public Boolean exists(final String key) {
        try {
            GetOption option = GetOption.newBuilder()
                    .withPrefix(ByteSequence.from(key, StandardCharsets.UTF_8))
                    .build();
            List<KeyValue> keyValues = client.getKVClient().get(ByteSequence.from(key, StandardCharsets.UTF_8), option)
                    .get().getKvs();
            return !keyValues.isEmpty();
        } catch (Exception e) {
            LOG.error("check node exists error", e);
            throw new ShenyuException(e);
        }
    }

    /**
     * update value of node.
     * 
     * @param key   node name
     * @param value node value
     */
    public void put(final String key, final String value) {
        try {
            client.getKVClient().put(ByteSequence.from(key, StandardCharsets.UTF_8),
                    ByteSequence.from(value, StandardCharsets.UTF_8)).get();
        } catch (Exception e) {
            LOG.error("update value of node error.", e);
            throw new ShenyuException(e);
        }
    }

    /**
     * delete node.
     * 
     * @param key node name
     */
    public void delete(final String key) {
        client.getKVClient().delete(ByteSequence.from(key, StandardCharsets.UTF_8));
    }

    /**
     * delete node of recursive.
     * 
     * @param path parent node name
     */
    public void deleteEtcdPathRecursive(final String path) {
        DeleteOption option = DeleteOption.newBuilder()
                .withPrefix(ByteSequence.from(path, StandardCharsets.UTF_8))
                .build();
        try {
            client.getKVClient().delete(ByteSequence.from(path, StandardCharsets.UTF_8), option).get(10,
                    TimeUnit.SECONDS);
        } catch (Exception e) {
            LOG.error("delete node of recursive error.", e);
            throw new ShenyuException(e);
        }
    }

    /**
     * watchKeyChanges.
     *
     * @param key      key
     * @param listener listener
     * @return {@link Watch.Watcher}
     */
    public Watch.Watcher watchKeyChanges(final String key, final Watch.Listener listener) {
        WatchOption option = WatchOption.newBuilder().isPrefix(true).build();

        return client.getWatchClient().watch(bytesOf(key), option, listener);
    }

    /**
     * get keyResponse.
     * 
     * @param key       watch key.
     * @param getOption get option.
     * @return key response.
     */
    public GetResponse getRange(final String key, final GetOption getOption) {
        try {
            return this.client.getKVClient().get(bytesOf(key), getOption).get();
        } catch (ExecutionException | InterruptedException e) {
            LOG.error("etcd getRange key {} error", key, e);
            throw new ShenyuException(e);
        }
    }

    /**
     * put data as ephemeral.
     * 
     * @param key   key
     * @param value value
     */
    public void putEphemeral(final String key, final String value) {
        try {
            KV kvClient = client.getKVClient();
            kvClient.put(ByteSequence.from(key, UTF_8), ByteSequence.from(value, UTF_8),
                    PutOption.newBuilder().withLeaseId(globalLeaseId).build())
                    .get(timeout, TimeUnit.MILLISECONDS);
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            LOG.error("putEphemeral(key:{},value:{}) error.", key, value, e);
            throw new ShenyuException(e);
        }
    }

    private void initLease() {
        try {
            this.globalLeaseId = client.getLeaseClient().grant(ttl).get().getID();
            keepAlive();
        } catch (InterruptedException e) {
            LOG.error("initLease error.", e);
            Thread.currentThread().interrupt();
            throw new ShenyuException(e);
        } catch (ExecutionException e) {
            LOG.error("initLease error.", e);
            throw new ShenyuException(e);
        }
    }

    private void keepAlive() {
        if (!keepAliveRunning) {
            return;
        }
        retryCount.set(0);
        scheduleRetryInternal(0);
    }

    private void scheduleRetry() {
        int attempt = retryCount.incrementAndGet();
        scheduleRetryInternal(attempt);
    }

    private void scheduleRetryInternal(final int attempt) {
        if (!keepAliveRunning) {
            return;
        }

        if (Objects.nonNull(retryFuture) && !retryFuture.isDone()) {
            retryFuture.cancel(false);
        }

        long delaySeconds;
        if (attempt <= 0) {
            delaySeconds = 0;
        } else {
            long backoffFactor = 1L << Math.min(Math.max(attempt - 1, 0), 6);
            delaySeconds = Math.min(INITIAL_RETRY_DELAY_SECONDS * backoffFactor, MAX_RETRY_DELAY_SECONDS);
        }

        retryFuture = retryExecutor.schedule(() -> {
            if (!keepAliveRunning) {
                return;
            }

            currentObserver = new StreamObserver<>() {
                @Override
                public void onNext(final LeaseKeepAliveResponse leaseKeepAliveResponse) {
                    retryCount.set(0);
                }

                @Override
                public void onError(final Throwable throwable) {
                    LOG.error("keep alive error", throwable);
                    currentObserver = null;
                    scheduleRetry();
                }

                @Override
                public void onCompleted() {
                    LOG.warn("keep alive stream completed");
                    currentObserver = null;
                    scheduleRetry();
                }
            };

            try {
                client.getLeaseClient().keepAlive(globalLeaseId, currentObserver);
            } catch (Exception e) {
                LOG.error("Failed to start keep alive", e);
                currentObserver = null;
                scheduleRetry();
            }
        }, delaySeconds, TimeUnit.SECONDS);

        LOG.debug("Scheduled keep alive retry in {} seconds (attempt: {})", delaySeconds, attempt);
    }

    /**
     * Create a new {@link Builder} instance for building an {@link EtcdClient}.
     * 
     * @return a new {@link Builder} instance
     */
    public static Builder builder() {

        return Builder.builder();
    }

    public static final class Builder {

        /**
         * etcd client.
         */
        private Client client;

        private long ttl = 60;

        private long timeout = 5000;

        private Builder() {
        }

        private static Builder builder() {
            return new Builder();
        }

        public Builder client(final Client client) {
            this.client = client;
            return this;
        }

        public Builder ttl(final long ttl) {
            this.ttl = ttl;
            return this;
        }

        public Builder timeout(final long timeout) {
            this.timeout = timeout;
            return this;
        }

        public EtcdClient build() {
            return new EtcdClient(client, ttl, timeout);
        }
    }

}
