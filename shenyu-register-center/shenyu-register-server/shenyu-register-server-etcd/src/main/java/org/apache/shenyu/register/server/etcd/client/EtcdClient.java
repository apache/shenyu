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

package org.apache.shenyu.register.server.etcd.client;

import com.coreos.jetcd.Client;
import com.coreos.jetcd.KV;
import com.coreos.jetcd.Lease;
import com.coreos.jetcd.Watch;
import com.coreos.jetcd.data.ByteSequence;
import com.coreos.jetcd.data.KeyValue;
import com.coreos.jetcd.kv.GetResponse;
import com.coreos.jetcd.lease.LeaseGrantResponse;
import com.coreos.jetcd.options.GetOption;
import com.coreos.jetcd.options.WatchOption;
import com.coreos.jetcd.watch.WatchResponse;
import com.google.common.util.concurrent.ThreadFactoryBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.List;
import java.util.Optional;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;
import java.util.function.Supplier;
import java.util.stream.Collectors;


/**
 * etcd client.
 */
public class EtcdClient {

    private static final Logger LOGGER = LoggerFactory.getLogger(EtcdClient.class);

    private static final int EPHEMERAL_LEASE = 60;

    private static final int DEFAULT_CORE_POOL_SIZE = 10;

    private static final int DEFAULT_QUEUE_SIZE = 1000;

    private final ThreadPoolExecutor defaultPoolExecutor;

    private final Client client;

    public EtcdClient(final String urls) {
        defaultPoolExecutor = new ThreadPoolExecutor(
                DEFAULT_CORE_POOL_SIZE, DEFAULT_CORE_POOL_SIZE * 2,
                0L, TimeUnit.NANOSECONDS,
                new ArrayBlockingQueue<>(DEFAULT_QUEUE_SIZE),
                new ThreadFactoryBuilder()
                        .setNameFormat("etcd register center watch-%d")
                        .setDaemon(true)
                        .build());

        this.client = Client.builder().endpoints(urls).build();

        try {
            initLease();
        } catch (ExecutionException | InterruptedException e) {
            LOGGER.error("initLease error.", e);
        }
    }

    private void initLease() throws ExecutionException, InterruptedException {
        Lease lease = client.getLeaseClient();
        LeaseGrantResponse response = lease.grant(EPHEMERAL_LEASE).get();
        long leaseId = response.getID();
        lease.keepAlive(leaseId);
    }

    /**
     * read data.
     * @param key key
     * @return string of data
     */
    public String read(final String key) {
        KV kv = client.getKVClient();
        ByteSequence storeKey = Optional.ofNullable(key).map(ByteSequence::fromString).orElse(null);
        GetResponse response = null;
        try {
            response = kv.get(storeKey).get();
        } catch (InterruptedException | ExecutionException e) {
            LOGGER.error("read(key:{}) error.", key, e);
        }
        if (response == null) {
            return null;
        }
        LOGGER.debug(String.valueOf(response.getHeader()));
        Node info = response.getKvs().stream().map(EtcdClient::kv2NodeInfo).findFirst().orElse(null);
        assert info != null;
        return info.getValue();
    }

    /**
     * get children of path.
     * @param path path
     * @return list of children
     */
    public List<String> getChildren(final String path) {
        try {
            return listKeys(path);
        } catch (ExecutionException | InterruptedException e) {
            LOGGER.error("getChildren(path:{}) error.", path, e);
        }
        return null;
    }

    private List<String> listKeys(final String prefix) throws ExecutionException, InterruptedException {
        KV kv = client.getKVClient();
        ByteSequence storePrefix = Optional.ofNullable(prefix).map(ByteSequence::fromString).orElse(null);
        GetOption option = GetOption.newBuilder().withKeysOnly(true).withPrefix(storePrefix).build();
        GetResponse response = kv.get(storePrefix, option).get();
        return response.getKvs().stream()
                .map(o -> o.getKey().toStringUtf8())
                .filter(k -> !k.equals(prefix))
                .collect(Collectors.toList());
    }

    /**
     * subscribe children change.
     * @param key key
     * @param handler event handler
     */
    public void subscribeChildChanges(final String key, final EtcdListenHandler handler) {
        defaultPoolExecutor.execute(() -> {
            final Stoppable stoppable = new Stoppable();
            try {
                watchChildren(key, stoppable, handler);
            } catch (Exception e) {
                stoppable.stop();
                LOGGER.warn(String.format("Watch exception of %s", "/s"), e);
            }
        });
    }

    private void watchChildren(final String key, final Supplier<Boolean> exitSignSupplier,
                               final BiConsumer<Event, Node> consumer) throws InterruptedException {
        ByteSequence storeKey = Optional.ofNullable(key).map(ByteSequence::fromString).orElse(null);

        try (Watch watch = client.getWatchClient();
             Watch.Watcher watcher = watch.watch(storeKey,
                     WatchOption.newBuilder().withPrefix(storeKey).build())) {
            while (!exitSignSupplier.get()) {
                WatchResponse response = watcher.listen();
                response.getEvents().forEach(watchEvent -> {
                    KeyValue keyValue = watchEvent.getKeyValue();
                    Node info = kv2NodeInfo(keyValue);
                    // 跳过根节点的变化
                    if (watchEvent.getKeyValue().getKey().equals(storeKey)) {
                        return;
                    }
                    Event event;
                    switch (watchEvent.getEventType()) {
                        case PUT:
                            event = Event.UPDATE;
                            break;
                        case DELETE:
                            event = Event.DELETE;
                            break;
                        default:
                            event = Event.UNRECOGNIZED;
                    }
                    consumer.accept(event, info);
                });
            }
        }
    }

    static Node kv2NodeInfo(final KeyValue kv) {
        String key = kv.getKey().toStringUtf8();
        String value = Optional.ofNullable(kv.getValue()).map(ByteSequence::toStringUtf8).orElse("");
        return new Node(key, value, kv.getCreateRevision(), kv.getModRevision(), kv.getVersion());
    }

    /**
     * close client.
     */
    public void close() {
        Optional.ofNullable(client).ifPresent(Client::close);
    }

    static class Stoppable implements Supplier<Boolean> {

        private boolean exit;

        @Override
        public Boolean get() {
            return exit;
        }

        void stop() {
            this.exit = true;
        }

    }
}
