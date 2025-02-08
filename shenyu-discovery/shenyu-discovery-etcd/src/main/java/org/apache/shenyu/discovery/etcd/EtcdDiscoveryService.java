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

package org.apache.shenyu.discovery.etcd;

import io.etcd.jetcd.ByteSequence;
import io.etcd.jetcd.Client;
import io.etcd.jetcd.Lease;
import io.etcd.jetcd.KV;
import io.etcd.jetcd.Watch;
import io.etcd.jetcd.kv.GetResponse;
import io.etcd.jetcd.lease.LeaseKeepAliveResponse;
import io.etcd.jetcd.options.GetOption;
import io.etcd.jetcd.options.PutOption;
import io.etcd.jetcd.options.WatchOption;
import io.etcd.jetcd.KeyValue;
import io.etcd.jetcd.watch.WatchEvent;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Properties;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;

import static java.nio.charset.StandardCharsets.UTF_8;

@Join
public class EtcdDiscoveryService implements ShenyuDiscoveryService {

    private static final Logger LOGGER = LoggerFactory.getLogger(EtcdDiscoveryService.class);

    private Client etcdClient;

    private final ConcurrentMap<String, Watch.Watcher> watchCache = new ConcurrentHashMap<>();

    private long leaseId;

    private long ttl;

    private long timeout;

    private volatile boolean isShuttingDown;

    @Override
    public void init(final DiscoveryConfig config) {
        try {
            if (Objects.nonNull(this.etcdClient)) {
                return;
            }
            Properties props = config.getProps();
            this.timeout = Long.parseLong(props.getProperty("etcdTimeout", "3000"));
            this.ttl = Long.parseLong(props.getProperty("etcdTTL", "5"));
            this.etcdClient = Client.builder().endpoints(config.getServerList().split(",")).build();
            LOGGER.info("Etcd Discovery Service initialize successfully");
            if (leaseId == 0) {
                initLease();
            }
        } catch (Exception e) {
            LOGGER.error("Error initializing Etcd Discovery Service", e);
            throw new ShenyuException(e);
        }
    }

    private void initLease() {
        Lease lease = null;
        try {
            lease = etcdClient.getLeaseClient();
            this.leaseId = lease.grant(ttl).get().getID();
            lease.keepAlive(leaseId, new StreamObserver<>() {
                @Override
                public void onNext(final LeaseKeepAliveResponse leaseKeepAliveResponse) {
                }
                
                @Override
                public void onError(final Throwable throwable) {
                    if (!isShuttingDown) {
                        LOGGER.error("etcd lease keep alive error", throwable);
                    }
                }
                
                @Override
                public void onCompleted() {
                }
            });
        } catch (InterruptedException | ExecutionException e) {
            LOGGER.error("initLease error.", e);
            if (Objects.nonNull(lease) && leaseId != 0) {
                try {
                    lease.revoke(leaseId).get();
                    leaseId = 0;
                } catch (InterruptedException | ExecutionException ex) {
                    LOGGER.error("Failed to revoke lease after initialization error.", ex);
                }
            }
            throw new ShenyuException(e);
        }
    }

    @Override
    public void watch(final String key, final DataChangedEventListener listener) {
        try {
            GetOption build = GetOption.newBuilder().isPrefix(true).build();
            CompletableFuture<GetResponse> getResponseCompletableFuture = etcdClient.getKVClient().get(bytesOf(key), build);
            GetResponse getResponse = getResponseCompletableFuture.get();
            List<KeyValue> kvs = getResponse.getKvs();
            for (KeyValue kv : kvs) {
                DiscoveryDataChangedEvent dataChangedEvent = new DiscoveryDataChangedEvent(kv.getKey().toString(), kv.getValue().toString(UTF_8), DiscoveryDataChangedEvent.Event.ADDED);
                listener.onChange(dataChangedEvent);
            }
            Watch watch = etcdClient.getWatchClient();
            WatchOption option = WatchOption.newBuilder().isPrefix(true).withPrevKV(true).build();
            Watch.Watcher watcher = watch.watch(bytesOf(key), option, Watch.listener(response -> {
                for (WatchEvent event : response.getEvents()) {
                    DiscoveryDataChangedEvent dataChangedEvent;
                    // ignore parent node
                    if (event.getKeyValue().getKey().equals(bytesOf(key))) {
                        return;
                    }
                    String value = event.getKeyValue().getValue().toString(StandardCharsets.UTF_8);
                    String path = event.getKeyValue().getKey().toString(StandardCharsets.UTF_8);

                    if (Objects.nonNull(event.getKeyValue()) && Objects.nonNull(value)) {
                        switch (event.getEventType()) {
                            case PUT:
                                dataChangedEvent = event.getKeyValue().getCreateRevision() == event.getKeyValue().getModRevision()
                                    ? new DiscoveryDataChangedEvent(path, value, DiscoveryDataChangedEvent.Event.ADDED)
                                    : new DiscoveryDataChangedEvent(path, value, DiscoveryDataChangedEvent.Event.UPDATED);
                                break;
                            case DELETE:
                                dataChangedEvent = new DiscoveryDataChangedEvent(path, event.getPrevKV().getValue().toString(StandardCharsets.UTF_8), DiscoveryDataChangedEvent.Event.DELETED);
                                break;
                            default:
                                dataChangedEvent = new DiscoveryDataChangedEvent(path, value, DiscoveryDataChangedEvent.Event.IGNORED);
                        }
                        listener.onChange(dataChangedEvent);
                    }
                }
            }));
            watchCache.put(key, watcher);
            LOGGER.info("Added etcd watcher for key: {}", key);
        } catch (Exception e) {
            LOGGER.error("etcd client watch key: {} error", key, e);
            throw new ShenyuException(e);
        }
    }

    @Override
    public void unwatch(final String key) {
        if (watchCache.containsKey(key)) {
            watchCache.remove(key).close();
            LOGGER.info("Unwatched etcd key: {}", key);
        }
    }

    @Override
    public void register(final String key, final String value) {
        try {
            KV kvClient = etcdClient.getKVClient();
            String uuid = UUIDUtils.getInstance().generateShortUuid();
            PutOption putOption = PutOption.newBuilder().withPrevKV().withLeaseId(leaseId).build();
            kvClient.put(bytesOf(key + "/" + uuid), bytesOf(value), putOption).get(timeout, TimeUnit.MILLISECONDS);
            LOGGER.info("etcd client key: {} with value: {}", key, value);
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            LOGGER.error("etcd client register (key:{},value:{}) error.", key, value, e);
            throw new ShenyuException(e);
        }
    }

    @Override
    public List<String> getRegisterData(final String key) {
        try {
            KV kvClient = etcdClient.getKVClient();
            GetOption option = GetOption.newBuilder().isPrefix(true).build();
            GetResponse response = kvClient.get(bytesOf(key), option).get();
            return response.getKvs().stream()
                    .filter(o -> !o.getKey().equals(ByteSequence.from(key, StandardCharsets.UTF_8)))
                    .map(kv -> kv.getValue().toString(StandardCharsets.UTF_8))
                    .collect(Collectors.toList());
        } catch (Exception e) {
            LOGGER.error("etcd client get registered data with key: {} error", key, e);
            throw new ShenyuException(e);
        }
    }

    @Override
    public Boolean exists(final String key) {
        try {
            KV kvClient = etcdClient.getKVClient();
            GetOption option = GetOption.newBuilder().isPrefix(true).withCountOnly(true).build();
            GetResponse response = kvClient.get(bytesOf(key), option).get();
            return response.getCount() > 0;
        } catch (InterruptedException | ExecutionException e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public void shutdown() {
        try {
            isShuttingDown = true;
            for (Map.Entry<String, Watch.Watcher> entry : watchCache.entrySet()) {
                Watch.Watcher watcher = entry.getValue();
                watcher.close();
            }
            watchCache.clear();
            if (Objects.nonNull(etcdClient)) {
                etcdClient.close();
                etcdClient = null;
                leaseId = 0;
            }
            LOGGER.info("Shutting down EtcdDiscoveryService");
        } catch (Exception e) {
            LOGGER.error("etcd client shutdown error", e);
            throw new ShenyuException(e);
        } finally {
            isShuttingDown = false;
        }
    }

    private ByteSequence bytesOf(final String val) {
        return ByteSequence.from(val, UTF_8);
    }

}
