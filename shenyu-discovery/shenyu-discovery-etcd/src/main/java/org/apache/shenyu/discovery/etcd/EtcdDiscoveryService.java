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
import io.etcd.jetcd.watch.WatchEvent;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.discovery.api.ShenyuDiscoveryService;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import io.grpc.stub.StreamObserver;

import org.apache.shenyu.discovery.api.listener.DiscoveryDataChangedEvent;
import org.apache.shenyu.spi.Join;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Objects;
import java.util.Properties;
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

    private final ConcurrentHashMap<String, Watch.Watcher> watchCache = new ConcurrentHashMap<>();

    private long leaseId = 0;

    private long ttl;

    private long timeout;

    @Override
    public void init(DiscoveryConfig config) {
        Properties props = config.getProps();
        this.timeout = Long.parseLong(props.getProperty("etcdTimeout", "3000"));
        this.ttl = Long.parseLong(props.getProperty("etcdTTL", "5"));
        if (this.etcdClient == null) {
            this.etcdClient = Client.builder().endpoints(config.getServerList().split(",")).build();
        }
        if (leaseId == 0) {
            initLease();
        }

    }

    private void initLease() {
        try (Lease lease = etcdClient.getLeaseClient()) {
            System.out.println("here");
            this.leaseId = lease.grant(ttl).get().getID();
            lease.keepAlive(leaseId, new StreamObserver<LeaseKeepAliveResponse>() {
                @Override
                public void onNext(final LeaseKeepAliveResponse leaseKeepAliveResponse) {
                }

                @Override
                public void onError(final Throwable throwable) {
                    LOGGER.error("keep alive error", throwable);
                }

                @Override
                public void onCompleted() {
                }
            });
        } catch (InterruptedException | ExecutionException e) {
            LOGGER.error("initLease error.", e);
            throw new ShenyuException(e);
        }
    }


    @Override
    public void watch(String key, DataChangedEventListener listener) {
        if (!this.watchCache.containsKey(key)) {
            try {
                Watch watch = etcdClient.getWatchClient();
                WatchOption option = WatchOption.newBuilder().isPrefix(true).build();
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
                                    dataChangedEvent = new DiscoveryDataChangedEvent(path, value, DiscoveryDataChangedEvent.Event.UPDATED);
                                    break;
                                case DELETE:
                                    dataChangedEvent = new DiscoveryDataChangedEvent(path, value, DiscoveryDataChangedEvent.Event.DELETED);
                                    break;
                                default:
                                    dataChangedEvent = new DiscoveryDataChangedEvent(path, value, DiscoveryDataChangedEvent.Event.IGNORED);
                            }
                            listener.onChange(dataChangedEvent);
                        }
                    }
                }));
                watchCache.put(key, watcher);
            } catch (Exception e) {
                throw new ShenyuException(e);
            }
        }
    }

    @Override
    public void unwatch(String key) {
        if (watchCache.containsKey(key)) {
            watchCache.remove(key).close();
        }
    }

    @Override
    public void register(String key, String value) {
        try {
            KV kvClient = etcdClient.getKVClient();
            PutOption putOption = PutOption.newBuilder().withLeaseId(leaseId).build();
            kvClient.put(bytesOf(key), bytesOf(value), putOption).get(timeout, TimeUnit.MILLISECONDS);
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            LOGGER.error("etcd client register success(key:{},value:{}) error.", key, value, e);
            throw new ShenyuException(e);
        }
    }

    @Override
    public List<String> getRegisterData(String key) {
        try {
            KV kvClient = etcdClient.getKVClient();
            GetOption option = GetOption.newBuilder().isPrefix(true).build();
            GetResponse response = kvClient.get(bytesOf(key), option).get();
            return response.getKvs().stream()
                    .filter(o -> !o.getKey().equals(ByteSequence.from(key, StandardCharsets.UTF_8)))
                    .map(kv -> kv.getValue().toString(StandardCharsets.UTF_8))
                    .collect(Collectors.toList());
        } catch (Exception e) {
            throw new ShenyuException(e);
        }
    }

    @Override
    public Boolean exists(String key) {
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
        if (Objects.nonNull(etcdClient)) {
            this.etcdClient.close();
        }
    }

    private ByteSequence bytesOf(final String val) {
        return ByteSequence.from(val, UTF_8);
    }
}
