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

package org.apache.shenyu.register.instance.etcd;


import io.etcd.jetcd.ByteSequence;
import io.etcd.jetcd.Client;
import io.etcd.jetcd.KV;
import io.etcd.jetcd.Watch;
import io.etcd.jetcd.kv.GetResponse;
import io.etcd.jetcd.lease.LeaseKeepAliveResponse;
import io.etcd.jetcd.options.GetOption;
import io.etcd.jetcd.options.PutOption;
import io.etcd.jetcd.options.WatchOption;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.common.exception.ShenyuException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.stream.Collectors;


/**
 * etcd client.
 */
public class EtcdClient {

    public static final Charset UTF_8 = StandardCharsets.UTF_8;

    private static final Logger LOGGER = LoggerFactory.getLogger(EtcdClient.class);

    private final ConcurrentHashMap<String, Watch.Watcher> watchCache = new ConcurrentHashMap<>();

    private final Client client;

    private final long ttl;

    private final long timeout;

    private long globalLeaseId;

    public EtcdClient(final String url, final long ttl, final long timeout) {
        this.client = Client.builder().endpoints(url.split(",")).build();
        this.ttl = ttl;
        this.timeout = timeout;
        initLease();
    }

    private void initLease() {
        try {
            this.globalLeaseId = client.getLeaseClient().grant(ttl).get().getID();
            client.getLeaseClient().keepAlive(globalLeaseId, new StreamObserver<LeaseKeepAliveResponse>() {
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
        }
    }

    /**
     * watch key changes.
     * @param key
     * @param listener
     * @return
     */
    public List<String> watchKeyChanges(final String key,final Watch.Listener listener) {
        WatchOption option = WatchOption.newBuilder()
                .isPrefix(true)
                .build();
        Watch.Watcher watch = client.getWatchClient().watch(ByteSequence.from(key, StandardCharsets.UTF_8), option, listener);
        watchCache.put(key, watch);
        return getKeysByPrefix(key);
    }

    /**
     * remove watchCache.
     * @param key
     */
    public void removeWatchCache(final String key){
        watchCache.remove(key);
    }


    /**
     * get keys by prefix.
     *
     * @param prefix key prefix.
     * @return key values
     */
    public List<String> getKeysByPrefix(final String prefix){
        GetOption getOption = GetOption.newBuilder()
                .isPrefix(true)
                .build();
        try {
            return getRange(prefix, getOption)
                    .getKvs()
                    .stream()
                    .map(x ->x.getValue().toString(StandardCharsets.UTF_8))
                    .collect(Collectors.toList());
        } catch (Exception e) {
            LOGGER.error("getKeysByPrefix prefix {} error {}",prefix,e);
            throw new ShenyuException(e);
        }

    }

    /**
     * get keyResponse.
     * @param key
     * @param getOption
     * @return
     * @throws Exception
     */
    public GetResponse getRange(final String key, final GetOption getOption) throws Exception {
        return this.client.getKVClient().get(bytesOf(key), getOption).get();
    }

    /**
     * bytesOf string.
     * @param val
     * @return
     */
    public  ByteSequence bytesOf(final String val) {
        return ByteSequence.from(val, UTF_8);
    }

    /**
     * close client.
     */
    public void close() {
        this.client.close();
    }

    /**
     * put data as ephemeral.
     * @param key key
     * @param value value
     */
    public void putEphemeral(final String key, final String value) {
        try {
            KV kvClient = client.getKVClient();
            kvClient.put(ByteSequence.from(key, UTF_8), ByteSequence.from(value, UTF_8),
                    PutOption.newBuilder().withLeaseId(globalLeaseId).build())
                    .get(timeout, TimeUnit.MILLISECONDS);
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            LOGGER.error("putEphemeral(key:{},value:{}) error.", key, value, e);
        }
    }
}
