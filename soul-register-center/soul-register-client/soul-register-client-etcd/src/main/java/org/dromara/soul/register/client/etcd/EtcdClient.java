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

package org.dromara.soul.register.client.etcd;

import io.etcd.jetcd.ByteSequence;
import io.etcd.jetcd.Client;
import io.etcd.jetcd.KeyValue;
import io.etcd.jetcd.kv.GetResponse;
import io.etcd.jetcd.kv.PutResponse;
import io.etcd.jetcd.lease.LeaseKeepAliveResponse;
import io.etcd.jetcd.options.GetOption;
import io.etcd.jetcd.options.PutOption;
import io.grpc.stub.StreamObserver;
import lombok.extern.slf4j.Slf4j;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * etcd client.
 *
 * @author lw1243925457
 */
@Slf4j
public class EtcdClient {

    public static final Charset UTF_8 = StandardCharsets.UTF_8;

    private final Client client;

    private final long ttl;

    private final long timeout;

    private long globalLeaseId;

    public EtcdClient(final String url, final long ttl, final long timeout) {
        this.client = Client.builder().endpoints(url).build();
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
                    log.info("keep alive error");
                    throwable.printStackTrace();
                }

                @Override
                public void onCompleted() {

                }
            });
        } catch (InterruptedException | ExecutionException e) {
            e.printStackTrace();
        }
    }

    /**
     * close client.
     */
    public void close() {
        this.client.close();
    }

    /**
     * put data as persistent.
     * @param key key
     * @param value value
     */
    public void put(final String key, final String value) {
        log.info("\nput: {} == {}\n", key, value);
        if (key == null || value == null) {
            return;
        }
        CompletableFuture<PutResponse> putFuture =
                this.client.getKVClient().put(ByteSequence.from(key, UTF_8), ByteSequence.from(value, UTF_8));
        try {
            putFuture.get(timeout, TimeUnit.MILLISECONDS);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * put data as ephemeral.
     * @param key key
     * @param value value
     */
    public void putEphemeral(final String key, final String value) {
        try {
            client.getKVClient()
                    .put(ByteSequence.from(key, UTF_8),
                            ByteSequence.from(String.valueOf(value), UTF_8),
                            PutOption.newBuilder().withLeaseId(globalLeaseId).build())
                    .get(timeout, TimeUnit.MILLISECONDS);
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            e.printStackTrace();
        }
    }

    /**
     * read data of path.
     * @param key key
     * @return data
     */
    public String getKVValue(final String key) {
        if (null == key) {
            return null;
        }

        CompletableFuture<GetResponse> responseFuture = this.client.getKVClient().get(ByteSequence.from(key, UTF_8));

        try {
            List<KeyValue> result = responseFuture.get(timeout, TimeUnit.MILLISECONDS).getKvs();
            if (!result.isEmpty()) {
                return result.get(0).getValue().toString(UTF_8);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        return null;
    }

    /**
     * check path is exist.
     * @param path path
     * @return boolean
     */
    public boolean checkExists(final String path) {
        try {
            return client.getKVClient()
                    .get(ByteSequence.from(path, UTF_8), GetOption.newBuilder().withCountOnly(true).build())
                    .get(timeout, TimeUnit.MILLISECONDS)
                    .getCount() > 0;
        } catch (InterruptedException | ExecutionException | TimeoutException e) {
            e.printStackTrace();
        }
        return false;
    }
}
