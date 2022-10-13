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
import io.etcd.jetcd.KeyValue;
import io.etcd.jetcd.Watch;
import io.etcd.jetcd.kv.GetResponse;
import io.etcd.jetcd.lease.LeaseKeepAliveResponse;
import io.etcd.jetcd.options.GetOption;
import io.etcd.jetcd.options.PutOption;
import io.etcd.jetcd.options.WatchOption;
import io.etcd.jetcd.watch.WatchEvent;
import io.grpc.stub.StreamObserver;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * etcd client.
 */
public class EtcdClient {

    public static final Charset UTF_8 = StandardCharsets.UTF_8;

    private static final Logger LOGGER = LoggerFactory.getLogger(EtcdClient.class);

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
     * close client.
     */
    public void close() {
        this.client.close();
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
            LOGGER.error("putEphemeral(key:{},value:{}) error.", key, value, e);
        }
    }

    public InstanceRegisterDTO getEphemeral(final String key) throws ExecutionException, InterruptedException {
        String prefix = "/shenyu/register/instance";
        CompletableFuture<GetResponse> getResponseCompletableFuture =
                client.getKVClient().get(ByteSequence.from(prefix, UTF_8), GetOption.newBuilder().withPrefix(ByteSequence.from(prefix, UTF_8)).build());

        List<KeyValue> keyValueList = getResponseCompletableFuture.get().getKvs();
        KV kvClient = client.getKVClient();
        String nodeDataJson = String.valueOf(kvClient.get(ByteSequence.from(key, UTF_8)));
        return GsonUtils.getInstance().fromJson(nodeDataJson, InstanceRegisterDTO.class);
    }

    public List<InstanceRegisterDTO> watchService(final String prefixAddress) {
        List<InstanceRegisterDTO> instanceRegisterDTOS = new ArrayList<>();
        CompletableFuture<GetResponse> getResponseCompletableFuture =
                client.getKVClient().get(ByteSequence.from(prefixAddress, UTF_8),
                        GetOption.newBuilder().withPrefix(ByteSequence.from(prefixAddress, UTF_8)).build());

        try {
            List<KeyValue> keyValueList = getResponseCompletableFuture.get().getKvs();
            for (KeyValue keyValue : keyValueList) {
                instanceRegisterDTOS.add(GsonUtils.getInstance().fromJson(keyValue.getValue().toString(UTF_8), InstanceRegisterDTO.class));
            }
        } catch (Exception e) {
            LOGGER.error("watchService error", e);
        }
        return instanceRegisterDTOS;
    }

    public void watch(final String prefixAddress, final List<InstanceRegisterDTO> instanceRegisterDTOS) {
        WatchOption watchOption = WatchOption.newBuilder().withPrefix(ByteSequence.from(prefixAddress, UTF_8)).build();
        Watch.Listener listener = Watch.listener(watchResponse -> {
            watchResponse.getEvents().forEach(watchEvent -> {
                WatchEvent.EventType eventType = watchEvent.getEventType();
                switch (eventType) {
                    case PUT:
                        instanceRegisterDTOS.add(GsonUtils.getInstance().fromJson(watchEvent.getKeyValue().getValue().toString(), InstanceRegisterDTO.class));
                        break;
                    case DELETE:
                        instanceRegisterDTOS.remove(GsonUtils.getInstance().fromJson(watchEvent.getKeyValue().getValue().toString(), InstanceRegisterDTO.class));
                        break;
                    default:
                        break;
                }
            });
        });
        client.getWatchClient().watch(ByteSequence.from(prefixAddress, UTF_8), watchOption, listener);

    }
}
