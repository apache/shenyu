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

package org.apache.shenyu.sync.data.etcd;

import io.etcd.jetcd.Client;
import io.etcd.jetcd.KeyValue;
import io.etcd.jetcd.Watch;
import io.etcd.jetcd.ByteSequence;
import io.etcd.jetcd.options.DeleteOption;
import io.etcd.jetcd.options.GetOption;
import io.etcd.jetcd.options.WatchOption;
import io.etcd.jetcd.watch.WatchEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * Etcd client of Bootstrap.
 */
public class EtcdClient {

    /**
     * logger.
     */
    private static final Logger LOG = LoggerFactory.getLogger(EtcdClient.class);

    private final Client client;

    private final ConcurrentHashMap<String, Watch.Watcher> watchCache = new ConcurrentHashMap<>();

    public EtcdClient(final Client client) {
        this.client = client;
    }

    /**
     * close client.
     */
    public void close() {
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
            keyValues = client.getKVClient().get(ByteSequence.from(key, StandardCharsets.UTF_8)).get().getKvs();
        } catch (InterruptedException | ExecutionException e) {
            LOG.error(e.getMessage(), e);
        }
        return keyValues.isEmpty() ? null : keyValues.iterator().next().getValue().toString(StandardCharsets.UTF_8);
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
    public List<String> getChildrenKeys(final String prefix, final String separator) throws ExecutionException, InterruptedException {
        ByteSequence prefixByteSequence = ByteSequence.from(prefix, StandardCharsets.UTF_8);
        GetOption getOption = GetOption.newBuilder().withPrefix(prefixByteSequence).withSortField(GetOption.SortTarget.KEY).withSortOrder(GetOption.SortOrder.ASCEND).build();
        List<KeyValue> keyValues = client.getKVClient().get(prefixByteSequence, getOption).get().getKvs();
        return keyValues.stream().map(e -> getSubNodeKeyName(prefix, e.getKey().toString(StandardCharsets.UTF_8), separator)).distinct().collect(Collectors.toList());
    }

    private String getSubNodeKeyName(final String prefix, final String fullPath, final String separator) {
        String pathWithoutPrefix = fullPath.substring(prefix.length());
        return pathWithoutPrefix.contains(separator) ? pathWithoutPrefix.substring(1) : pathWithoutPrefix;
    }

    /**
     * update value of node.
     *
     * @param key   node name
     * @param value node value
     * @throws ExecutionException   the exception
     * @throws InterruptedException the exception
     */
    public void put(final String key, final String value) throws ExecutionException, InterruptedException {
        client.getKVClient().put(ByteSequence.from(key, StandardCharsets.UTF_8), ByteSequence.from(value, StandardCharsets.UTF_8)).get();
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
     * @param key parent node name
     */
    public void deleteRecursive(final String key) {
        DeleteOption option = DeleteOption.newBuilder()
                .withPrefix(ByteSequence.from(key, StandardCharsets.UTF_8))
                .build();
        client.getKVClient().delete(ByteSequence.from(key, StandardCharsets.UTF_8), option);
    }

    /**
     * subscribe data change.
     *
     * @param key           node name
     * @param updateHandler node value handler of update
     * @param deleteHandler node value handler of delete
     */
    public void watchDataChange(final String key, final BiConsumer<String, String> updateHandler, final Consumer<String> deleteHandler) {
        Watch.Listener listener = watch(updateHandler, deleteHandler);
        Watch.Watcher watch = client.getWatchClient().watch(ByteSequence.from(key, StandardCharsets.UTF_8), listener);
        watchCache.put(key, watch);
    }

    /**
     * subscribe sub node change.
     *
     * @param key           param node name.
     * @param updateHandler sub node handler of update
     * @param deleteHandler sub node delete of delete
     */
    public void watchChildChange(final String key, final BiConsumer<String, String> updateHandler, final Consumer<String> deleteHandler) {
        Watch.Listener listener = watch(updateHandler, deleteHandler);
        WatchOption option = WatchOption.newBuilder()
                .withPrefix(ByteSequence.from(key, StandardCharsets.UTF_8))
                .build();
        Watch.Watcher watch = client.getWatchClient().watch(ByteSequence.from(key, StandardCharsets.UTF_8), option, listener);
        watchCache.put(key, watch);
    }

    private Watch.Listener watch(final BiConsumer<String, String> updateHandler, final Consumer<String> deleteHandler) {
        return Watch.listener(response -> {
            for (WatchEvent event : response.getEvents()) {
                String path = event.getKeyValue().getKey().toString(StandardCharsets.UTF_8);
                String value = event.getKeyValue().getValue().toString(StandardCharsets.UTF_8);
                switch (event.getEventType()) {
                    case PUT:
                        updateHandler.accept(path, value);
                        continue;
                    case DELETE:
                        deleteHandler.accept(path);
                        continue;
                    default:
                }
            }
        });
    }

    /**
     * cancel subscribe.
     * @param key node name
     */
    public void watchClose(final String key) {
        if (watchCache.containsKey(key)) {
            watchCache.get(key).close();
            watchCache.remove(key);
        }
    }
}
