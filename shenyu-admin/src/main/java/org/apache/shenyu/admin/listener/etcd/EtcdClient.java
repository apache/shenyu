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

package org.apache.shenyu.admin.listener.etcd;

import io.etcd.jetcd.ByteSequence;
import io.etcd.jetcd.Client;
import io.etcd.jetcd.KeyValue;
import io.etcd.jetcd.options.DeleteOption;
import io.etcd.jetcd.options.GetOption;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;

import java.nio.charset.StandardCharsets;
import java.util.List;

/**
 * Etcd client of Admin.
 */
@Slf4j
public class EtcdClient {

    private final Client client;

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
     * check node exists.
     * @param key node name
     * @return bool
     */
    @SneakyThrows
    public Boolean exists(final String key) {
        GetOption option = GetOption.newBuilder()
                .withPrefix(ByteSequence.from(key, StandardCharsets.UTF_8))
                .build();
        List<KeyValue> keyValues = client.getKVClient().get(ByteSequence.from(key, StandardCharsets.UTF_8), option).get().getKvs();
        return !keyValues.isEmpty();
    }

    /**
     * update value of node.
     * @param key node name
     * @param value node value
     */
    @SneakyThrows
    public void put(final String key, final String value) {
        client.getKVClient().put(ByteSequence.from(key, StandardCharsets.UTF_8), ByteSequence.from(value, StandardCharsets.UTF_8)).get();
    }

    /**
     * delete node.
     * @param key node name
     */
    public void delete(final String key) {
        client.getKVClient().delete(ByteSequence.from(key, StandardCharsets.UTF_8));
    }

    /**
     * delete node of recursive.
     * @param path parent node name
     */
    public void deleteEtcdPathRecursive(final String path) {
        DeleteOption option = DeleteOption.newBuilder()
                .withPrefix(ByteSequence.from(path, StandardCharsets.UTF_8))
                .build();
        client.getKVClient().delete(ByteSequence.from(path, StandardCharsets.UTF_8), option);
    }
}
