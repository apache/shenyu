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

import com.google.protobuf.ByteString;
import io.etcd.jetcd.ByteSequence;
import io.etcd.jetcd.Client;
import io.etcd.jetcd.KV;
import io.etcd.jetcd.KeyValue;
import io.etcd.jetcd.Watch;
import io.etcd.jetcd.kv.GetResponse;
import io.etcd.jetcd.options.WatchOption;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * test case for {@link EtcdSyncDataService}.
 */
@ExtendWith(MockitoExtension.class)
public class EtcdSyncDataServiceTest {
    
    private static final String MOCK_PLUGIN_PATH = "/shenyu/plugin/divide";

    private static final String MOCK_PLUGIN_NAME = "divide";

    private EtcdSyncDataService etcdSyncDataService;

    @InjectMocks
    private EtcdClient etcdClient;

    @Mock
    private Client client;

    @Mock
    private Watch.Watcher watcher;

    @BeforeEach
    public void setUp() {
        PluginData pluginData = PluginData.builder().name(MOCK_PLUGIN_NAME).enabled(Boolean.FALSE).build();
        KV kv = mock(KV.class);
        CompletableFuture<GetResponse> future = mock(CompletableFuture.class);
        GetResponse getResponse = mock(GetResponse.class);
        final List<KeyValue> keyValues = new ArrayList<>(2);
        KeyValue keyValue1 = mock(KeyValue.class);
        keyValues.add(keyValue1);
        final ByteString key1 = ByteString.copyFromUtf8(MOCK_PLUGIN_PATH);
        final ByteString value1 = ByteString.copyFromUtf8(GsonUtils.getInstance().toJson(pluginData));

        /**
         *  mock get method.
         */
        when(client.getKVClient()).thenReturn(kv);
        when(kv.get(any())).thenReturn(future);
        try {
            when(future.get()).thenReturn(getResponse);
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
        when(getResponse.getKvs()).thenReturn(keyValues);
        when(keyValue1.getValue()).thenReturn(ByteSequence.from(value1));
        /**
         * mock getChildrenKeys method.
         */
        when(kv.get(any(), any())).thenReturn(future);
        when(keyValue1.getKey()).thenReturn(ByteSequence.from(key1));
        /**
         * mock watchDataChange method.
         */
        Watch watch = mock(Watch.class);
        when(client.getWatchClient()).thenReturn(watch);
        when(watch.watch(any(ByteSequence.class), any(Watch.Listener.class))).thenReturn(watcher);
        /**
         * mock watchChildChange method.
         */
        when(watch.watch(any(ByteSequence.class), any(WatchOption.class), any(Watch.Listener.class))).thenReturn(watcher);
    }

    @Test
    public void testWatchPluginWhenInit() {
        final List<PluginData> subscribeList = new ArrayList<>(1);
        etcdSyncDataService = new EtcdSyncDataService(etcdClient, new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                subscribeList.add(pluginData);
            }
        }, Collections.emptyList(), Collections.emptyList());
        assertThat(subscribeList.size(), is(1));
        assertThat(subscribeList.get(0).getName(), is("divide"));
    }
}
