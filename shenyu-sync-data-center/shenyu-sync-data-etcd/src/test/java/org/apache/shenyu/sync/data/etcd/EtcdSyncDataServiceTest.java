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
import org.apache.shenyu.common.constant.DefaultPathConstants;
import org.apache.shenyu.common.dto.AppAuthData;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.common.dto.PluginData;
import org.apache.shenyu.common.enums.ConfigGroupEnum;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.sync.data.api.AuthDataSubscriber;
import org.apache.shenyu.sync.data.api.MetaDataSubscriber;
import org.apache.shenyu.sync.data.api.PluginDataSubscriber;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

import static org.hamcrest.core.Is.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
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

    @Test
    public void deletePluginTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        final List<PluginData> subscribeList = new ArrayList<>(1);
        etcdSyncDataService = new EtcdSyncDataService(etcdClient, new PluginDataSubscriber() {
            @Override
            public void onSubscribe(final PluginData pluginData) {
                subscribeList.add(pluginData);
            }

            @Override
            public void unSubscribe(final PluginData pluginData) {
                final PluginData pluginDataDel = subscribeList.stream()
                        .filter(pluginDataSource -> pluginDataSource.getName().equals(pluginData.getName()))
                        .findFirst().orElse(null);
                subscribeList.remove(pluginDataDel);
            }
        }, Collections.emptyList(), Collections.emptyList());
        subscribeList.clear();
        final Method deletePlugin = EtcdSyncDataService.class.getDeclaredMethod("deletePlugin", String.class);
        final Method cachePluginData = EtcdSyncDataService.class.getDeclaredMethod("cachePluginData", String.class);
        deletePlugin.setAccessible(true);
        cachePluginData.setAccessible(true);
        final PluginData pluginData = new PluginData();
        pluginData.setName("pluginName");
        cachePluginData.invoke(etcdSyncDataService, GsonUtils.getInstance().toJson(pluginData));
        assertEquals(subscribeList.get(0).getName(), "pluginName");
        deletePlugin.invoke(etcdSyncDataService, "pluginName");
        assertTrue(subscribeList.isEmpty());
    }

    @Test
    public void deleteMetaDataTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        final List<MetaData> subscribeList = new ArrayList<>(1);
        etcdSyncDataService = new EtcdSyncDataService(etcdClient, mock(PluginDataSubscriber.class),
                Collections.singletonList(new MetaDataSubscriber() {
                    @Override
                    public void onSubscribe(final MetaData metaData) {
                        subscribeList.add(metaData);
                    }

                    @Override
                    public void unSubscribe(final MetaData metaData) {
                        final MetaData metaDataDel = subscribeList.stream()
                                .filter(metaDataSource -> metaDataSource.getId().equals(metaData.getId()))
                                .findFirst().orElse(null);
                        subscribeList.remove(metaDataDel);
                    }
                }), Collections.emptyList());
        subscribeList.clear();
        final Method cacheMetaData = EtcdSyncDataService.class.getDeclaredMethod("cacheMetaData", String.class);
        final Method deleteMetaData = EtcdSyncDataService.class.getDeclaredMethod("unCacheMetaData", MetaData.class);
        cacheMetaData.setAccessible(true);
        deleteMetaData.setAccessible(true);
        MetaData metaData = new MetaData();
        metaData.setId("metaDataId");
        cacheMetaData.invoke(etcdSyncDataService, GsonUtils.getInstance().toJson(metaData));
        assertEquals(subscribeList.get(0).getId(), metaData.getId());
        deleteMetaData.invoke(etcdSyncDataService, metaData);
        assertTrue(subscribeList.isEmpty());
    }

    @Test
    public void authDataTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        final List<AppAuthData> subscribeList = new ArrayList<>(1);
        etcdSyncDataService = new EtcdSyncDataService(etcdClient, mock(PluginDataSubscriber.class), Collections.emptyList(),
                Collections.singletonList(new AuthDataSubscriber() {
                    @Override
                    public void onSubscribe(final AppAuthData metaData) {
                        subscribeList.add(metaData);
                    }

                    @Override
                    public void unSubscribe(final AppAuthData appAuthData) {
                        final AppAuthData appAuthDataOld = subscribeList.stream()
                                .filter(appAuthDataSource -> appAuthDataSource.getAppKey().equals(appAuthData.getAppKey()))
                                .findFirst().orElse(null);
                        subscribeList.remove(appAuthDataOld);
                    }
                }));
        subscribeList.clear();
        final Method cacheAuthData = EtcdSyncDataService.class.getDeclaredMethod("cacheAuthData", String.class);
        final Method unCacheAuthData = EtcdSyncDataService.class.getDeclaredMethod("unCacheAuthData", String.class);
        cacheAuthData.setAccessible(true);
        unCacheAuthData.setAccessible(true);
        AppAuthData appAuthData = new AppAuthData();
        appAuthData.setAppKey("appKeyValue");
        cacheAuthData.invoke(etcdSyncDataService, GsonUtils.getInstance().toJson(appAuthData));
        assertEquals(subscribeList.get(0).getAppKey(), appAuthData.getAppKey());
        unCacheAuthData.invoke(etcdSyncDataService, String.join("/", DefaultPathConstants.APP_AUTH_PARENT, appAuthData.getAppKey()));
        assertTrue(subscribeList.isEmpty());
    }

    @Test
    public void closeTest() {
        etcdSyncDataService = new EtcdSyncDataService(etcdClient, mock(PluginDataSubscriber.class), Collections.emptyList(),
                Collections.emptyList());
        assertDoesNotThrow(() -> etcdSyncDataService.close());
    }

    @Test
    public void ruleTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        this.commonTest(ConfigGroupEnum.RULE, "/shenyu/rule/divide/selectorId-ruleId");
    }

    @Test
    public void selectorTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        this.commonTest(ConfigGroupEnum.SELECTOR, "/shenyu/selector/divide/selectorId");
    }

    @Test
    public void metaDataTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        this.commonTest(ConfigGroupEnum.META_DATA, "/shenyu/metaData/divide/metaDataId");
    }

    @Test
    public void appAuthTest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        this.commonTest(ConfigGroupEnum.APP_AUTH, "/shenyu/appAuth/divide/appAuthId");
    }

    /**
    * commonTest.
    * @param configGroupEnum configGroupEnum
    * @param key key
    * @throws NoSuchMethodException NoSuchMethodException
    * @throws InvocationTargetException InvocationTargetException
    * @throws IllegalAccessException IllegalAccessException
    * @throws NoSuchFieldException NoSuchFieldException
    */
    public void commonTest(final ConfigGroupEnum configGroupEnum, final String key) throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        final EtcdClient mockEtcdClient = mock(EtcdClient.class);
        etcdSyncDataService = new EtcdSyncDataService(etcdClient,
                mock(PluginDataSubscriber.class),
                Collections.emptyList(),
                Collections.emptyList());
        final Field etcdClient = EtcdSyncDataService.class.getDeclaredField("etcdClient");
        etcdClient.setAccessible(true);
        etcdClient.set(etcdSyncDataService, mockEtcdClient);
        final Method subscribeChildChanges = EtcdSyncDataService.class.getDeclaredMethod("subscribeChildChanges", ConfigGroupEnum.class, String.class);
        subscribeChildChanges.setAccessible(true);

        final List<BiConsumer<String, String>> biConsumers = new ArrayList<>(4);
        doAnswer(invocation -> {
            biConsumers.add(invocation.getArgument(1));
            return true;
        }).when(mockEtcdClient).watchChildChange(anyString(), any(), any());

        final List<BiConsumer<String, String>> dataBiConsumers = new ArrayList<>(4);
        final List<Consumer<String>> dataConsumers = new ArrayList<>(4);
        doAnswer(invocation -> {
            dataBiConsumers.add(invocation.getArgument(1));
            dataConsumers.add(invocation.getArgument(2));
            return true;
        }).when(mockEtcdClient).watchDataChange(anyString(), any(), any());

        subscribeChildChanges.invoke(etcdSyncDataService, configGroupEnum, "groupParentPath");
        // hit default
        assertThrows(InvocationTargetException.class, () -> subscribeChildChanges.invoke(etcdSyncDataService, ConfigGroupEnum.PLUGIN, "groupParentPath"));

        biConsumers.forEach(biConsumer -> {
            biConsumer.accept("updatePath", "updateValue");
        });
        dataConsumers.forEach(consumer -> {
            consumer.accept(key);
        });
        dataBiConsumers.forEach(biConsumer -> {
            biConsumer.accept("updatePath", "{}");
        });
        mockEtcdClient.close();
    }
}
