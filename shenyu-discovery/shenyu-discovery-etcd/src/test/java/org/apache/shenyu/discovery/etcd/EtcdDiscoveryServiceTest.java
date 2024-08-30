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
import io.etcd.jetcd.ClientBuilder;
import io.etcd.jetcd.KV;
import io.etcd.jetcd.KeyValue;
import io.etcd.jetcd.Lease;
import io.etcd.jetcd.Watch;
import io.etcd.jetcd.kv.GetResponse;
import io.etcd.jetcd.kv.PutResponse;
import io.etcd.jetcd.lease.LeaseGrantResponse;
import io.etcd.jetcd.lease.LeaseKeepAliveResponse;
import io.etcd.jetcd.options.GetOption;
import io.etcd.jetcd.options.PutOption;
import io.etcd.jetcd.options.WatchOption;
import io.etcd.jetcd.watch.WatchEvent;
import io.etcd.jetcd.watch.WatchResponse;
import io.grpc.stub.StreamObserver;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Properties;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


/**
 * The test for  {@link EtcdDiscoveryService } .
 */
public class EtcdDiscoveryServiceTest {

    private EtcdDiscoveryService etcdDiscoveryServiceUnderTest;

    private Client etcdClient;

    @BeforeEach
    void setUp() throws Exception {
        etcdDiscoveryServiceUnderTest = new EtcdDiscoveryService();
        etcdClient = mock(Client.class);
        setField(EtcdDiscoveryService.class, "etcdClient", etcdClient);
    }

    @AfterEach
    void downTest() {
        etcdDiscoveryServiceUnderTest.shutdown();
        verify(etcdClient).close();
    }

    private <T> void setField(final Class<T> clazz, final String fieldName, final Object value) throws NoSuchFieldException, IllegalAccessException {
        Field field = clazz.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(etcdDiscoveryServiceUnderTest, value);
        field.setAccessible(false);
    }

    @Test
    public void testInit() throws ExecutionException, InterruptedException, NoSuchFieldException, IllegalAccessException {
        setField(EtcdDiscoveryService.class, "etcdClient", null);
        final ClientBuilder builder = mock(ClientBuilder.class);
        when(builder.endpoints(anyString())).thenReturn(builder);
        when(builder.build()).thenReturn(etcdClient);
        final MockedStatic<Client> client = mockStatic(Client.class);
        client.when(Client::builder).thenReturn(builder);

        final Lease lease = mock(Lease.class);
        final CompletableFuture<LeaseGrantResponse> leaseGrantFuture = mock(CompletableFuture.class);
        when(lease.grant(anyLong())).thenReturn(leaseGrantFuture);
        final LeaseGrantResponse leaseGrantResponse = mock(LeaseGrantResponse.class);
        when(leaseGrantFuture.get()).thenReturn(leaseGrantResponse);
        when(etcdClient.getLeaseClient()).thenReturn(lease);

        ArrayList<StreamObserver<LeaseKeepAliveResponse>> observerList = new ArrayList<>();
        doAnswer(invocationOnMock -> {
            observerList.add(invocationOnMock.getArgument(1));
            return lease;
        }).when(lease).keepAlive(anyLong(), any());

        final Properties props = new Properties();
        props.put("etcdTimeout", "3000");
        props.put("etcdTTL", "5");
        final DiscoveryConfig discoveryConfig = new DiscoveryConfig();
        discoveryConfig.setProps(props);
        discoveryConfig.setServerList("localhost:2379");
        etcdDiscoveryServiceUnderTest.init(discoveryConfig);
        final LeaseKeepAliveResponse leaseKeepAliveResponse = mock(LeaseKeepAliveResponse.class);

        observerList.forEach(observer -> {
            observer.onNext(leaseKeepAliveResponse);
            observer.onError(new ShenyuException("test"));
            observer.onCompleted();
        });

        doThrow(new InterruptedException("test")).when(leaseGrantFuture).get();
        assertDoesNotThrow(() -> etcdDiscoveryServiceUnderTest.init(discoveryConfig));
    }

    @Test
    void testWatch() throws NoSuchFieldException, IllegalAccessException, ExecutionException, InterruptedException {
        final String eventKey = "event_key";
        final String eventValue = "event_value";
        final String key = "key";

        final Watch watch = mock(Watch.class);
        when(watch.watch(any(ByteSequence.class), any(WatchOption.class), any(Watch.Listener.class)))
                .thenReturn(mock(Watch.Watcher.class));
        final KV kvClient = mock(KV.class);
        when(etcdClient.getKVClient()).thenReturn(kvClient);
        final GetResponse getResponse = mock(GetResponse.class);
        final CompletableFuture<GetResponse> completableFuture = mock(CompletableFuture.class);
        when(completableFuture.get())
                .thenReturn(getResponse);
        when(kvClient.get(any(ByteSequence.class), any(GetOption.class))).thenReturn(completableFuture);
        when(etcdClient.getWatchClient()).thenReturn(watch);

        ArrayList<Watch.Listener> listeners = new ArrayList<>();
        doAnswer(invocationOnMock -> {
            listeners.add(invocationOnMock.getArgument(2));
            return mock(Watch.Watcher.class);
        }).when(watch).watch(any(ByteSequence.class), any(WatchOption.class), any(Watch.Listener.class));

        final KeyValue keyValue = mock(KeyValue.class);
        when(keyValue.getKey()).thenReturn(mock(ByteSequence.class));
        when(keyValue.getKey().toString(any())).thenReturn(eventKey);
        when(keyValue.getValue()).thenReturn(mock(ByteSequence.class));
        when(keyValue.getValue().toString(any())).thenReturn(eventValue);

        ArrayList<WatchEvent> events = new ArrayList<>();
        for (WatchEvent.EventType eventType : WatchEvent.EventType.values()) {
            WatchEvent event = mock(WatchEvent.class);
            when(event.getEventType()).thenReturn(eventType);
            when(event.getPrevKV()).thenReturn(keyValue);
            when(event.getKeyValue()).thenReturn(keyValue);
            events.add(event);
        }
        final WatchResponse watchResponse = mock(WatchResponse.class);
        when(watchResponse.getEvents()).thenReturn(events);
        final DataChangedEventListener mockListener = mock(DataChangedEventListener.class);
        etcdDiscoveryServiceUnderTest.watch(key, mockListener);
        listeners.forEach(listener -> listener.onNext(watchResponse));

        final Field cacheField = etcdDiscoveryServiceUnderTest.getClass().getDeclaredField("watchCache");
        cacheField.setAccessible(true);
        ConcurrentMap<String, Watch.Watcher> watchCache = (ConcurrentMap) cacheField.get(etcdDiscoveryServiceUnderTest);
        Assertions.assertNotNull(watchCache.get(key));
    }

    @Test
    public void testUnWatch() throws NoSuchFieldException, IllegalAccessException {
        final String key = "key";
        etcdDiscoveryServiceUnderTest.unwatch(key);
        final Field watchCacheField = etcdDiscoveryServiceUnderTest.getClass().getDeclaredField("watchCache");
        watchCacheField.setAccessible(true);
        ConcurrentMap<String, Watch.Watcher> o = (ConcurrentMap) watchCacheField.get(etcdDiscoveryServiceUnderTest);
        assertFalse(o.containsKey(key));
    }

    @Test
    void registerTest() throws ExecutionException, InterruptedException, TimeoutException {
        final String key = "key";
        final String value = "value";

        final KV kvClient = mock(KV.class);
        when(etcdClient.getKVClient()).thenReturn(kvClient);

        final PutResponse putResponse = mock(PutResponse.class);
        final CompletableFuture<PutResponse> completableFuture = mock(CompletableFuture.class);
        when(completableFuture.get(anyLong(), any(TimeUnit.class)))
                .thenReturn(putResponse);
        when(kvClient.put(any(ByteSequence.class), any(ByteSequence.class), any(PutOption.class)))
                .thenReturn(completableFuture);
        etcdDiscoveryServiceUnderTest.register(key, value);

        doThrow(new InterruptedException()).when(completableFuture).get(anyLong(), any(TimeUnit.class));
        assertThrows(ShenyuException.class, () -> etcdDiscoveryServiceUnderTest.register(key, value));
    }

    @Test
    void testGetRegisterData() throws InterruptedException, ExecutionException {
        final String key = "key";
        final KV kv = mock(KV.class);
        when(etcdClient.getKVClient()).thenReturn(kv);
        final GetResponse getResponse = mock(GetResponse.class);
        when(getResponse.getKvs()).thenReturn(Collections.emptyList());
        final CompletableFuture<GetResponse> completableFuture = mock(CompletableFuture.class);
        when(completableFuture.get()).thenReturn(getResponse);
        when(kv.get(any(ByteSequence.class), any(GetOption.class))).thenReturn(completableFuture);
        assertDoesNotThrow(() -> etcdDiscoveryServiceUnderTest.getRegisterData(key));
        doThrow(new InterruptedException("test")).when(completableFuture).get();
        assertThrows(ShenyuException.class, () -> etcdDiscoveryServiceUnderTest.getRegisterData(key));
    }

    @Test
    void testExists() throws ExecutionException, InterruptedException {
        final String key = "key";
        final KV kvClient = mock(KV.class);
        when(etcdClient.getKVClient()).thenReturn(kvClient);

        final GetResponse getResponse = mock(GetResponse.class);
        when(getResponse.getCount()).thenReturn(1L);
        final CompletableFuture<GetResponse> completableFuture = mock(CompletableFuture.class);
        when(completableFuture.get()).thenReturn(getResponse);
        when(kvClient.get(any(ByteSequence.class), any(GetOption.class))).thenReturn(completableFuture);

        final Boolean result = etcdDiscoveryServiceUnderTest.exists(key);
        assertTrue(result);
        doThrow(new InterruptedException("test")).when(completableFuture).get();
        assertThrows(ShenyuException.class, () -> etcdDiscoveryServiceUnderTest.exists(key));
    }

}
