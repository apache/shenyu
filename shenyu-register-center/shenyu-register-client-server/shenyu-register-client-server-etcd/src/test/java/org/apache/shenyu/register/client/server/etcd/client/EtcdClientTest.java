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

package org.apache.shenyu.register.client.server.etcd.client;

import com.coreos.jetcd.Client;
import com.coreos.jetcd.ClientBuilder;
import com.coreos.jetcd.KV;
import com.coreos.jetcd.Lease;
import com.coreos.jetcd.Watch;
import com.coreos.jetcd.data.ByteSequence;
import com.coreos.jetcd.data.KeyValue;
import com.coreos.jetcd.kv.GetResponse;
import com.coreos.jetcd.lease.LeaseGrantResponse;
import com.coreos.jetcd.options.GetOption;
import com.coreos.jetcd.options.WatchOption;
import com.coreos.jetcd.watch.WatchEvent;
import com.coreos.jetcd.watch.WatchResponse;
import org.apache.shenyu.common.exception.ShenyuException;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

/**
 * test for EtcdClient.
 */
public class EtcdClientTest {

    @Test
    public void etcdClientTest() {
        try (MockedStatic<Client> clientMockedStatic = mockStatic(Client.class)) {
            final ClientBuilder clientBuilder = mock(ClientBuilder.class);
            clientMockedStatic.when(Client::builder).thenReturn(clientBuilder);
            when(clientBuilder.endpoints(anyString())).thenReturn(clientBuilder);
            final Client client = mock(Client.class);
            when(clientBuilder.endpoints(anyString()).build()).thenReturn(client);
            final Lease lease = mock(Lease.class);
            when(client.getLeaseClient()).thenReturn(lease);
            final CompletableFuture<LeaseGrantResponse> completableFuture = mock(CompletableFuture.class);
            final LeaseGrantResponse leaseGrantResponse = mock(LeaseGrantResponse.class);

            when(client.getLeaseClient().grant(anyLong())).thenReturn(completableFuture);
            when(completableFuture.get()).thenReturn(leaseGrantResponse);
            Assertions.assertDoesNotThrow(() -> new EtcdClient("urls"));

            doThrow(new InterruptedException("error")).when(completableFuture).get();
            Assertions.assertDoesNotThrow(() -> new EtcdClient("urls"));
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }

    @Test
    public void readTest() {
        try (MockedStatic<Client> clientMockedStatic = mockStatic(Client.class)) {
            final ClientBuilder clientBuilder = mock(ClientBuilder.class);
            clientMockedStatic.when(Client::builder).thenReturn(clientBuilder);
            when(clientBuilder.endpoints(anyString())).thenReturn(clientBuilder);
            final Client client = mock(Client.class);
            when(clientBuilder.endpoints(anyString()).build()).thenReturn(client);
            final Lease lease = mock(Lease.class);
            when(client.getLeaseClient()).thenReturn(lease);
            final CompletableFuture<LeaseGrantResponse> completableFuture = mock(CompletableFuture.class);
            final LeaseGrantResponse leaseGrantResponse = mock(LeaseGrantResponse.class);

            when(client.getLeaseClient().grant(anyLong())).thenReturn(completableFuture);
            when(completableFuture.get()).thenReturn(leaseGrantResponse);
            final KV mockKV = mock(KV.class);
            when(client.getKVClient()).thenReturn(mockKV);
            final EtcdClient etcdClient = new EtcdClient("urls");

            final CompletableFuture<GetResponse> future = mock(CompletableFuture.class);

            when(mockKV.get(any(ByteSequence.class))).thenReturn(future);
            when(future.get()).thenReturn(null);
            Assertions.assertDoesNotThrow(() -> etcdClient.read("key"));
            GetResponse getResponse = mock(GetResponse.class);
            when(future.get()).thenReturn(getResponse);
            List<KeyValue> keyValues = new ArrayList<>();
            final KeyValue keyValue = mock(KeyValue.class);
            when(keyValue.getKey()).thenReturn(ByteSequence.fromString("key"));
            keyValues.add(keyValue);
            Assertions.assertThrows(AssertionError.class, () -> etcdClient.read("key"));
            when(getResponse.getKvs()).thenReturn(keyValues);
            Assertions.assertDoesNotThrow(() -> etcdClient.read("key"));

            doThrow(new InterruptedException("error")).when(future).get();
            Assertions.assertDoesNotThrow(() -> etcdClient.read("key"));

            Assertions.assertDoesNotThrow(etcdClient::close);
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }

    @Test
    public void getChildrenTest() {
        try (MockedStatic<Client> clientMockedStatic = mockStatic(Client.class)) {
            final ClientBuilder clientBuilder = mock(ClientBuilder.class);
            clientMockedStatic.when(Client::builder).thenReturn(clientBuilder);
            when(clientBuilder.endpoints(anyString())).thenReturn(clientBuilder);
            final Client client = mock(Client.class);
            when(clientBuilder.endpoints(anyString()).build()).thenReturn(client);
            final Lease lease = mock(Lease.class);
            when(client.getLeaseClient()).thenReturn(lease);
            final CompletableFuture<LeaseGrantResponse> completableFuture = mock(CompletableFuture.class);
            final LeaseGrantResponse leaseGrantResponse = mock(LeaseGrantResponse.class);
            when(client.getLeaseClient().grant(anyLong())).thenReturn(completableFuture);
            when(completableFuture.get()).thenReturn(leaseGrantResponse);
            final KV mockKV = mock(KV.class);
            when(client.getKVClient()).thenReturn(mockKV);
            final EtcdClient etcdClient = new EtcdClient("urls");
            final Method getChildren = EtcdClient.class.getDeclaredMethod("getChildren", String.class);
            getChildren.setAccessible(true);

            final CompletableFuture<GetResponse> future = mock(CompletableFuture.class);
            GetResponse getResponse = mock(GetResponse.class);
            when(mockKV.get(any(ByteSequence.class), any(GetOption.class))).thenReturn(future);
            when(future.get()).thenReturn(getResponse);
            List<KeyValue> keyValues = new ArrayList<>();
            final KeyValue keyValue = mock(KeyValue.class);
            when(keyValue.getKey()).thenReturn(ByteSequence.fromString("key"));
            keyValues.add(keyValue);
            when(getResponse.getKvs()).thenReturn(keyValues);
            getChildren.invoke(etcdClient, "path");

            when(keyValue.getKey()).thenReturn(ByteSequence.fromString("path"));
            getChildren.invoke(etcdClient, "path");

            doThrow(new InterruptedException("error")).when(future).get();
            Assertions.assertDoesNotThrow(() -> getChildren.invoke(etcdClient, "path"));
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }

    @Test
    public void subscribeChildChangesTest() {
        try (MockedStatic<Client> clientMockedStatic = mockStatic(Client.class)) {
            final ClientBuilder clientBuilder = mock(ClientBuilder.class);
            clientMockedStatic.when(Client::builder).thenReturn(clientBuilder);
            when(clientBuilder.endpoints(anyString())).thenReturn(clientBuilder);
            final Client client = mock(Client.class);
            when(clientBuilder.endpoints(anyString()).build()).thenReturn(client);
            final Lease lease = mock(Lease.class);
            when(client.getLeaseClient()).thenReturn(lease);
            final CompletableFuture<LeaseGrantResponse> completableFuture = mock(CompletableFuture.class);
            final LeaseGrantResponse leaseGrantResponse = mock(LeaseGrantResponse.class);
            when(client.getLeaseClient().grant(anyLong())).thenReturn(completableFuture);
            when(completableFuture.get()).thenReturn(leaseGrantResponse);
            final KV mockKV = mock(KV.class);
            when(client.getKVClient()).thenReturn(mockKV);
            final EtcdClient etcdClient = new EtcdClient("urls");
            final Method subscribeChildChanges = EtcdClient.class.getDeclaredMethod("subscribeChildChanges", String.class, EtcdListenHandler.class);
            subscribeChildChanges.setAccessible(true);
            final EtcdListenHandler etcdListenHandler = mock(EtcdListenHandler.class);
            subscribeChildChanges.invoke(etcdClient, "path", etcdListenHandler);

            final Watch watch = mock(Watch.class);
            final Watch.Watcher watcher = mock(Watch.Watcher.class);
            when(watch.watch(any(ByteSequence.class), any(WatchOption.class))).thenReturn(watcher);
            when(client.getWatchClient()).thenReturn(watch);
            final WatchResponse watchResponse = mock(WatchResponse.class);
            when(watcher.listen()).thenReturn(watchResponse);
            List<WatchEvent> watchEvents = new ArrayList<>();
            final WatchEvent watchEvent = mock(WatchEvent.class);
            watchEvents.add(watchEvent);
            when(watchResponse.getEvents()).thenReturn(watchEvents);
            final KeyValue keyValue = mock(KeyValue.class);
            when(keyValue.getKey()).thenReturn(ByteSequence.fromString("path"));
            when(keyValue.getValue()).thenReturn(ByteSequence.fromString("value"));
            when(watchEvent.getKeyValue()).thenReturn(keyValue);
            subscribeChildChanges.invoke(etcdClient, "path", etcdListenHandler);
            when(keyValue.getKey()).thenReturn(ByteSequence.fromString("key"));
            // hit EventType.PUT
            when(watchEvent.getEventType()).thenReturn(WatchEvent.EventType.PUT);
            subscribeChildChanges.invoke(etcdClient, "path", etcdListenHandler);
            // hit EventType.DELETE
            when(watchEvent.getEventType()).thenReturn(WatchEvent.EventType.DELETE);
            subscribeChildChanges.invoke(etcdClient, "path", etcdListenHandler);
            // hit EventType.UNRECOGNIZED
            when(watchEvent.getEventType()).thenReturn(WatchEvent.EventType.UNRECOGNIZED);
            subscribeChildChanges.invoke(etcdClient, "path", etcdListenHandler);

            Thread.sleep(20);
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }

}
