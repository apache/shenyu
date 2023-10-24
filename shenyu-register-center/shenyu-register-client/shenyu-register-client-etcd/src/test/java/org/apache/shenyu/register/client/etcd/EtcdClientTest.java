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

package org.apache.shenyu.register.client.etcd;

import io.etcd.jetcd.Client;
import io.etcd.jetcd.ClientBuilder;
import io.etcd.jetcd.KV;
import io.etcd.jetcd.Lease;
import io.etcd.jetcd.kv.PutResponse;
import io.etcd.jetcd.lease.LeaseGrantResponse;
import io.etcd.jetcd.lease.LeaseKeepAliveResponse;

import io.grpc.stub.StreamObserver;

import org.apache.shenyu.common.exception.ShenyuException;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import org.mockito.MockedStatic;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyLong;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
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
            Assertions.assertDoesNotThrow(() -> new EtcdClient("url", 60L, 3000L));

            List<StreamObserver<LeaseKeepAliveResponse>> observerList = new ArrayList<>();
            doAnswer(invocation -> {
                observerList.add(invocation.getArgument(1));
                return lease;
            }).when(lease).keepAlive(anyLong(), any());
            Assertions.assertDoesNotThrow(() -> new EtcdClient("url", 60L, 3000L));
            final LeaseKeepAliveResponse leaseKeepAliveResponse = mock(LeaseKeepAliveResponse.class);
            observerList.forEach(streamObserver -> {
                streamObserver.onCompleted();
                streamObserver.onError(new ShenyuException("test"));
                streamObserver.onNext(leaseKeepAliveResponse);
            });

            doThrow(new InterruptedException("error")).when(completableFuture).get();
            Assertions.assertDoesNotThrow(() -> new EtcdClient("url", 60L, 3000L));
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }

    @Test
    public void closeTest() {
        try (MockedStatic<Client> clientMockedStatic = mockStatic(Client.class)) {
            this.mockEtcd(clientMockedStatic);
            final EtcdClient etcdClient = new EtcdClient("url", 60L, 3000L);
            etcdClient.close();
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }

    @Test
    public void putEphemeralTest() {
        try (MockedStatic<Client> clientMockedStatic = mockStatic(Client.class)) {
            final Client client = this.mockEtcd(clientMockedStatic);
            final KV mockKV = mock(KV.class);
            when(client.getKVClient()).thenReturn(mockKV);
            final CompletableFuture<PutResponse> completableFuture = mock(CompletableFuture.class);
            when(mockKV.put(any(), any(), any())).thenReturn(completableFuture);
            final PutResponse putResponse = mock(PutResponse.class);
            when(completableFuture.get(anyLong(), any(TimeUnit.class))).thenReturn(putResponse);
            final EtcdClient etcdClient = new EtcdClient("url", 60L, 3000L);
            etcdClient.putEphemeral("key", "value");

            doThrow(new InterruptedException("error")).when(completableFuture).get(anyLong(), any(TimeUnit.class));
            etcdClient.putEphemeral("key", "value");
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }

    private Client mockEtcd(final MockedStatic<Client> clientMockedStatic) throws InterruptedException, ExecutionException {
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
        return client;
    }
}
