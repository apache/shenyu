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

package org.apache.shenyu.register.server.etcd;

import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.server.api.ShenyuServerRegisterPublisher;
import org.apache.shenyu.register.server.etcd.client.EtcdClient;
import org.apache.shenyu.register.server.etcd.client.EtcdListenHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.times;

/**
 * test for EtcdServerRegisterRepository.
 */
public class EtcdServerRegisterRepositoryTest {

    private EtcdServerRegisterRepository repository;

    private ShenyuServerRegisterPublisher publisher;

    private EtcdListenHandler watchHandler;

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException {
        this.publisher = mockPublish();
        this.repository = new EtcdServerRegisterRepository();
        Class<? extends EtcdServerRegisterRepository> clazz = this.repository.getClass();

        String fieldClientString = "client";
        Field fieldClient = clazz.getDeclaredField(fieldClientString);
        fieldClient.setAccessible(true);
        fieldClient.set(repository, mockEtcdClient());

        String fieldPublisherString = "publisher";
        Field fieldPublisher = clazz.getDeclaredField(fieldPublisherString);
        fieldPublisher.setAccessible(true);
        fieldPublisher.set(repository, publisher);
    }

    @Test
    public void testSubscribe() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        Class<? extends EtcdServerRegisterRepository> clazz = this.repository.getClass();
        String methodString = "subscribeMetaData";
        Method method = clazz.getDeclaredMethod(methodString, String.class);
        method.setAccessible(true);
        method.invoke(repository, "http");
        verify(publisher, times(2)).publish(any());

        String data = GsonUtils.getInstance().toJson(MetaDataRegisterDTO.builder().build());
        watchHandler.updateHandler("/path", data);
        verify(publisher, times(3)).publish(any());
    }

    private ShenyuServerRegisterPublisher mockPublish() {
        ShenyuServerRegisterPublisher publisher = mock(ShenyuServerRegisterPublisher.class);
        doNothing().when(publisher).publish(any());
        return publisher;
    }

    private EtcdClient mockEtcdClient() {
        MetaDataRegisterDTO data = MetaDataRegisterDTO.builder().build();
        EtcdClient client = mock(EtcdClient.class);

        when(client.getChildren(anyString())).thenReturn(Arrays.asList("/path1", "/path2"));
        when(client.read(anyString())).thenReturn(GsonUtils.getInstance().toJson(data));

        doAnswer(invocationOnMock -> {
            this.watchHandler = invocationOnMock.getArgument(1);
            return null;
        }).when(client).subscribeChildChanges(anyString(), any(EtcdListenHandler.class));

        return client;
    }
}
