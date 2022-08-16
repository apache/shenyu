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

package org.apache.shenyu.register.client.server.etcd;

import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.client.server.etcd.client.EtcdClient;
import org.apache.shenyu.register.client.server.etcd.client.EtcdListenHandler;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.times;

/**
 * test for EtcdClientServerRegisterRepository.
 */
public class EtcdServerRegisterRepositoryTest {
    
    private EtcdClientServerRegisterRepository repository;
    
    private ShenyuClientServerRegisterPublisher publisher;
    
    private EtcdListenHandler watchHandler;
    
    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException {
        this.publisher = mockPublish();
        this.repository = new EtcdClientServerRegisterRepository();
        Class<? extends EtcdClientServerRegisterRepository> clazz = this.repository.getClass();
        
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
        Class<? extends EtcdClientServerRegisterRepository> clazz = this.repository.getClass();
        String methodString = "subscribeMetaData";
        Method method = clazz.getDeclaredMethod(methodString, String.class);
        method.setAccessible(true);
        method.invoke(repository, "http");
        
        verify(publisher, times(2)).publish(localAny());
        
        String data = GsonUtils.getInstance().toJson(MetaDataRegisterDTO.builder().build());
        watchHandler.updateHandler("/path", data);
        watchHandler.deleteHandler("/path", data);
        verify(publisher, times(3)).publish(localAny());
    }

    @Test
    public void testSubscribeURI() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        Class<? extends EtcdClientServerRegisterRepository> clazz = this.repository.getClass();
        String methodString = "subscribeURI";
        Method method = clazz.getDeclaredMethod(methodString, String.class);
        method.setAccessible(true);
        MetaDataRegisterDTO data = MetaDataRegisterDTO.builder().build();
        EtcdClient client = mock(EtcdClient.class);
        when(client.getChildren(anyString())).thenReturn(Collections.singletonList("/http/path1/path2/path3/path4"));
        when(client.read(anyString())).thenReturn(GsonUtils.getInstance().toJson(data));

        doAnswer(invocationOnMock -> {
            this.watchHandler = invocationOnMock.getArgument(1);
            return null;
        }).when(client).subscribeChildChanges(anyString(), any(EtcdListenHandler.class));

        Field fieldClient = clazz.getDeclaredField("client");
        fieldClient.setAccessible(true);
        fieldClient.set(repository, client);
        method.invoke(repository, "http/path1/path2/path3/path4");
        verify(publisher, times(1)).publish(localAny());
        String data2 = GsonUtils.getInstance().toJson(MetaDataRegisterDTO.builder().build());
        watchHandler.updateHandler("/http/path1/path2/path3/path4", data2);
        watchHandler.deleteHandler("http/path1/path2/path3/path4", data2);
        verify(publisher, times(3)).publish(localAny());
    }

    @Test
    public void initTest() {
        try (MockedConstruction<EtcdClient> etcdClientMockedConstruction = mockConstruction(EtcdClient.class)) {
            final EtcdClientServerRegisterRepository etcdClientServerRegisterRepository = new EtcdClientServerRegisterRepository();
            final ShenyuClientServerRegisterPublisher shenyuClientServerRegisterPublisher = mock(ShenyuClientServerRegisterPublisher.class);
            final ShenyuRegisterCenterConfig shenyuRegisterCenterConfig = new ShenyuRegisterCenterConfig();
            etcdClientServerRegisterRepository.init(shenyuClientServerRegisterPublisher, shenyuRegisterCenterConfig);
            etcdClientServerRegisterRepository.close();
        } catch (Exception e) {
            throw new ShenyuException(e.getCause());
        }
    }

    @Test
    public void registerUriChildrenListTest() throws NoSuchMethodException, IllegalAccessException, NoSuchFieldException, InvocationTargetException {
        final EtcdClient client = mock(EtcdClient.class);
        final Field fieldClient = EtcdClientServerRegisterRepository.class.getDeclaredField("client");
        fieldClient.setAccessible(true);
        fieldClient.set(repository, client);

        final Method method = EtcdClientServerRegisterRepository.class.getDeclaredMethod("registerUriChildrenList", String.class, String.class, String.class);
        method.setAccessible(true);
        method.invoke(repository, "rpcPath", "context", "rpcType");
    }
    
    private ShenyuClientServerRegisterPublisher mockPublish() {
        ShenyuClientServerRegisterPublisher publisher = mock(ShenyuClientServerRegisterPublisher.class);
        doNothing().when(publisher).publish(localAny());
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
    
    private List<DataTypeParent> localAny() {
        return any();
    }
}
