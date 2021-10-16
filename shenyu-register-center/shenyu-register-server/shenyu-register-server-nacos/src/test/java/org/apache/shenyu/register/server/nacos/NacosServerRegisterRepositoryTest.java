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

package org.apache.shenyu.register.server.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.config.listener.Listener;
import com.alibaba.nacos.api.naming.listener.EventListener;
import com.alibaba.nacos.api.naming.listener.NamingEvent;
import com.alibaba.nacos.api.naming.pojo.Instance;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.server.api.ShenyuServerRegisterPublisher;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;
import java.util.Collections;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.anyLong;
import static org.mockito.Mockito.anyBoolean;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.times;

/**
 * Test for NacosServerRegisterRepository.
 */
public class NacosServerRegisterRepositoryTest {

    private NacosServerRegisterRepository repository;

    private ShenyuServerRegisterPublisher publisher;

    private Listener configListener;

    private EventListener eventListener;

    @Before
    public void setUp() throws NoSuchFieldException, IllegalAccessException, NacosException {
        this.publisher = mockPublish();
        this.repository = new NacosServerRegisterRepository();
        Class<? extends NacosServerRegisterRepository> clazz = this.repository.getClass();

        String configServiceString = "configService";
        Field configService = clazz.getDeclaredField(configServiceString);
        configService.setAccessible(true);
        configService.set(repository, mockConfigService());

        String namingServiceString = "namingService";
        Field namingService = clazz.getDeclaredField(namingServiceString);
        namingService.setAccessible(true);
        namingService.set(repository, mockNamingService());

        String fieldPublisherString = "publisher";
        Field fieldPublisher = clazz.getDeclaredField(fieldPublisherString);
        fieldPublisher.setAccessible(true);
        fieldPublisher.set(repository, publisher);
    }

    private ConfigService mockConfigService() throws NacosException {
        ConfigService configService = mock(ConfigService.class);

        doAnswer(invocationOnMock -> {
            this.configListener = invocationOnMock.getArgument(2);
            return null;
        }).when(configService).addListener(anyString(), anyString(), any(Listener.class));

        doAnswer(invocationOnMock -> {
            List<String> list = new ArrayList<>();
            list.add(GsonUtils.getInstance().toJson(MetaDataRegisterDTO.builder().build()));
            return GsonUtils.getInstance().toJson(list);
        }).when(configService).getConfig(anyString(), anyString(), anyLong());

        return configService;
    }

    private NamingService mockNamingService() throws NacosException {
        NamingService namingService = mock(NamingService.class);

        doAnswer(invocationOnMock -> mockInstances())
                .when(namingService).selectInstances(anyString(), anyBoolean());

        doAnswer(invocationOnMock -> {
            this.eventListener = invocationOnMock.getArgument(1);
            return null;
        }).when(namingService).subscribe(anyString(), any(EventListener.class));

        return namingService;
    }

    private List<Instance> mockInstances() {
        MetaDataRegisterDTO metadata = MetaDataRegisterDTO.builder().build();
        Map<String, String> metadataMap = new HashMap<>(1);
        metadataMap.put("contextPath", "contextPath");
        metadataMap.put("uriMetadata", GsonUtils.getInstance().toJson(URIRegisterDTO.transForm(metadata)));

        Instance instance = new Instance();
        instance.setEphemeral(true);
        instance.setIp("127.0.0.1");
        instance.setPort(80);
        instance.setMetadata(metadataMap);

        return Collections.singletonList(instance);
    }

    private ShenyuServerRegisterPublisher mockPublish() {
        ShenyuServerRegisterPublisher publisher = mock(ShenyuServerRegisterPublisher.class);
        doNothing().when(publisher).publish(any());
        return publisher;
    }

    private NamingEvent mockEvent() {
        return new NamingEvent("serviceName", mockInstances());
    }

    @Test
    public void testSubscribeTypeOfSupportURI() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        Class<? extends NacosServerRegisterRepository> clazz = this.repository.getClass();
        String methodString = "subscribeRpcTypeService";
        Method method = clazz.getDeclaredMethod(methodString, RpcTypeEnum.class);
        method.setAccessible(true);
        method.invoke(repository, RpcTypeEnum.HTTP);
        verify(publisher, times(2)).publish(any());

        List<String> list = new ArrayList<>();
        list.add(GsonUtils.getInstance().toJson(MetaDataRegisterDTO.builder().build()));
        configListener.receiveConfigInfo(GsonUtils.getInstance().toJson(list));
        verify(publisher, times(3)).publish(any());

        eventListener.onEvent(mockEvent());
        verify(publisher, times(4)).publish(any());
    }

    @Test
    public void testSubscribeTypeOfNotSupportURI() throws NoSuchMethodException, InvocationTargetException,
            IllegalAccessException {
        Class<? extends NacosServerRegisterRepository> clazz = this.repository.getClass();
        String methodString = "subscribeRpcTypeService";
        Method method = clazz.getDeclaredMethod(methodString, RpcTypeEnum.class);
        method.setAccessible(true);
        method.invoke(repository, RpcTypeEnum.DUBBO);
        verify(publisher, times(2)).publish(any());

        List<String> list = new ArrayList<>();
        list.add(GsonUtils.getInstance().toJson(MetaDataRegisterDTO.builder().build()));
        configListener.receiveConfigInfo(GsonUtils.getInstance().toJson(list));
        verify(publisher, times(3)).publish(any());

        eventListener.onEvent(mockEvent());
        verify(publisher, times(4)).publish(any());
    }
}
