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

package org.apache.shenyu.register.client.server.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.agent.model.Service;
import com.ecwid.consul.v1.kv.model.GetValue;
import com.google.common.collect.Maps;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.boot.test.context.runner.ApplicationContextRunner;
import org.springframework.context.annotation.Bean;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;

/**
 * The TestCase for {@link ConsulClientServerRegisterRepository}.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class ConsulServerRegisterRepositoryTest {
    
    private ShenyuClientServerRegisterPublisher mockPublish() {
        ShenyuClientServerRegisterPublisher publisher = mock(ShenyuClientServerRegisterPublisher.class);
        doNothing().when(publisher).publish(localAny());
        return publisher;
    }
    
    @Bean
    private ConsulClient mockConsulClient() {
        URIRegisterDTO mockServer = URIRegisterDTO.builder().appName("mockServer").contextPath("/mockServer").eventType(EventType.REGISTER).build();
        Service newService = new Service();
        Map<String, String> map = Maps.newHashMap();
        map.put("uri", GsonUtils.getInstance().toJson(mockServer));
        newService.setMeta(map);
        ConsulClient client = mock(ConsulClient.class);
        
        Map<String, Service> serviceHashMap = Maps.newHashMap();
        serviceHashMap.put(mockServer.getContextPath(), newService);
        Response<Map<String, Service>> mapResponse = new Response<>(serviceHashMap, 1L, true, 1L);
        Mockito.when(client.getAgentServices()).thenReturn(mapResponse);
        return client;
    }
    
    @Bean
    private ConsulClientServerRegisterRepository mockConsulServerRegisterRepository() throws Exception {
        ConsulClientServerRegisterRepository consulServerRegisterRepository = new ConsulClientServerRegisterRepository();
        Class<? extends ConsulClientServerRegisterRepository> clazz = consulServerRegisterRepository.getClass();
        
        String fieldClientString = "consulClient";
        Field fieldClient = clazz.getDeclaredField(fieldClientString);
        fieldClient.setAccessible(true);
        fieldClient.set(consulServerRegisterRepository, mockConsulClient());
        
        String fieldPublisherString = "publisher";
        Field fieldPublisher = clazz.getDeclaredField(fieldPublisherString);
        fieldPublisher.setAccessible(true);
        fieldPublisher.set(consulServerRegisterRepository, mockPublish());
        return consulServerRegisterRepository;
    }
    
    @Test
    public void testConsulServerRegisterRepository() {
        new ApplicationContextRunner().withUserConfiguration(ConsulServerRegisterRepositoryTest.class)
                .run(context -> {
                    MetaDataRegisterDTO mockServer = MetaDataRegisterDTO.builder().appName("mockServer").contextPath("/mock")
                            .host("127.0.0.1").rpcType(RpcTypeEnum.DUBBO.getName()).build();
                    Map<String, GetValue> mateData = new HashMap<>();
                    GetValue getValue = new GetValue();
                    getValue.setValue(Base64.getEncoder().encodeToString(GsonUtils.getInstance().toJson(mockServer).getBytes(StandardCharsets.UTF_8)));
                    getValue.setCreateIndex(1L);
                    mateData.put("/mock", getValue);
                    context.publishEvent(new ConsulConfigChangedEvent(this, 1L, mateData));
                    ConsulConfigChangedEvent consulConfigChangedEvent = new ConsulConfigChangedEvent(this, 1L, mateData);
                    context.publishEvent(consulConfigChangedEvent);
                    Assertions.assertEquals(consulConfigChangedEvent.getConsulIndex(), 1L);
                });
    }

    @Test
    public void initTest() {
        final MockedConstruction<ConsulClient> consulClientMockedConstruction = mockConstruction(ConsulClient.class);
        ConsulClientServerRegisterRepository consulServerRegisterRepository = new ConsulClientServerRegisterRepository();
        final ShenyuClientServerRegisterPublisher shenyuClientServerRegisterPublisher = mock(ShenyuClientServerRegisterPublisher.class);
        final ShenyuRegisterCenterConfig shenyuRegisterCenterConfig = mock(ShenyuRegisterCenterConfig.class);
        consulServerRegisterRepository.init(shenyuClientServerRegisterPublisher, shenyuRegisterCenterConfig);
        consulClientMockedConstruction.close();
    }

    @Test
    public void publishRegisterURITest() throws NoSuchMethodException, InvocationTargetException, IllegalAccessException, NoSuchFieldException {
        final ConsulClientServerRegisterRepository consulServerRegisterRepository = new ConsulClientServerRegisterRepository();
        final Method publishRegisterURI = ConsulClientServerRegisterRepository.class.getDeclaredMethod("publishRegisterURI", String.class, List.class);
        publishRegisterURI.setAccessible(true);
        Field fieldPublisher = ConsulClientServerRegisterRepository.class.getDeclaredField("publisher");
        fieldPublisher.setAccessible(true);
        fieldPublisher.set(consulServerRegisterRepository, mockPublish());
        publishRegisterURI.invoke(consulServerRegisterRepository, "http", new ArrayList<>());
    }
    
    private DataTypeParent localAny() {
        return any();
    }
}
