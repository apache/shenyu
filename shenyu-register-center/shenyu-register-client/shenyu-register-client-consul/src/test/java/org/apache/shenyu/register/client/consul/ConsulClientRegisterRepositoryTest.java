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

package org.apache.shenyu.register.client.consul;

import com.ecwid.consul.v1.ConsulClient;
import com.ecwid.consul.v1.Response;
import com.ecwid.consul.v1.agent.model.NewService;
import com.ecwid.consul.v1.agent.model.Service;
import com.ecwid.consul.v1.kv.model.GetValue;
import com.google.common.collect.Maps;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.enums.EventType;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.Mockito.mock;

/**
 * test for ConsulClientRegisterRepository.
 */
public class ConsulClientRegisterRepositoryTest {

    private ConsulClientRegisterRepository repository;

    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException {
        this.repository = new ConsulClientRegisterRepository();
        Class<? extends ConsulClientRegisterRepository> clazz = this.repository.getClass();

        String consulClientFieldStr = "consulClient";
        Field consulClientField = clazz.getDeclaredField(consulClientFieldStr);
        consulClientField.setAccessible(true);
        consulClientField.set(repository, mockConsulClient());

        String serviceFieldStr = "service";
        Field serviceField = clazz.getDeclaredField(serviceFieldStr);
        serviceField.setAccessible(true);
        serviceField.set(repository, mockNewService());

        String customMetadataPathFieldStr = "customMetadataPath";
        Field customMetadataPathField = clazz.getDeclaredField(customMetadataPathFieldStr);
        customMetadataPathField.setAccessible(true);
        customMetadataPathField.set(repository, "XJGateway/register");
    }

    private ConsulClient mockConsulClient() {
        URIRegisterDTO mockServer = URIRegisterDTO.builder().appName("mockServer").contextPath("/mockServer").eventType(EventType.REGISTER).build();
        Service newService = new Service();
        Map<String, String> map = Maps.newHashMap();
        map.put("uri", GsonUtils.getInstance().toJson(mockServer));
        newService.setMeta(map);
        ConsulClient client = mock(ConsulClient.class);

        GetValue getValue = new GetValue();
        getValue.setValue("eyJhcHBOYW1lIjoiWEpHYXRld2F5LVJQQy1DbGllbnQiLCJjb250ZXh0UGF0aCI6Ii9ycGMiLCJwYXRoIjoiL3JwYy9vcmRlci9wYXRoL3tpZH0ve25hbWV9IiwicGF0aERlc"
                + "2MiOiIiLCJycGNUeXBlIjoic3ByaW5nQ2xvdWQiLCJzZXJ2aWNlTmFtZSI6ImNvbS54amdjLmdhdGV3YXkuY2xpZW50LnJwYy5jb250cm9sbGVyLk9yZGVyQ29udHJvbGxlciIsIm1ldGhv"
                + "ZE5hbWUiOiJnZXRQYXRoVmFyaWFibGUiLCJydWxlTmFtZSI6Ii9ycGMvb3JkZXIvcGF0aC97aWR9L3tuYW1lfSIsInBhcmFtZXRlclR5cGVzIjoiamF2YS5sYW5nLlN0cmluZyxqYXZhLmx"
                + "hbmcuU3RyaW5nIiwiZW5hYmxlZCI6dHJ1ZSwicGx1Z2luTmFtZXMiOltdLCJyZWdpc3Rlck1ldGFEYXRhIjpmYWxzZSwidGltZU1pbGxpcyI6MTY4OTg0NTkyMjE2MywiYWRkUHJlZml4ZW"
                + "QiOmZhbHNlfQ==");
        Response<GetValue> getValueResponse = new Response<>(getValue, 1L, true, 1L);
        Mockito.when(client.getKVValue("XJGateway/register/metadata/springCloud/rpc/rpc--rpc-order-path-%7Bid%7D-%7Bname%7D")).thenReturn(getValueResponse);

        Response<Boolean> setValueResponse = new Response<>(Boolean.TRUE, 1L, true, 1L);
        Mockito.when(client.setKVValue("XJGateway/register/metadata/springCloud/rpc/rpc--rpc-order-path-%7Bid%7D-%7Bname%7D", "{}")).thenReturn(setValueResponse);
        return client;
    }

    private NewService mockNewService() {
        NewService service = new NewService();
        service.setMeta(new HashMap<>());
        service.setName("Consul-Client");
        service.setId("Consul-Client-01");
        service.setAddress("127.0.0.1");
        String tags = "test";
        if (StringUtils.isNotBlank(tags)) {
            service.setTags(new ArrayList<>(Arrays.asList(tags.split(","))));
        }
        service.setEnableTagOverride(false);
        service.setPort(10086);
        return service;
    }

    @Test
    public void testPersistInterface() {
        final MetaDataRegisterDTO data = MetaDataRegisterDTO.builder()
                .rpcType("springCloud")
                .appName("Consul-Client")
                .path("/rpc/order/path/{id}/{name}")
                .serviceName("org.apache.shenyu.examples.springcloud.controller.OrderController")
                .methodName("getPathVariable")
                .host("host")
                .port(80)
                .contextPath("/rpc")
                .ruleName("/rpc/order/path/{id}/{name}")
                .parameterTypes("java.lang.String")
                .addPrefixed(false)
                .enabled(false)
                .build();
        repository.persistInterface(data);
        repository.closeRepository();
    }

}
