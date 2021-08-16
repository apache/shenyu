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

package org.apache.shenyu.register.client.nacos;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.exception.NacosException;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.pojo.Instance;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test for nacos register center client.
 */
public class NacosClientRegisterRepositoryTest {

    private NacosClientRegisterRepository repository;

    private final Map<String, Object> nacosBroker = new HashMap<>();

    private ConfigService configService;

    private NamingService namingService;

    @Before
    public void setUp() throws IllegalAccessException, NoSuchFieldException, NacosException {
        this.repository = new NacosClientRegisterRepository();
        this.configService = mockConfigService();
        this.namingService = mockNamingService();
        Class<? extends NacosClientRegisterRepository> clazz = this.repository.getClass();

        String configFiledStr = "configService";
        Field configFiled = clazz.getDeclaredField(configFiledStr);
        configFiled.setAccessible(true);
        configFiled.set(repository, this.configService);

        String namingFiledStr = "namingService";
        Field namingFiled = clazz.getDeclaredField(namingFiledStr);
        namingFiled.setAccessible(true);
        namingFiled.set(repository, this.namingService);

        nacosBroker.clear();
    }

    private ConfigService mockConfigService() throws NacosException {
        ConfigService configService = mock(ConfigService.class);

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            String value = invocationOnMock.getArgument(2);
            nacosBroker.put(key, value);
            return null;
        }).when(configService).publishConfig(anyString(), anyString(), anyString());

        return configService;
    }

    private NamingService mockNamingService() throws NacosException {
        NamingService namingService = mock(NamingService.class);

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            Instance instance = invocationOnMock.getArgument(1);
            nacosBroker.put(key, instance);
            return null;
        }).when(namingService).registerInstance(anyString(), any());

        return namingService;
    }

    @Test
    public void testClose() throws NacosException {
        repository.close();
        verify(configService, times(1)).shutDown();
        verify(namingService, times(1)).shutDown();
    }

    @Test
    public void testPersistInterface() {
        final MetaDataRegisterDTO data = MetaDataRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .contextPath("/context")
                .ruleName("ruleName")
                .build();

        repository.persistInterface(data);
        String uriInstancePath = "shenyu.register.service.http";
        assert nacosBroker.containsKey(uriInstancePath);
        Instance instance = (Instance) nacosBroker.get(uriInstancePath);
        assert instance.getPort() == data.getPort();
        assert instance.getIp().equals(data.getHost());
        Map<String, String> metadataMap = instance.getMetadata();
        assert metadataMap.get("uriMetadata").equals(GsonUtils.getInstance().toJson(URIRegisterDTO.transForm(data)));

        String configPath = "shenyu.register.service.http.context";
        assert nacosBroker.containsKey(configPath);
        String dataStr = GsonUtils.getInstance().toJson(data);
        assert nacosBroker.get(configPath).equals(GsonUtils.getInstance().toJson(Collections.singletonList(dataStr)));
    }

}
