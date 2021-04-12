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

package nacos;

import com.alibaba.nacos.api.config.ConfigService;
import com.alibaba.nacos.api.naming.NamingService;
import com.alibaba.nacos.api.naming.pojo.Instance;
import lombok.SneakyThrows;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.register.client.nacos.NacosClientRegisterRepository;
import org.dromara.soul.register.common.dto.MetaDataRegisterDTO;
import org.dromara.soul.register.common.dto.URIRegisterDTO;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

/**
 * Test for nacos register center client.
 *
 * @author lw1243925457
 */
public class NacosClientRegisterRepositoryTest {

    private NacosClientRegisterRepository repository;

    private final Map<String, Object> nacosBroker = new HashMap<>();

    @Before
    public void setUp() throws IllegalAccessException, NoSuchFieldException {
        this.repository = new NacosClientRegisterRepository();
        Class<? extends NacosClientRegisterRepository> clazz = this.repository.getClass();

        String configFiledStr = "configService";
        Field configFiled = clazz.getDeclaredField(configFiledStr);
        configFiled.setAccessible(true);
        configFiled.set(repository, mockConfigService());

        String namingFiledStr = "namingService";
        Field namingFiled = clazz.getDeclaredField(namingFiledStr);
        namingFiled.setAccessible(true);
        namingFiled.set(repository, mockNamingService());

        nacosBroker.clear();
    }

    @SneakyThrows
    private ConfigService mockConfigService() {
        ConfigService configService = mock(ConfigService.class);

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            String value = invocationOnMock.getArgument(2);
            nacosBroker.put(key, value);
            return null;
        }).when(configService).publishConfig(anyString(), anyString(), anyString());

        return configService;
    }

    @SneakyThrows
    private NamingService mockNamingService() {
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
    public void testPersistInterface() {
        final MetaDataRegisterDTO data = MetaDataRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .contextPath("/context")
                .ruleName("ruleName")
                .build();

        repository.persistInterface(data);

        String uriInstancePath = "soul.register.service.http";
        assert nacosBroker.containsKey(uriInstancePath);
        Instance instance = (Instance) nacosBroker.get(uriInstancePath);
        assert instance.getPort() == data.getPort();
        assert instance.getIp().equals(data.getHost());
        Map<String, String> metadataMap = instance.getMetadata();
        assert metadataMap.get("uriMetadata").equals(GsonUtils.getInstance().toJson(URIRegisterDTO.transForm(data)));

        String configPath = "soul.register.service.http.context";
        assert nacosBroker.containsKey(configPath);
        String dataStr = GsonUtils.getInstance().toJson(data);
        assert nacosBroker.get(configPath).equals(GsonUtils.getInstance().toJson(Collections.singletonList(dataStr)));
    }

}
