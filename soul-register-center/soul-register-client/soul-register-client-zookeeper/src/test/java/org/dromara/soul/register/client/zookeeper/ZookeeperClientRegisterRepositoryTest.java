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

package org.dromara.soul.register.client.zookeeper;

import org.I0Itec.zkclient.ZkClient;
import org.dromara.soul.common.utils.GsonUtils;
import org.dromara.soul.register.common.dto.MetaDataRegisterDTO;
import org.dromara.soul.register.common.dto.URIRegisterDTO;
import org.junit.Before;
import org.junit.Test;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

/**
 * Test for Zookeeper client register repository.
 *
 * @author lw1243925457
 */
public class ZookeeperClientRegisterRepositoryTest {

    private ZookeeperClientRegisterRepository repository;

    private final Map<String, Object> zookeeperBroker = new HashMap<>();

    private final Set<String> ephemeralNode = new HashSet<>();

    @Before
    public void setUp() throws NoSuchFieldException, IllegalAccessException {
        this.repository = new ZookeeperClientRegisterRepository();
        Class<? extends ZookeeperClientRegisterRepository> clazz = this.repository.getClass();

        String fieldString = "zkClient";
        Field field = clazz.getDeclaredField(fieldString);
        field.setAccessible(true);
        field.set(repository, mockZkClient());

        zookeeperBroker.clear();
        ephemeralNode.clear();
    }

    private ZkClient mockZkClient() {
        ZkClient zkClient = mock(ZkClient.class);

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            String value = invocationOnMock.getArgument(1);
            zookeeperBroker.put(key, value);
            ephemeralNode.add(key);
            return true;
        }).when(zkClient).createEphemeral(anyString(), any(Object.class));

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            String value = invocationOnMock.getArgument(1);
            zookeeperBroker.put(key, value);
            return true;
        }).when(zkClient).createPersistent(any(), any());

        doAnswer(invocationOnMock -> {
            String key = invocationOnMock.getArgument(0);
            String value = invocationOnMock.getArgument(1);
            zookeeperBroker.put(key, value);
            return true;
        }).when(zkClient).writeData(any(), any());

        doAnswer(invocationOnMock -> {
            String node = invocationOnMock.getArgument(0);
            return zookeeperBroker.containsKey(node);
        }).when(zkClient).exists(anyString());

        doAnswer(invocationOnMock -> {
            ephemeralNode.forEach(zookeeperBroker::remove);
            return true;
        }).when(zkClient).close();

        return zkClient;
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
        String metadataPath = "/soul/register/metadata/http/context/context-ruleName";
        assert zookeeperBroker.containsKey(metadataPath);
        assert zookeeperBroker.get(metadataPath).equals(GsonUtils.getInstance().toJson(data));

        String uriPath = "/soul/register/uri/http/context/host:80";
        assert zookeeperBroker.containsKey(uriPath);
        assert zookeeperBroker.get(uriPath).equals(GsonUtils.getInstance().toJson(URIRegisterDTO.transForm(data)));

        repository.close();
        assert zookeeperBroker.containsKey(metadataPath);
        assert !zookeeperBroker.containsKey(uriPath);
    }

}
