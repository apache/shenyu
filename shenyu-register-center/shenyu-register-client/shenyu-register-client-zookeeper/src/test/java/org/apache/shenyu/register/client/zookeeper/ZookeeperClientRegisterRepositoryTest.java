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

package org.apache.shenyu.register.client.zookeeper;

import org.I0Itec.zkclient.ZkClient;
import org.apache.curator.test.TestingServer;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

/**
 * Test for Zookeeper client register repository.
 */
public class ZookeeperClientRegisterRepositoryTest {

    private static TestingServer zkServer;

    private ZookeeperClientRegisterRepository repository;

    private final Map<String, Object> zookeeperBroker = new HashMap<>();

    private final Set<String> ephemeralNode = new HashSet<>();

    @BeforeClass
    public static void setUpBeforeClass() throws Exception {
        zkServer = new TestingServer(1181, true);
    }

    @AfterClass
    public static void tearDownAfterClass() throws IOException {
        zkServer.stop();
    }

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
        String metadataPath = "/shenyu/register/metadata/http/context/context-ruleName";
        assert zookeeperBroker.containsKey(metadataPath);
        assert zookeeperBroker.get(metadataPath).equals(GsonUtils.getInstance().toJson(data));

        String uriPath = "/shenyu/register/uri/http/context/host:80";
        assert zookeeperBroker.containsKey(uriPath);
        assert zookeeperBroker.get(uriPath).equals(GsonUtils.getInstance().toJson(URIRegisterDTO.transForm(data)));

        repository.close();
        assert zookeeperBroker.containsKey(metadataPath);
        assert !zookeeperBroker.containsKey(uriPath);
    }

    @Test
    public void testPersistInterfaceDoAnswerWriteData4Grpc() {

        ZkClient zkClient = mock(ZkClient.class);

        when(zkClient.exists(anyString())).thenReturn(true);

        final MetaDataRegisterDTO data = MetaDataRegisterDTO.builder()
                .rpcType(RpcTypeEnum.GRPC.getName())
                .host("host")
                .port(80)
                .contextPath("/context")
                .ruleName("ruleName")
                .serviceName("testService")
                .methodName("testMethod")
                .build();

        repository.persistInterface(data);
        String metadataPath = "/shenyu/register/metadata/grpc/context/testService.testMethod";
        assert zookeeperBroker.containsKey(metadataPath);
        assert zookeeperBroker.get(metadataPath).equals(GsonUtils.getInstance().toJson(data));

        String uriPath = "/shenyu/register/uri/grpc/context/host:80";
        assert zookeeperBroker.containsKey(uriPath);
        assert zookeeperBroker.get(uriPath).equals(GsonUtils.getInstance().toJson(URIRegisterDTO.transForm(data)));

        repository.close();
        assert zookeeperBroker.containsKey(metadataPath);
        assert !zookeeperBroker.containsKey(uriPath);
    }

    @Test
    public void testInit() {
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setServerLists("127.0.0.1:1181");
        repository.init(config);
    }

}
