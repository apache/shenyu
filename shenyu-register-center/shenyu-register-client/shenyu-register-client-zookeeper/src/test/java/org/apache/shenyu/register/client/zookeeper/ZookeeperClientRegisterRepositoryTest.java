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

import org.apache.curator.test.TestingServer;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.Properties;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Test for Zookeeper client register repository.
 */
public class ZookeeperClientRegisterRepositoryTest {

    private ZookeeperClientRegisterRepository repository;

    private ZookeeperClient client;

    @BeforeEach
    public void setUp() throws Exception {
        TestingServer server = new TestingServer();
        this.repository = new ZookeeperClientRegisterRepository();
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setServerLists(server.getConnectString());
        this.repository.init(config);

        Class<? extends ZookeeperClientRegisterRepository> clazz = this.repository.getClass();

        String fieldString = "client";
        Field field = clazz.getDeclaredField(fieldString);
        field.setAccessible(true);
        this.client = (ZookeeperClient) field.get(repository);
    }

    @Test
    public void initTest() throws Exception {
        TestingServer server = new TestingServer();
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setServerLists(server.getConnectString());
        this.repository = new ZookeeperClientRegisterRepository(config);
        Properties configProps = config.getProps();
        configProps.setProperty("digest", "digest");
        this.repository.init(config);
    }

    @Test
    public void testPersistInterface() {
        MetaDataRegisterDTO data = MetaDataRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .contextPath("/context")
                .ruleName("ruleName")
                .build();
        repository.persistInterface(data);
        String metadataPath = "/shenyu/register/metadata/http/context/context-ruleName";
        String value = client.get(metadataPath);
        assertEquals(value, GsonUtils.getInstance().toJson(data));
        repository.close();
    }

    @Test
    public void testPersistUri() {
        URIRegisterDTO data = URIRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .appName("/context")
                .build();
        repository.persistURI(data);
        String uriPath = "/shenyu/register/uri/http/context/host:80";
        String value = client.get(uriPath);
        assertEquals(value, GsonUtils.getInstance().toJson(data));
        repository.close();
    }

    @Test
    public void testPersistInterfaceDoAnswerWriteData4Grpc() {
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
        // hit `metadataSet.contains(realNode)`
        repository.persistInterface(data);
        String metadataPath = "/shenyu/register/metadata/grpc/context/testService.testMethod";
        String value = client.get(metadataPath);
        assertEquals(value, GsonUtils.getInstance().toJson(data));
        repository.close();
    }
}
