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

package org.apache.shenyu.register.client.server.zookeeper;

import org.apache.curator.test.TestingServer;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.client.server.api.ShenyuClientServerRegisterPublisher;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.apache.shenyu.register.common.type.DataTypeParent;
import org.apache.zookeeper.CreateMode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test for Zookeeper register center.
 */
public class ZookeeperServerRegisterRepositoryTest {
    
    private ZookeeperClientServerRegisterRepository repository;
    
    private ShenyuClientServerRegisterPublisher publisher;
    
    private ZookeeperClient client;

    @BeforeEach
    public void setUp() throws Exception {
        this.publisher = mockPublish();

        TestingServer server = new TestingServer();
        ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
        config.setServerLists(server.getConnectString());
        this.repository = new ZookeeperClientServerRegisterRepository();
        this.repository.init(publisher, config);

        Class<? extends ZookeeperClientServerRegisterRepository> clazz = this.repository.getClass();

        String fieldString = "client";
        Field field = clazz.getDeclaredField(fieldString);
        field.setAccessible(true);
        this.client = (ZookeeperClient) field.get(repository);
    }
    
    private ShenyuClientServerRegisterPublisher mockPublish() {
        ShenyuClientServerRegisterPublisher publisher = mock(ShenyuClientServerRegisterPublisher.class);
        doNothing().when(publisher).publish(localAny());
        return publisher;
    }

    @Test
    public void testSubscribeMetaData() throws InterruptedException {
        MetaDataRegisterDTO data = MetaDataRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .contextPath("/context")
                .ruleName("ruleName")
                .build();
        String metadataPath = "/shenyu/register/metadata/http/context/context-ruleName";
        client.createOrUpdate(metadataPath, GsonUtils.getInstance().toJson(data), CreateMode.PERSISTENT);

        // wait 1ms for listener being invoked.
        Thread.sleep(1000);

        verify(publisher, times(1)).publish(localAny());
        repository.close();
    }
    
    @Test
    public void testSubscribeURI() throws Exception {
        URIRegisterDTO data = URIRegisterDTO.builder()
                .rpcType("http")
                .host("host")
                .port(80)
                .appName("/context")
                .build();
        String uriPath = "/shenyu/register/uri/http/context/host:80";
        client.createOrUpdate(uriPath, GsonUtils.getInstance().toJson(data), CreateMode.EPHEMERAL);

        // wait 1ms for listener being invoked.
        Thread.sleep(1000);

        verify(publisher, times(1)).publish(localAny());

        client.delete(uriPath);

        // wait 1ms for listener being invoked.
        Thread.sleep(1000);

        verify(publisher, times(2)).publish(localAny());
    }
    
    private List<DataTypeParent> localAny() {
        return any();
    }
}
