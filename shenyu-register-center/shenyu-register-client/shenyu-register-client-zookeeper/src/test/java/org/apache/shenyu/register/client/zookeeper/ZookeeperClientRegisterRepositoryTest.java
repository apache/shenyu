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

import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.listen.Listenable;
import org.apache.curator.framework.state.ConnectionState;
import org.apache.curator.framework.state.ConnectionStateListener;
import org.apache.shenyu.common.enums.RpcTypeEnum;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.apache.shenyu.register.common.dto.URIRegisterDTO;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;

import java.util.ArrayList;
import java.util.List;
import java.util.Properties;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockConstruction;
import static org.mockito.Mockito.when;

/**
 * Test for Zookeeper client register repository.
 */
public class ZookeeperClientRegisterRepositoryTest {

    @Test
    public void testZookeeperInstanceRegisterRepository() {
        final Listenable listenable = mock(Listenable.class);
        try (MockedConstruction<ZookeeperClient> construction = mockConstruction(ZookeeperClient.class, (mock, context) -> {
            final CuratorFramework curatorFramework = mock(CuratorFramework.class);
            when(mock.getClient()).thenReturn(curatorFramework);
            when(curatorFramework.getConnectionStateListenable()).thenReturn(listenable);
        })) {
            ShenyuRegisterCenterConfig config = new ShenyuRegisterCenterConfig();
            final ZookeeperClientRegisterRepository repository = new ZookeeperClientRegisterRepository(config);
            final Properties configProps = config.getProps();
            configProps.setProperty("digest", "digest");
            List<ConnectionStateListener> connectionStateListeners = new ArrayList<>();
            doAnswer(invocationOnMock -> {
                connectionStateListeners.add(invocationOnMock.getArgument(0));
                return null;
            }).when(listenable).addListener(any());
            repository.init(config);

            repository.persistInterface(MetaDataRegisterDTO.builder().appName("mockServer").contextPath("/mock")
                    .ruleName("/rule").host("127.0.0.1").rpcType(RpcTypeEnum.HTTP.getName()).build());

            repository.persistInterface(MetaDataRegisterDTO.builder().appName("mockServer").contextPath("/mock")
                    .ruleName("/rule").host("127.0.0.1").rpcType(RpcTypeEnum.SPRING_CLOUD.getName()).build());

            repository.persistInterface(MetaDataRegisterDTO.builder().appName("mockServer").contextPath("/mock")
                    .ruleName("/rule").host("127.0.0.1").rpcType(RpcTypeEnum.TARS.getName()).build());

            repository.persistURI(URIRegisterDTO.builder().protocol("http://").appName("test1")
                    .rpcType(RpcTypeEnum.HTTP.getName()).host("localhost").port(8091).build());
            connectionStateListeners.forEach(connectionStateListener -> {
                connectionStateListener.stateChanged(null, ConnectionState.RECONNECTED);
            });
            Assertions.assertDoesNotThrow(() -> new ZookeeperClientRegisterRepository());
            repository.close();
        }
    }
}
