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

package org.apache.shenyu.register.instance.zookeeper;

import org.I0Itec.zkclient.ZkClient;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.mock;

public class ZookeeperInstanceRegisterRepositoryTest {

    private final List<String> registerPath = new ArrayList<>();

    private final Map<String, String> registerInstanceMap
            = new HashMap<>();

    private final ZookeeperInstanceRegisterRepository repository
            = new ZookeeperInstanceRegisterRepository();

    @BeforeEach
    public void setup() throws Exception {
        final ZkClient zkClient = mockZkClient();
        final Field field = repository.getClass().getDeclaredField("zkClient");
        field.setAccessible(true);
        field.set(repository, zkClient);
    }

    @Test
    public void testPersistInstance() {
        InstanceRegisterDTO data = InstanceRegisterDTO.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();
        repository.persistInstance(data);
        assertTrue(registerPath.contains("/shenyu/register/instance"));
        assertEquals(registerInstanceMap.get("/shenyu/register/instance/shenyu-host:9195"), "{\"appName\":\"shenyu-test\",\"host\":\"shenyu-host\",\"port\":9195}");
    }

    private ZkClient mockZkClient() {
        final ZkClient zkClient = mock(ZkClient.class);
        doAnswer(invocation -> {
            final String path = invocation.getArgument(0);
            registerPath.add(path);
            return null;
        }).when(zkClient).createPersistent(anyString(), anyBoolean());
        doAnswer(invocation -> {
            final String path = invocation.getArgument(0);
            final String nodeValue = invocation.getArgument(1);
            registerInstanceMap.put(path, nodeValue);
            return null;
        }).when(zkClient).createEphemeral(anyString(), anyString());
        return zkClient;
    }
}
