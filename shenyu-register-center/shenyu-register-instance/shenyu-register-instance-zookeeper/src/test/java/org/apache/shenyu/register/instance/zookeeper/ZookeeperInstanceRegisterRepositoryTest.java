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

import org.apache.curator.test.TestingServer;
import org.apache.shenyu.common.config.ShenyuConfig;
import org.apache.shenyu.common.utils.GsonUtils;
import org.apache.shenyu.register.common.dto.InstanceRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class ZookeeperInstanceRegisterRepositoryTest {

    private final ZookeeperInstanceRegisterRepository repository
            = new ZookeeperInstanceRegisterRepository();

    private ZookeeperClient client;

    @BeforeEach
    public void setup() throws Exception {
        TestingServer server = new TestingServer();
        ShenyuConfig.InstanceConfig config = new ShenyuConfig.InstanceConfig();
        config.setServerLists(server.getConnectString());
        this.repository.init(config);

        Class<? extends ZookeeperInstanceRegisterRepository> clazz = this.repository.getClass();

        String fieldString = "client";
        Field field = clazz.getDeclaredField(fieldString);
        field.setAccessible(true);
        this.client = (ZookeeperClient) field.get(repository);
    }

    @Test
    public void testPersistInstance() {
        InstanceRegisterDTO data = InstanceRegisterDTO.builder()
                .appName("shenyu-test")
                .host("shenyu-host")
                .port(9195)
                .build();
        repository.persistInstance(data);
        String value = client.get("/shenyu/register/instance/shenyu-host:9195");
        assertEquals(value, GsonUtils.getInstance().toJson(data));
    }
}
