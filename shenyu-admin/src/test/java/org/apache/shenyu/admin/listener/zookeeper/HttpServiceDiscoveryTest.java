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

package org.apache.shenyu.admin.listener.zookeeper;

import org.apache.curator.test.TestingServer;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.dto.SelectorData;
import org.apache.shenyu.register.client.server.zookeeper.ZookeeperClient;
import org.apache.zookeeper.CreateMode;
import org.junit.jupiter.api.Test;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.core.env.Environment;

import java.lang.reflect.Field;

import static org.apache.shenyu.admin.listener.zookeeper.HttpServiceDiscovery.ROOT;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * The TestCase for {@link HttpServiceDiscovery}.
 */
public final class HttpServiceDiscoveryTest {

    @Test
    public void testAfterPropertiesSet() throws Exception {
        SelectorService selectorService = mock(SelectorService.class);
        SelectorMapper selectorMapper = mock(SelectorMapper.class);
        ApplicationEventPublisher eventPublisher = mock(ApplicationEventPublisher.class);
        Environment env = mock(Environment.class);
        SelectorDO selector = mock(SelectorDO.class);
        SelectorData selectorData = mock(SelectorData.class);
        when(selectorService.findByName(anyString())).thenReturn(selector);
        when(selectorService.buildByName(anyString())).thenReturn(selectorData);
        when(env.getProperty("shenyu.http.register", Boolean.class, false)).thenReturn(true);

        TestingServer server = new TestingServer();
        when(env.getProperty("shenyu.http.zookeeperUrl", "")).thenReturn(server.getConnectString());

        HttpServiceDiscovery discovery = new HttpServiceDiscovery(selectorService, selectorMapper, eventPublisher, env);

        Class<? extends HttpServiceDiscovery> clazz = discovery.getClass();

        String server1 = "server1";
        discovery.afterPropertiesSet();

        String fieldString = "zkClient";
        Field field = clazz.getDeclaredField(fieldString);
        field.setAccessible(true);
        ZookeeperClient zkClient = (ZookeeperClient) field.get(discovery);
        zkClient.createOrUpdate(ROOT + "/" + server1 + "/hello", "", CreateMode.PERSISTENT);

        // wait for take effect
        Thread.sleep(500);
        verify(selectorMapper).updateSelective(selector);
    }
}
