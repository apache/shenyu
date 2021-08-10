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

import org.I0Itec.zkclient.ZkClient;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.service.SelectorService;
import org.apache.shenyu.common.dto.SelectorData;
import org.assertj.core.util.Lists;
import org.junit.Test;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.core.env.Environment;

import java.util.List;

import static org.apache.shenyu.admin.listener.zookeeper.HttpServiceDiscovery.ROOT;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.powermock.api.mockito.PowerMockito.when;

/**
 * The TestCase for HttpServiceDiscovery.
 */
public final class HttpServiceDiscoveryTest {

    @Test
    public void testAfterPropertiesSet() {
        SelectorService selectorService = mock(SelectorService.class);
        SelectorMapper selectorMapper = mock(SelectorMapper.class);
        ApplicationEventPublisher eventPublisher = mock(ApplicationEventPublisher.class);
        Environment env = mock(Environment.class);
        SelectorDO selector = mock(SelectorDO.class);
        SelectorData selectorData = mock(SelectorData.class);
        when(selectorService.findByName(anyString())).thenReturn(selector);
        when(selectorService.buildByName(anyString())).thenReturn(selectorData);
        when(env.getProperty("shenyu.http.register", Boolean.class, false)).thenReturn(false, true);
        HttpServiceDiscovery discovery = new HttpServiceDiscovery(selectorService, selectorMapper, eventPublisher, env);
        HttpServiceDiscovery spy = spy(discovery);
        spy.afterPropertiesSet();
        verify(env).getProperty("shenyu.http.register", Boolean.class, false);
        String zookeeperUrl = "192.168.0.10:2181";
        when(env.getProperty("shenyu.http.zookeeperUrl", "")).thenReturn(zookeeperUrl);
        ZkClient zkClient = mock(ZkClient.class);
        doReturn(zkClient).when(spy).createZkClient(zookeeperUrl);
        when(zkClient.exists(ROOT)).thenReturn(false);
        String server1 = "server1";
        List<String> contextPathList = Lists.newArrayList(server1);
        when(zkClient.getChildren(ROOT)).thenReturn(contextPathList);
        when(zkClient.getChildren(ROOT + "/" + server1)).thenReturn(contextPathList);
        when(zkClient.readData(anyString())).thenReturn("192.169.0.9");
        spy.afterPropertiesSet();
        verify(zkClient).createPersistent(ROOT, true);
        verify(zkClient).getChildren(ROOT);
        verify(selectorMapper).updateSelective(selector);
    }
}
