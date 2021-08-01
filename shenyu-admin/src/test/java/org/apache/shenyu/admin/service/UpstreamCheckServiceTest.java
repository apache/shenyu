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

package org.apache.shenyu.admin.service;

import com.google.common.collect.Lists;
import lombok.SneakyThrows;
import org.apache.shenyu.admin.model.entity.PluginDO;
import org.apache.shenyu.admin.model.entity.SelectorDO;
import org.apache.shenyu.admin.mapper.PluginMapper;
import org.apache.shenyu.admin.mapper.SelectorConditionMapper;
import org.apache.shenyu.admin.mapper.SelectorMapper;
import org.apache.shenyu.admin.service.impl.UpstreamCheckService;
import org.apache.shenyu.common.constant.Constants;
import org.apache.shenyu.common.dto.convert.DivideUpstream;
import org.apache.shenyu.common.dto.convert.ZombieUpstream;
import org.apache.shenyu.common.enums.PluginEnum;
import org.apache.shenyu.register.common.config.ShenyuRegisterCenterConfig;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.Mockito.when;

/**
 * Test cases for UpstreamCheckService.
 */
@RunWith(MockitoJUnitRunner.Silent.class)
public final class UpstreamCheckServiceTest {

    private static final String MOCK_SELECTOR_NAME = "mockSelectorName";

    private static final String MOCK_SELECTOR_NAME_2 = "mockSelectorName2";

    private static final String MOCK_SELECTOR_NAME_OTHER = "mockSelectorNameOther";

    private static final String MOCK_PLUGIN_ID = "mockPluginId";

    private volatile int port = -1;

    private UpstreamCheckService upstreamCheckService;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @Mock
    private PluginMapper pluginMapper;

    @Mock
    private SelectorConditionMapper selectorConditionMapper;

    private ShenyuRegisterCenterConfig shenyuRegisterCenterConfig = new ShenyuRegisterCenterConfig();

    private Map<String, List<DivideUpstream>> upstreamMap;

    private Set<ZombieUpstream> zombieSet;

    @Before
    public void setUp() {
        shenyuRegisterCenterConfig.setRegisterType("http");
        Properties properties = new Properties();
        properties.setProperty(Constants.IS_CHECKED, "true");
        shenyuRegisterCenterConfig.setProps(properties);

        //get static variable reference by reflection
        upstreamMap = (Map<String, List<DivideUpstream>>) ReflectionTestUtils.getField(UpstreamCheckService.class, "UPSTREAM_MAP");
        zombieSet = (Set<ZombieUpstream>) ReflectionTestUtils.getField(UpstreamCheckService.class, "ZOMBIE_SET");

        //mock data
        PluginDO pluginDO = PluginDO.builder()
                .name(PluginEnum.DIVIDE.getName())
                .id(MOCK_PLUGIN_ID)
                .build();
        SelectorDO selectorDOWithUrlError = SelectorDO.builder()
                .pluginId(MOCK_PLUGIN_ID)
                .name(MOCK_SELECTOR_NAME)
                .handle("[{\"upstreamHost\":\"localhost\",\"protocol\":\"http://\",\"upstreamUrl\":\"divide-upstream-50\",\"weight\":50}]")
                .build();
        SelectorDO selectorDOWithUrlReachable = SelectorDO.builder()
                .pluginId(MOCK_PLUGIN_ID)
                .name(MOCK_SELECTOR_NAME_OTHER)
                .handle("[{\"upstreamHost\":\"localhost\",\"protocol\":\"http://\",\"localhost\":\"divide-upstream-60\",\"weight\":60}]")
                .build();

        //stubbing
        when(pluginMapper.selectByNames(anyList())).thenReturn(Lists.newArrayList(pluginDO));
        when(pluginMapper.selectById(anyString())).thenReturn(pluginDO);
        when(selectorMapper.findByPluginId(anyString())).thenReturn(Lists.newArrayList(selectorDOWithUrlError, selectorDOWithUrlReachable));
        when(selectorMapper.selectByName(anyString())).then(invocationOnMock -> {
            Object[] args = invocationOnMock.getArguments();
            if (MOCK_SELECTOR_NAME.equals(args[0])) {
                return selectorDOWithUrlError;
            } else if (MOCK_SELECTOR_NAME_OTHER.equals(args[0])) {
                return selectorDOWithUrlReachable;
            }
            return null;
        });

        upstreamCheckService = new UpstreamCheckService(selectorMapper, eventPublisher, pluginMapper, selectorConditionMapper, shenyuRegisterCenterConfig);
    }

    @Test
    public void testRemoveByKey() {
        UpstreamCheckService.removeByKey(MOCK_SELECTOR_NAME);
        Assert.assertFalse(upstreamMap.containsKey(MOCK_SELECTOR_NAME));
    }

    @Test
    public void testSubmit() {
        // Test submit when selector name not exists
        testSubmitOnce();
        // Test submit when selector name exists
        testSubmitOnce();
    }

    private void testSubmitOnce() {
        final DivideUpstream divideUpstream = DivideUpstream.builder()
                .upstreamUrl("divide-upstream-60")
                .weight(60)
                .build();
        upstreamCheckService.submit(MOCK_SELECTOR_NAME_OTHER, divideUpstream);
        Assert.assertTrue(upstreamMap.containsKey(MOCK_SELECTOR_NAME_OTHER));
    }

    @Test
    public void testReplace() {
        final DivideUpstream divideUpstream = DivideUpstream.builder()
                .upstreamHost("localhost")
                .build();
        final DivideUpstream divideUpstream2 = DivideUpstream.builder()
                .upstreamHost("localhost2")
                .build();
        upstreamCheckService.submit(MOCK_SELECTOR_NAME_2, divideUpstream);
        upstreamCheckService.replace(MOCK_SELECTOR_NAME_2, Collections.singletonList(divideUpstream2));
        Assert.assertEquals(1, upstreamMap.get(MOCK_SELECTOR_NAME_2).size());
        Assert.assertEquals("localhost2", upstreamMap.get(MOCK_SELECTOR_NAME_2).get(0).getUpstreamHost());
    }

    @Test
    public void testScheduled() {
        Runnable runnable = () -> {
            ServerSocket serverSocket;
            try {
                serverSocket = new ServerSocket(0);
                port = serverSocket.getLocalPort();
                Socket socket = serverSocket.accept();
                socket.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        };
        new Thread(runnable).start();

        while (port == -1) {
            Thread.yield();
        }

        // Successfully checked url
        final DivideUpstream divideUpstream1 = DivideUpstream.builder()
                .upstreamHost("127.0.0.1")
                .upstreamUrl("127.0.0.1:" + port)
                .status(false)
                .build();
        final DivideUpstream divideUpstream2 = DivideUpstream.builder()
                .upstreamHost("127.0.0.1")
                .upstreamUrl("127.0.0.1:" + port)
                .status(false)
                .build();
        final DivideUpstream divideUpstream3 = DivideUpstream.builder()
                .upstreamHost("failed")
                .build();
        ZombieUpstream zombieUpstream1 = ZombieUpstream.builder()
                .divideUpstream(divideUpstream1)
                .zombieCheckTimes(5)
                .selectorName("test1")
                .build();
        ZombieUpstream zombieUpstream2 = ZombieUpstream.builder()
                .divideUpstream(divideUpstream3)
                .zombieCheckTimes(5)
                .selectorName("failedTest")
                .build();

        zombieSet.add(zombieUpstream1);
        zombieSet.add(zombieUpstream2);
        upstreamMap.put("test2", Collections.singletonList(divideUpstream2));
        ReflectionTestUtils.invokeMethod(upstreamCheckService, "scheduled");
        Assert.assertFalse(zombieSet.contains(zombieUpstream1));
        Assert.assertNotNull(upstreamMap.get("test1"));
        Assert.assertNotNull(upstreamMap.get("test2"));
    }

    @SneakyThrows
    @Test
    public void testUpdateHandler() {
        final DivideUpstream divideUpstream = DivideUpstream.builder()
                .upstreamHost("localhost")
                .build();
        List<DivideUpstream> upstreamList = Collections.emptyList();
        List<DivideUpstream> successList = Collections.singletonList(divideUpstream);
        ReflectionTestUtils.invokeMethod(upstreamCheckService, "updateHandler", "test", upstreamList, successList);
        ReflectionTestUtils.invokeMethod(upstreamCheckService, "updateHandler", "test", upstreamList, upstreamList);
    }
}
