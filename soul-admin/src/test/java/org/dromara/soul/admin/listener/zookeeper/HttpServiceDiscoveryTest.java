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

package org.dromara.soul.admin.listener.zookeeper;

import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.curator.test.TestingServer;
import org.assertj.core.util.Arrays;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.common.dto.SelectorData;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.MockitoJUnitRunner;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.mock.env.MockEnvironment;

import static org.mockito.BDDMockito.given;

/**
 * Test cases for {@link HttpServiceDiscovery}.
 *
 * @author liMingLiang
 */
@SuppressWarnings("deprecation")
@RunWith(MockitoJUnitRunner.class)
public final class HttpServiceDiscoveryTest {

    private static final String ZK_URL = "127.0.0.1:21810";

    private static final int SLEEP_INTERVAL = 50;

    private static final String ROOT = "/soul/register";

    private static final String SERVER_NODE = "serverNodeTest";

    private static final String SUB_NODE_1 = "SubNode1Test";

    private static final String SUB_NODE_2 = "SubNode2Test";

    private static final String SUB_NODE_DATA = "SubNodeDataTest";

    private static MockEnvironment env;

    private TestingServer zkServer;

    @Mock
    private SelectorService selectorService;

    @Mock
    private SelectorMapper selectorMapper;

    @Mock
    private ApplicationEventPublisher eventPublisher;

    @BeforeClass
    public static void setUpBeforeClass() {
        env = new MockEnvironment();
    }

    @Before
    public void setUp() throws Exception {
        env.setProperty("soul.http.zookeeperUrl", ZK_URL);
        env.setProperty("soul.http.register", "true");
        given(selectorService.findByName(Mockito.anyString())).willReturn(new SelectorDO());
        given(selectorService.buildByName(Mockito.anyString())).willReturn(new SelectorData());
        zkServer = new TestingServer(21810, true);
    }

    @After
    public void tearDown() throws Exception {
        zkServer.close();
    }

    @Test
    public void testAfterPropertiesSet() throws InterruptedException {
        ZkClient zkClient = new ZkClient(ZK_URL);
        String pathChildNode1 = StringUtils.join(Arrays.array(ROOT, SERVER_NODE, SUB_NODE_1), '/');
        createSubNode(zkClient, pathChildNode1);

        HttpServiceDiscovery httpServiceDiscovery = new HttpServiceDiscovery(selectorService, selectorMapper, eventPublisher, env);
        httpServiceDiscovery.afterPropertiesSet();

        String pathChildNode2 = StringUtils.join(Arrays.array(ROOT, SERVER_NODE, SUB_NODE_2), '/');
        createSubNode(zkClient, pathChildNode2);
        sleep();

        String pathServerNode = StringUtils.join(Arrays.array(ROOT, SERVER_NODE), '/');
        zkClient.delete(pathChildNode1);
        zkClient.delete(pathChildNode2);
        zkClient.delete(pathServerNode);
        sleep();

        Assert.assertTrue(CollectionUtils.isEmpty(zkClient.getChildren(ROOT)));
    }

    @Test
    public void testAfterPropertiesSetForSelectorEmpty() {
        given(selectorService.findByName(Mockito.anyString())).willReturn(null);

        ZkClient zkClient = new ZkClient(ZK_URL);
        String pathChildNode1 = StringUtils.join(Arrays.array(ROOT, SERVER_NODE, SUB_NODE_1), '/');
        createSubNode(zkClient, pathChildNode1);

        HttpServiceDiscovery httpServiceDiscovery = new HttpServiceDiscovery(selectorService, selectorMapper, eventPublisher, env);
        httpServiceDiscovery.afterPropertiesSet();

        Assert.assertTrue(CollectionUtils.isNotEmpty(zkClient.getChildren(ROOT)));
    }

    @Test
    public void testAfterPropertiesSetForPathNotFound() throws InterruptedException {
        HttpServiceDiscovery httpServiceDiscovery = new HttpServiceDiscovery(selectorService, selectorMapper, eventPublisher, env);
        httpServiceDiscovery.afterPropertiesSet();
        ZkClient zkClient = new ZkClient(ZK_URL);
        String pathChildNode1 = StringUtils.join(Arrays.array(ROOT, SERVER_NODE, SUB_NODE_1), '/');
        createSubNode(zkClient, pathChildNode1);
        sleep();

        Assert.assertEquals(SUB_NODE_DATA, zkClient.readData(pathChildNode1));
    }

    @Test
    public void testAfterPropertiesSetForSkipInit() {
        env.setProperty("soul.http.register", "false");
        HttpServiceDiscovery httpServiceDiscovery = new HttpServiceDiscovery(selectorService, selectorMapper, eventPublisher, env);
        httpServiceDiscovery.afterPropertiesSet();

        Assert.assertNotNull(httpServiceDiscovery);
    }

    @Test
    public void testAfterPropertiesSetForZkDisabled() {
        env.setProperty("soul.http.zookeeperUrl", "");
        HttpServiceDiscovery httpServiceDiscovery = new HttpServiceDiscovery(selectorService, selectorMapper, eventPublisher, env);
        httpServiceDiscovery.afterPropertiesSet();

        Assert.assertNotNull(httpServiceDiscovery);
    }

    private void createSubNode(final ZkClient zkClient, final String pathChildNode) {
        zkClient.createPersistent(pathChildNode, true);
        zkClient.writeData(pathChildNode, SUB_NODE_DATA);
    }

    private void sleep() throws InterruptedException {
        Thread.sleep(SLEEP_INTERVAL);
    }
}
