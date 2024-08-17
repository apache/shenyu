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

package org.apache.shenyu.discovery.zookeeper;

import org.apache.curator.CuratorZookeeperClient;
import org.apache.curator.framework.CuratorFramework;
import org.apache.curator.framework.WatcherRemoveCuratorFramework;
import org.apache.curator.framework.api.ACLBackgroundPathAndBytesable;
import org.apache.curator.framework.api.CreateBuilder;
import org.apache.curator.framework.api.CreateBuilder2;
import org.apache.curator.framework.api.GetChildrenBuilder;
import org.apache.curator.framework.api.ProtectACLCreateModeStatPathAndBytesable;
import org.apache.curator.framework.imps.ExistsBuilderImpl;
import org.apache.curator.framework.listen.Listenable;
import org.apache.curator.framework.recipes.cache.TreeCache;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.shenyu.discovery.api.config.DiscoveryConfig;
import org.apache.shenyu.discovery.api.listener.DataChangedEventListener;
import org.apache.zookeeper.data.Stat;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.TimeUnit;

import static org.junit.Assert.assertThrows;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

class ZookeeperDiscoveryServiceTest {

    private ZookeeperDiscoveryService zookeeperDiscoveryServiceUnderTest;

    private CuratorFramework curatorFramework;

    @BeforeEach
    void setUp() throws Exception {
        zookeeperDiscoveryServiceUnderTest = new ZookeeperDiscoveryService();
        curatorFramework = mock(CuratorFramework.class);
        DiscoveryConfig discoveryConfig = new DiscoveryConfig();
        discoveryConfig.setServerList("localhost:2181");
        discoveryConfig.setProps(new Properties());
        curatorFramework.start();
        curatorFramework.blockUntilConnected(30, TimeUnit.SECONDS);
        setField(zookeeperDiscoveryServiceUnderTest.getClass(), "client", curatorFramework);

    }

    @AfterEach
    void downTest() {
        zookeeperDiscoveryServiceUnderTest.shutdown();
        verify(curatorFramework).close();
    }

    private <T> void setField(final Class<T> clazz, final String fieldName, final Object value) throws NoSuchFieldException, IllegalAccessException {
        Field field = clazz.getDeclaredField(fieldName);
        field.setAccessible(true);
        field.set(zookeeperDiscoveryServiceUnderTest, value);
        field.setAccessible(false);
    }

    @Test
    void testExists() throws Exception {
        assertThrows(ShenyuException.class, () -> zookeeperDiscoveryServiceUnderTest.exists("key"));
        ExistsBuilderImpl existsBuilder = mock(ExistsBuilderImpl.class);
        when(curatorFramework.checkExists()).thenReturn(existsBuilder);
        when(existsBuilder.forPath(anyString())).thenReturn(mock(Stat.class));
        final Boolean result = zookeeperDiscoveryServiceUnderTest.exists("key");
        Assertions.assertTrue(result);
    }

    @Test
    void registerTest() throws Exception {
        assertThrows(ShenyuException.class, () ->
                zookeeperDiscoveryServiceUnderTest.register("/test", "hello"));
        CreateBuilder createBuilder = mock(CreateBuilder.class);
        when(curatorFramework.create()).thenReturn(createBuilder);
        CreateBuilder2 createBuilder2 = mock(CreateBuilder2.class);
        when(createBuilder.orSetData()).thenReturn(createBuilder2);
        ProtectACLCreateModeStatPathAndBytesable protectACLCreateModeStatPathAndBytesable = mock(ProtectACLCreateModeStatPathAndBytesable.class);
        when(createBuilder2.creatingParentsIfNeeded()).thenReturn(protectACLCreateModeStatPathAndBytesable);
        ACLBackgroundPathAndBytesable pathAndBytesable = mock(ACLBackgroundPathAndBytesable.class);
        when(protectACLCreateModeStatPathAndBytesable.withMode(any())).thenReturn(pathAndBytesable);
        when(pathAndBytesable.forPath(anyString(), any(byte[].class))).thenReturn(null);
        zookeeperDiscoveryServiceUnderTest.register("/test", "hello");
    }

    @Test
    void getRegisterDataTest() throws Exception {
        assertThrows(ShenyuException.class, () -> zookeeperDiscoveryServiceUnderTest.getRegisterData("/test"));
        GetChildrenBuilder getChildrenBuilder = mock(GetChildrenBuilder.class);
        when(curatorFramework.getChildren()).thenReturn(getChildrenBuilder);
        when(getChildrenBuilder.forPath(anyString())).thenReturn(new ArrayList<>());
        List<String> children = zookeeperDiscoveryServiceUnderTest.getRegisterData("/test");
        Assertions.assertEquals(0, children.size());
    }

    @Test
    void testWatch() throws NoSuchFieldException, IllegalAccessException, InvocationTargetException {
        final DataChangedEventListener mockListener = mock(DataChangedEventListener.class);
        CuratorZookeeperClient curatorZookeeperClient = mock(CuratorZookeeperClient.class);
        WatcherRemoveCuratorFramework watcherRemoveCuratorFramework = mock(WatcherRemoveCuratorFramework.class);
        when(curatorFramework.newWatcherRemoveCuratorFramework()).thenReturn(watcherRemoveCuratorFramework);
        when(curatorFramework.getZookeeperClient()).thenReturn(curatorZookeeperClient);
        when(curatorZookeeperClient.isConnected()).thenReturn(true);
        final Listenable listenable = mock(Listenable.class);
        when(watcherRemoveCuratorFramework.getConnectionStateListenable()).thenReturn(listenable);
        when(watcherRemoveCuratorFramework.getZookeeperClient()).thenReturn(mock(CuratorZookeeperClient.class));
        zookeeperDiscoveryServiceUnderTest.watch("/key", mockListener);
        Field cacheField = zookeeperDiscoveryServiceUnderTest.getClass().getDeclaredField("cacheMap");
        cacheField.setAccessible(true);
        Map<String, TreeCache> cacheMap = (Map<String, TreeCache>) cacheField.get(zookeeperDiscoveryServiceUnderTest);
        Assertions.assertNotNull(cacheMap.get("/key"));
    }

    @Test
    void testUnwatch() throws NoSuchFieldException, IllegalAccessException {
        zookeeperDiscoveryServiceUnderTest.unwatch("/key");
        Field cacheField = zookeeperDiscoveryServiceUnderTest.getClass().getDeclaredField("cacheMap");
        cacheField.setAccessible(true);
        Map<String, TreeCache> cacheMap = (Map<String, TreeCache>) cacheField.get(zookeeperDiscoveryServiceUnderTest);
        Assertions.assertNull(cacheMap.get("/key"));
    }
}
