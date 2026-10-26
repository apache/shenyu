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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.impl.zookeeper.ClusterSelectMasterServiceZookeeperImpl;
import org.apache.shenyu.admin.mode.cluster.impl.zookeeper.ClusterZookeeperClient;
import org.apache.shenyu.common.exception.ShenyuException;
import org.apache.zookeeper.KeeperException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.integration.zookeeper.lock.ZookeeperLockRegistry;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.when;

/**
 * Test cases for ClusterSelectMasterServiceZookeeperImpl.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ClusterSelectMasterServiceZookeeperImplTest {

    private static final String MASTER_INFO = "/shenyu_cluster_lock/master/info";

    private static final String HOST = "127.0.0.1";

    private static final String PORT = "8080";

    private ClusterSelectMasterServiceZookeeperImpl zookeeperSelectMasterService;

    @Mock
    private ClusterProperties clusterProperties;

    @Mock
    private ZookeeperLockRegistry zookeeperLockRegistry;

    @Mock
    private ClusterZookeeperClient clusterZookeeperClient;

    @BeforeEach
    public void setUp() {
        zookeeperSelectMasterService = new ClusterSelectMasterServiceZookeeperImpl(clusterProperties, zookeeperLockRegistry, clusterZookeeperClient);
        given(clusterProperties.getSchema()).willReturn("http");
    }

    @Test
    public void testGetMasterUrlShouldNotThrowWhenMasterInfoNodeIsAbsent() {
        // a fresh cluster has no master info znode until the first selectMaster writes it
        given(clusterZookeeperClient.isExist(MASTER_INFO)).willReturn(false);
        when(clusterZookeeperClient.getDirectly(MASTER_INFO)).thenThrow(new ShenyuException(new KeeperException.NoNodeException()));

        assertEquals(StringUtils.EMPTY, zookeeperSelectMasterService.getMasterUrl());
    }

    @Test
    public void testGetMasterUrlShouldNotThrowWhenMasterInfoContentIsEmpty() {
        // the znode exists but carries no readable master info yet
        given(clusterZookeeperClient.isExist(MASTER_INFO)).willReturn(true);
        given(clusterZookeeperClient.getDirectly(MASTER_INFO)).willReturn(StringUtils.EMPTY);

        assertEquals(StringUtils.EMPTY, zookeeperSelectMasterService.getMasterUrl());
    }

    @Test
    public void testGetMasterUrlShouldNotDoubleTheSlashWhenContextPathStartsWithOne() {
        given(clusterZookeeperClient.isExist(MASTER_INFO)).willReturn(true);
        given(clusterZookeeperClient.getDirectly(MASTER_INFO)).willReturn(masterInfoJson("/admin"));

        assertEquals("http://" + HOST + ":" + PORT + "/admin", zookeeperSelectMasterService.getMasterUrl());
    }

    @Test
    public void testGetMasterUrlWithBareContextPath() {
        given(clusterZookeeperClient.isExist(MASTER_INFO)).willReturn(true);
        given(clusterZookeeperClient.getDirectly(MASTER_INFO)).willReturn(masterInfoJson("admin"));

        assertEquals("http://" + HOST + ":" + PORT + "/admin", zookeeperSelectMasterService.getMasterUrl());
    }

    @Test
    public void testGetMasterUrlWithoutContextPath() {
        given(clusterZookeeperClient.isExist(MASTER_INFO)).willReturn(true);
        given(clusterZookeeperClient.getDirectly(MASTER_INFO)).willReturn(masterInfoJson(StringUtils.EMPTY));

        assertEquals("http://" + HOST + ":" + PORT, zookeeperSelectMasterService.getMasterUrl());
    }

    private String masterInfoJson(final String contextPath) {
        return "{\"masterHost\":\"" + HOST + "\",\"masterPort\":\"" + PORT + "\",\"contextPath\":\"" + contextPath + "\"}";
    }
}
