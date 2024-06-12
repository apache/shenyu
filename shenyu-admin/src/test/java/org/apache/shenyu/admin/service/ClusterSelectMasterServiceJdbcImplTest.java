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

import org.apache.shenyu.admin.config.properties.ClusterProperties;
import org.apache.shenyu.admin.mode.cluster.impl.jdbc.ClusterSelectMasterServiceJdbcImpl;
import org.apache.shenyu.admin.mode.cluster.impl.jdbc.mapper.ClusterMasterMapper;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.model.entity.ClusterMasterDO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.integration.jdbc.lock.JdbcLockRegistry;

import java.lang.reflect.Field;
import java.util.concurrent.locks.Lock;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test cases for ClusterSelectMasterServiceJdbcImpl.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ClusterSelectMasterServiceJdbcImplTest {
    
    private static final String CONTEXT_PATH = "/test";
    
    private static final String PORT = "8080";
    
    private static final String HOST = "127.0.0.1";
    
    @InjectMocks
    private ClusterSelectMasterServiceJdbcImpl clusterSelectMasterServiceJdbc;
    
    @Mock
    private ClusterProperties clusterProperties;
    
    @Mock
    private JdbcLockRegistry jdbcLockRegistry;
    
    @Mock
    private Lock clusterMasterLock;
    
    @Mock
    private ClusterMasterMapper clusterMasterMapper;
    
    @BeforeEach
    public void setUp() throws NoSuchFieldException, IllegalAccessException {
        clusterSelectMasterServiceJdbc = new ClusterSelectMasterServiceJdbcImpl(clusterProperties, jdbcLockRegistry, clusterMasterMapper);
        given(clusterMasterLock.tryLock()).willReturn(true);
        Field clusterMasterLockField = ClusterSelectMasterServiceJdbcImpl.class.getDeclaredField("clusterMasterLock");
        clusterMasterLockField.setAccessible(true);
        clusterMasterLockField.set(clusterSelectMasterServiceJdbc, clusterMasterLock);
    }
    
    @Test
    void testSetMaster() {
        
        given(clusterMasterMapper.insert(any())).willReturn(1);
        
        clusterSelectMasterServiceJdbc.selectMaster(HOST, PORT, CONTEXT_PATH);
        
        verify(clusterMasterMapper, times(1)).insert(any());
    }
    
    @Test
    void testGetMaster() {
        
        ClusterMasterDO clusterMasterDO = buildClusterMasterDO();
        
        given(clusterMasterMapper.selectById(any())).willReturn(clusterMasterDO);
        
        ClusterMasterDTO actual = clusterSelectMasterServiceJdbc.getMaster();
        
        assertEquals(HOST, actual.getMasterHost());
        assertEquals(PORT, actual.getMasterPort());
        assertEquals(CONTEXT_PATH, actual.getContextPath());
        
    }
    
    private ClusterMasterDO buildClusterMasterDO() {
        ClusterMasterDO clusterMasterDO = new ClusterMasterDO();
        clusterMasterDO.setId("1");
        clusterMasterDO.setMasterHost(HOST);
        clusterMasterDO.setMasterPort(PORT);
        clusterMasterDO.setContextPath(CONTEXT_PATH);
        return clusterMasterDO;
    }
}
