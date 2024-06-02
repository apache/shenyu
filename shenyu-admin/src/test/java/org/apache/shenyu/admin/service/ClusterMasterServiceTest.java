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
import org.apache.shenyu.admin.mapper.ClusterMasterMapper;
import org.apache.shenyu.admin.model.dto.ClusterMasterDTO;
import org.apache.shenyu.admin.model.entity.ClusterMasterDO;
import org.apache.shenyu.admin.service.impl.ClusterMasterServiceImpl;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

/**
 * Test cases for ClusterMasterService.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public final class ClusterMasterServiceTest {
    
    private static final String CONTEXT_PATH = "/test";
    
    private static final String PORT = "8080";
    
    private static final String HOST = "127.0.0.1";
    
    @InjectMocks
    private ClusterMasterServiceImpl clusterMasterService;
    
    @Mock
    private ClusterProperties clusterProperties;
    
    @Mock
    private ClusterMasterMapper clusterMasterMapper;
    
    @BeforeEach
    public void setUp() {
        clusterMasterService = new ClusterMasterServiceImpl(clusterProperties, clusterMasterMapper);
    }
    
    @Test
    void testSetMaster() {
        given(clusterMasterMapper.insert(any())).willReturn(1);
        
        clusterMasterService.setMaster(HOST, PORT, CONTEXT_PATH);
        
        verify(clusterMasterMapper, times(1)).insert(any());
    }
    
    @Test
    void testCheckMasterSuccess() {
        given(clusterMasterMapper.count(any())).willReturn(1L);
        
        boolean isMaster = clusterMasterService.isMaster(HOST, PORT, CONTEXT_PATH);
        
        assertTrue(isMaster);
    }
    
    @Test
    void testCheckMasterFail() {
        given(clusterMasterMapper.count(any())).willReturn(0L);
        
        boolean isMaster = clusterMasterService.isMaster(HOST, PORT, CONTEXT_PATH);
        
        assertFalse(isMaster);
    }
    
    @Test
    void testGetMaster() {
        
        ClusterMasterDO clusterMasterDO = buildClusterMasterDO();
        
        given(clusterMasterMapper.selectById(any())).willReturn(clusterMasterDO);
        
        ClusterMasterDTO actual = clusterMasterService.getMaster();
        
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
