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

package org.apache.shenyu.admin.transfer;

import org.apache.shenyu.admin.model.dto.MetaDataDTO;
import org.apache.shenyu.admin.model.entity.MetaDataDO;
import org.apache.shenyu.admin.model.vo.MetaDataVO;
import org.apache.shenyu.common.dto.MetaData;
import org.apache.shenyu.register.common.dto.MetaDataRegisterDTO;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.Collections;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

/**
 * test cast for {@link MetaDataTransfer}.
 */
public final class MetaDataTransferTest {

    private MetaDataDTO metaDataDTO;

    private MetaDataRegisterDTO metaDataRegisterDTO;

    @BeforeEach
    public void setUp() {
        metaDataDTO = new MetaDataDTO();
        metaDataDTO.setId("id123");
        metaDataDTO.setAppName("appName123");
        metaDataDTO.setPath("/app");
        metaDataDTO.setEnabled(true);
        metaDataDTO.setMethodName("methodName123");
        metaDataDTO.setContextPath("/http");
        metaDataDTO.setParameterTypes("String");
        metaDataDTO.setPathDesc("desc");
        metaDataDTO.setRpcType("http");
        metaDataDTO.setRpcExt("none");
        metaDataDTO.setRuleName("ruleName123");
        metaDataDTO.setServiceName("serviceName123");


        metaDataRegisterDTO = new MetaDataRegisterDTO();
        metaDataRegisterDTO.setAppName("appName123");
        metaDataRegisterDTO.setPath("/app");
        metaDataRegisterDTO.setEnabled(true);
        metaDataRegisterDTO.setMethodName("methodName123");
        metaDataRegisterDTO.setContextPath("/http");
        metaDataRegisterDTO.setParameterTypes("String");
        metaDataRegisterDTO.setPathDesc("desc");
        metaDataRegisterDTO.setRpcType("http");
        metaDataRegisterDTO.setRpcExt("none");
        metaDataRegisterDTO.setRuleName("ruleName123");
        metaDataRegisterDTO.setServiceName("serviceName123");
    }

    @Test
    public void testMapToEntity() {
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapToEntity(metaDataDTO);
        assertEquals("id123", metaDataDO.getId());
        assertEquals("appName123", metaDataDO.getAppName());
        assertEquals(true, metaDataDO.getEnabled());
        assertEquals("methodName123", metaDataDO.getMethodName());
        assertEquals("String", metaDataDO.getParameterTypes());
        assertEquals("desc", metaDataDO.getPathDesc());
        assertEquals("http", metaDataDO.getRpcType());
        assertEquals("none", metaDataDO.getRpcExt());
        assertEquals("serviceName123", metaDataDO.getServiceName());
    }

    @Test
    public void testMapRegisterDTOToEntity() {
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapRegisterDTOToEntity(metaDataRegisterDTO);
        assertNull(null, metaDataDO.getId());
        assertEquals("appName123", metaDataDO.getAppName());
        assertEquals(true, metaDataDO.getEnabled());
        assertEquals("methodName123", metaDataDO.getMethodName());
        assertEquals("String", metaDataDO.getParameterTypes());
        assertEquals("desc", metaDataDO.getPathDesc());
        assertEquals("http", metaDataDO.getRpcType());
        assertEquals("none", metaDataDO.getRpcExt());
        assertEquals("serviceName123", metaDataDO.getServiceName());
    }

    @Test
    public void testCopy() {
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapToEntity(metaDataDTO);
        MetaDataDO copied = MetaDataTransfer.INSTANCE.copy(metaDataDO);
        assertEquals("id123", copied.getId());
        assertEquals("appName123", copied.getAppName());
        assertEquals(true, copied.getEnabled());
        assertEquals("methodName123", copied.getMethodName());
        assertEquals("String", copied.getParameterTypes());
        assertEquals("desc", copied.getPathDesc());
        assertEquals("http", copied.getRpcType());
        assertEquals("none", copied.getRpcExt());
        assertEquals("serviceName123", copied.getServiceName());
    }

    @Test
    public void testMapToData() {
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapToEntity(metaDataDTO);
        MetaData metaData = MetaDataTransfer.INSTANCE.mapToData(metaDataDO);
        assertEquals("id123", metaData.getId());
        assertEquals("appName123", metaData.getAppName());
        assertEquals(true, metaData.getEnabled());
        assertEquals("methodName123", metaData.getMethodName());
        assertEquals("String", metaData.getParameterTypes());
        assertEquals("http", metaData.getRpcType());
        assertEquals("none", metaData.getRpcExt());
        assertEquals("serviceName123", metaData.getServiceName());
    }

    @Test
    public void testMapToDataAll() {
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapToEntity(metaDataDTO);
        List<MetaDataDO> metaDataDOList = Collections.singletonList(metaDataDO);
        List<MetaData> metaDataList = MetaDataTransfer.INSTANCE.mapToDataAll(metaDataDOList);

        assertEquals(metaDataDOList.size(), metaDataList.size());

        MetaData metaData = metaDataList.get(0);
        assertEquals("id123", metaData.getId());
        assertEquals("appName123", metaData.getAppName());
        assertEquals(true, metaData.getEnabled());
        assertEquals("methodName123", metaData.getMethodName());
        assertEquals("String", metaData.getParameterTypes());
        assertEquals("http", metaData.getRpcType());
        assertEquals("none", metaData.getRpcExt());
        assertEquals("serviceName123", metaData.getServiceName());
    }

    @Test
    public void testMapToVO() {
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapToEntity(metaDataDTO);
        MetaDataVO metaDataVO = MetaDataTransfer.INSTANCE.mapToVO(metaDataDO);
        assertEquals("id123", metaDataVO.getId());
        assertEquals("appName123", metaDataVO.getAppName());
        assertEquals(true, metaDataVO.getEnabled());
        assertEquals("methodName123", metaDataVO.getMethodName());
        assertEquals("String", metaDataVO.getParameterTypes());
        assertEquals("desc", metaDataVO.getPathDesc());
        assertEquals("http", metaDataVO.getRpcType());
        assertEquals("none", metaDataVO.getRpcExt());
        assertEquals("serviceName123", metaDataVO.getServiceName());
    }

    @Test
    public void testMapToVOList() {
        MetaDataDO metaDataDO = MetaDataTransfer.INSTANCE.mapToEntity(metaDataDTO);
        List<MetaDataDO> metaDataDOList = Collections.singletonList(metaDataDO);
        List<MetaDataVO> metaDataVOList = MetaDataTransfer.INSTANCE.mapToVOList(metaDataDOList);
        assertEquals(metaDataDOList.size(), metaDataVOList.size());
        MetaDataVO metaDataVO = metaDataVOList.get(0);
        assertEquals("id123", metaDataVO.getId());
        assertEquals("appName123", metaDataVO.getAppName());
        assertEquals(true, metaDataVO.getEnabled());
        assertEquals("methodName123", metaDataVO.getMethodName());
        assertEquals("String", metaDataVO.getParameterTypes());
        assertEquals("desc", metaDataVO.getPathDesc());
        assertEquals("http", metaDataVO.getRpcType());
        assertEquals("none", metaDataVO.getRpcExt());
        assertEquals("serviceName123", metaDataVO.getServiceName());
    }
}
