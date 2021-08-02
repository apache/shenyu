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

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.datatype.XMLGregorianCalendar;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;

/**
 * The interface Meta data transfer.
 */
public enum MetaDataTransfer {

    /**
     * The constant INSTANCE.
     */
    INSTANCE;

    private final DatatypeFactory datatypeFactory;

    MetaDataTransfer() {
        try {
            datatypeFactory = DatatypeFactory.newInstance();
        } catch (DatatypeConfigurationException ex) {
            throw new RuntimeException(ex);
        }
    }

    /**
     * Map to entity meta data do.
     *
     * @param metaDataDTO the meta data dto
     * @return the meta data do
     */
    public MetaDataDO mapToEntity(final MetaDataDTO metaDataDTO) {
        if (metaDataDTO == null) {
            return null;
        }

        MetaDataDO.MetaDataDOBuilder<?, ?> metaDataDO = MetaDataDO.builder();

        metaDataDO.id(metaDataDTO.getId());
        metaDataDO.appName(metaDataDTO.getAppName());
        metaDataDO.path(metaDataDTO.getPath());
        metaDataDO.pathDesc(metaDataDTO.getPathDesc());
        metaDataDO.rpcType(metaDataDTO.getRpcType());
        metaDataDO.serviceName(metaDataDTO.getServiceName());
        metaDataDO.methodName(metaDataDTO.getMethodName());
        metaDataDO.parameterTypes(metaDataDTO.getParameterTypes());
        metaDataDO.rpcExt(metaDataDTO.getRpcExt());
        metaDataDO.enabled(metaDataDTO.getEnabled());

        return metaDataDO.build();
    }

    /**
     * Map to entity meta data do.
     *
     * @param metaDataDTO the meta data dto
     * @return the meta data do
     */
    public MetaDataDO mapRegisterDTOToEntity(final MetaDataRegisterDTO metaDataDTO) {
        if (metaDataDTO == null) {
            return null;
        }

        MetaDataDO.MetaDataDOBuilder<?, ?> metaDataDO = MetaDataDO.builder();

        metaDataDO.appName(metaDataDTO.getAppName());
        metaDataDO.path(metaDataDTO.getPath());
        metaDataDO.pathDesc(metaDataDTO.getPathDesc());
        metaDataDO.rpcType(metaDataDTO.getRpcType());
        metaDataDO.serviceName(metaDataDTO.getServiceName());
        metaDataDO.methodName(metaDataDTO.getMethodName());
        metaDataDO.parameterTypes(metaDataDTO.getParameterTypes());
        metaDataDO.rpcExt(metaDataDTO.getRpcExt());
        metaDataDO.enabled(metaDataDTO.isEnabled());

        return metaDataDO.build();
    }

    /**
     * Map to data meta data.
     *
     * @param metaDataDTO the meta data dto
     * @return the meta data
     */
    public MetaData mapToData(final MetaDataDTO metaDataDTO) {
        if (metaDataDTO == null) {
            return null;
        }

        MetaData.MetaDataBuilder metaData = MetaData.builder();

        metaData.id(metaDataDTO.getId());
        metaData.appName(metaDataDTO.getAppName());
        metaData.contextPath(metaDataDTO.getContextPath());
        metaData.path(metaDataDTO.getPath());
        metaData.rpcType(metaDataDTO.getRpcType());
        metaData.serviceName(metaDataDTO.getServiceName());
        metaData.methodName(metaDataDTO.getMethodName());
        metaData.parameterTypes(metaDataDTO.getParameterTypes());
        metaData.rpcExt(metaDataDTO.getRpcExt());
        metaData.enabled(metaDataDTO.getEnabled());

        return metaData.build();
    }

    /**
     * Map to data meta data.
     *
     * @param metaDataDO the meta data dto
     * @return the meta data
     */
    public MetaData mapToData(final MetaDataDO metaDataDO) {
        if (metaDataDO == null) {
            return null;
        }

        MetaData.MetaDataBuilder metaData = MetaData.builder();

        metaData.id(metaDataDO.getId());
        metaData.appName(metaDataDO.getAppName());
        metaData.path(metaDataDO.getPath());
        metaData.rpcType(metaDataDO.getRpcType());
        metaData.serviceName(metaDataDO.getServiceName());
        metaData.methodName(metaDataDO.getMethodName());
        metaData.parameterTypes(metaDataDO.getParameterTypes());
        metaData.rpcExt(metaDataDO.getRpcExt());
        metaData.enabled(metaDataDO.getEnabled());

        return metaData.build();
    }

    /**
     * Map to data all list.
     *
     * @param metaDataDOList the meta data do list
     * @return the list
     */
    public List<MetaData> mapToDataAll(final List<MetaDataDO> metaDataDOList) {
        if (metaDataDOList == null) {
            return null;
        }

        List<MetaData> list = new ArrayList<MetaData>(metaDataDOList.size());
        for (MetaDataDO metaDataDO : metaDataDOList) {
            list.add(mapToData(metaDataDO));
        }

        return list;
    }

    /**
     * Map to vo meta data vo.
     *
     * @param metaDataDO the meta data do
     * @return the meta data vo
     */
    public MetaDataVO mapToVO(final MetaDataDO metaDataDO) {
        if (metaDataDO == null) {
            return null;
        }

        MetaDataVO metaDataVO = new MetaDataVO();

        metaDataVO.setAppName(metaDataDO.getAppName());
        metaDataVO.setPath(metaDataDO.getPath());
        metaDataVO.setPathDesc(metaDataDO.getPathDesc());
        metaDataVO.setRpcType(metaDataDO.getRpcType());
        metaDataVO.setServiceName(metaDataDO.getServiceName());
        metaDataVO.setMethodName(metaDataDO.getMethodName());
        metaDataVO.setParameterTypes(metaDataDO.getParameterTypes());
        metaDataVO.setRpcExt(metaDataDO.getRpcExt());
        metaDataVO.setId(metaDataDO.getId());
        metaDataVO.setDateCreated(xmlGregorianCalendarToString(dateToXmlGregorianCalendar(metaDataDO.getDateCreated()), null));
        metaDataVO.setDateUpdated(xmlGregorianCalendarToString(dateToXmlGregorianCalendar(metaDataDO.getDateUpdated()), null));
        metaDataVO.setEnabled(metaDataDO.getEnabled());

        return metaDataVO;
    }

    /**
     * Map to vo list list.
     *
     * @param metaDataDOList the meta data do list
     * @return the list
     */
    public List<MetaDataVO> mapToVOList(final List<MetaDataDO> metaDataDOList) {
        if (metaDataDOList == null) {
            return null;
        }

        List<MetaDataVO> list = new ArrayList<>(metaDataDOList.size());
        for (MetaDataDO metaDataDO : metaDataDOList) {
            list.add(mapToVO(metaDataDO));
        }

        return list;
    }

    private String xmlGregorianCalendarToString(final XMLGregorianCalendar xcal, final String dateFormat) {
        if (xcal == null) {
            return null;
        }

        if (dateFormat == null) {
            return xcal.toString();
        } else {
            Date d = xcal.toGregorianCalendar().getTime();
            SimpleDateFormat sdf = new SimpleDateFormat(dateFormat);
            return sdf.format(d);
        }
    }

    private XMLGregorianCalendar dateToXmlGregorianCalendar(final Date date) {
        if (date == null) {
            return null;
        }

        GregorianCalendar c = new GregorianCalendar();
        c.setTime(date);
        return datatypeFactory.newXMLGregorianCalendar(c);
    }

}
