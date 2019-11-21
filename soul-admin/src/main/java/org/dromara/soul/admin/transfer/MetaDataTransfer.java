/*
 *
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * Contributor license agreements.See the NOTICE file distributed with
 * This work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * he License.You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package org.dromara.soul.admin.transfer;


import org.dromara.soul.admin.dto.MetaDataDTO;
import org.dromara.soul.admin.entity.MetaDataDO;
import org.dromara.soul.admin.vo.MetaDataVO;
import org.dromara.soul.common.dto.MetaData;
import org.mapstruct.Mapper;
import org.mapstruct.factory.Mappers;

import java.util.List;

/**
 * The interface Meta data transfer.
 *
 * @author xiaoyu(Myth)
 */
@Mapper
public interface MetaDataTransfer {

    /**
     * The constant INSTANCE.
     */
    MetaDataTransfer INSTANCE = Mappers.getMapper(MetaDataTransfer.class);


    /**
     * Map to entity meta data do.
     *
     * @param metaDataDTO the meta data dto
     * @return the meta data do
     */
    MetaDataDO mapToEntity(MetaDataDTO metaDataDTO);


    /**
     * Map to data meta data.
     *
     * @param metaDataDTO the meta data dto
     * @return the meta data
     */
    MetaData mapToData(MetaDataDTO metaDataDTO);


    /**
     * Map to data meta data.
     *
     * @param metaDataDO the meta data dto
     * @return the meta data
     */
    MetaData mapToData(MetaDataDO metaDataDO);

    /**
     * Map to data all list.
     *
     * @param metaDataDOList the meta data do list
     * @return the list
     */
    List<MetaData> mapToDataAll(List<MetaDataDO> metaDataDOList);


    /**
     * Map to vo meta data vo.
     *
     * @param metaDataDO the meta data do
     * @return the meta data vo
     */
    MetaDataVO mapToVO(MetaDataDO metaDataDO);

    /**
     * Map to vo list list.
     *
     * @param metaDataDOList the meta data do list
     * @return the list
     */
    List<MetaDataVO> mapToVOList(List<MetaDataDO> metaDataDOList);


}
