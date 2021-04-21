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

package org.dromara.soul.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.dromara.soul.admin.model.entity.DataPermissionDO;

import java.util.List;


/**
 * DataPermissionMapper.
 *
 * @author kaitoShy(plutokaito)
 */
@Mapper
public interface DataPermissionMapper {

    /**
     * get list of {@link DataPermissionDO} by user id and data type.
     *
     * @param userId user id
     * @return list of {@link DataPermissionDO}
     */
    List<DataPermissionDO> listByUserIdAndDataType(String userId);

    /**
     * delete data permission by user id and data id.
     * @param userId user id
     * @param dataId data id
     * @return effect rows count
     */
    int deleteByDataIdAndUserId(String userId, String dataId);

    /**
     * delete by list of data ids and user id.
     * @param dataIdsList data ids list
     * @param userId user id
     * @return int
     */
    int deleteByDataIds(@Param("list") List<String> dataIdsList, @Param("userId") String userId);

    /**
     * insert data permission.
     * @param dataPermissionDO {@linkplain DataPermissionDO}
     * @return int
     */
    int insertSelective(DataPermissionDO dataPermissionDO);
}
