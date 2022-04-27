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

package org.apache.shenyu.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.DataPermissionDO;

import java.util.List;


/**
 * DataPermissionMapper.
 */
@Mapper
public interface DataPermissionMapper {
    
    /**
     * user has permission.
     *
     * @param userId userId
     * @return has permission,if not has permission the return null.
     */
    Boolean existed(String userId);

    /**
     * get list of {@link DataPermissionDO} by user id and data type.
     *
     * @param userId user id
     * @return list of {@link DataPermissionDO}
     */
    List<DataPermissionDO> listByUserId(String userId);

    /**
     * deleteSelector data permission by user id and data id.
     * @param dataId data id
     * @param userId user id
     * @param dataType data type
     * @return effect rows count
     */
    int deleteByUniqueKey(@Param("dataId") String dataId,
                          @Param("userId") String userId,
                          @Param("dataType") Integer dataType);

    /**
     * deleteSelector data permission by user id.
     * @param userId user id
     * @return int
     */
    int deleteByUserId(String userId);

    /**
     * delete permission data by ids of users.
     * @param userIdList ids of users
     * @return the count of deleted
     */
    int deleteByUserIdList(@Param("userIdList") List<String> userIdList);

    /**
     * deleteSelector data permission by data id.
     * @param dataId data id
     * @return int
     */
    int deleteByDataId(String dataId);

    /**
     * deleteSelector data permission by data ids.
     * @param dataIdList data ids
     * @return int
     */
    int deleteByDataIdList(@Param("dataIdList") List<String> dataIdList);

    /**
     * deleteSelector by list of data ids and user id.
     * @param dataIdsList data ids list
     * @param userId user id
     * @param dataType data type
     * @return int
     */
    int deleteByDataIdsAndUserId(@Param("list") List<String> dataIdsList,
                                 @Param("userId") String userId,
                                 @Param("dataType") Integer dataType);

    /**
     * insert data permission.
     * @param dataPermissionDO {@linkplain DataPermissionDO}
     * @return int
     */
    int insertSelective(DataPermissionDO dataPermissionDO);

    /**
     * batch insert data permission.
     * @param dataPermissionList list of data permission
     * @return the count of inserted
     */
    int insertBatch(@Param("dataPermissionList") List<DataPermissionDO> dataPermissionList);

    /**
     * select data ids via list of data id and user id.
     * @param dataIds selector or rule id
     * @param userId user id
     * @param dataType data type
     * @return {@linkplain List}
     */
    List<String> selectDataIds(@Param("list") List<String> dataIds,
                               @Param("userId") String userId,
                               @Param("dataType") Integer dataType);

    /**
     * find one by unique key.
     * @param dataId data id
     * @param userId user id
     * @param dataType data type
     * @return {@linkplain DataPermissionDO}
     */
    DataPermissionDO findOneByUniqueKey(@Param("dataId") String dataId,
                                        @Param("userId") String userId,
                                        @Param("dataType") Integer dataType);
}
