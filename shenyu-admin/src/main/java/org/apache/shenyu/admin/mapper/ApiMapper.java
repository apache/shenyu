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
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.model.query.ApiQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * ApiMapper.
 **/
@Mapper
public interface ApiMapper extends ExistProvider {
    /**
     * delete by primary key.
     *
     * @param id primaryKey
     * @return deleteCount
     */
    int deleteByPrimaryKey(String id);

    /**
     * insert record to table.
     *
     * @param record the record
     * @return insert count
     */
    int insert(ApiDO record);

    /**
     * insert record to table selective.
     *
     * @param record the record
     * @return insert count
     */
    int insertSelective(ApiDO record);

    /**
     * select by primary key.
     *
     * @param id primary key
     * @return object by primary key
     */
    ApiDO selectByPrimaryKey(String id);

    /**
     * update record selective.
     *
     * @param record the updated record
     * @return update count
     */
    int updateByPrimaryKeySelective(ApiDO record);

    /**
     * update record.
     *
     * @param record the updated record
     * @return update count
     */
    int updateByPrimaryKey(ApiDO record);

    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);

    /**
     * select api by query.
     *
     * @param query {@linkplain ApiQuery}
     * @return {@linkplain List}
     */
    List<ApiDO> selectByQuery(ApiQuery query);

    /**
     * select api by ids.
     * @param ids primary keys.
     * @return {@linkplain ApiDO}
     */
    List<ApiDO> selectByIds(List<String> ids);

    /**
     * delete api.
     *
     * @param ids primary keys.
     * @return rows int
     */
    int deleteByIds(List<String> ids);

    /**
     * selectByApiPathHttpMethodRpcType.
     * @param apiPath apiPath
     * @param httpMethod httpMethod
     * @param rpcType rpcType
     * @return {@linkplain ApiDO}
     */
    List<ApiDO> selectByApiPathHttpMethodRpcType(@Param(value = "apiPath") String apiPath, @Param(value = "httpMethod") Integer httpMethod, @Param(value = "rpcType") String rpcType);

    /**
     * updateOfflineByContextPath.
     * @param contextPath context path
     * @return update count
     */
    int updateOfflineByContextPath(@Param(value = "contextPath") String contextPath);

}
