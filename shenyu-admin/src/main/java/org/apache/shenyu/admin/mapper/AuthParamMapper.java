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
import org.apache.shenyu.admin.model.entity.AuthParamDO;

import java.util.List;

/**
 * The interface Auth param mapper.
 */
@Mapper
public interface AuthParamMapper {

    /**
     * Save int.
     *
     * @param authParamDO the auth param do
     * @return the int
     */
    int save(AuthParamDO authParamDO);

    /**
     * Batch save int.
     *
     * @param authParamDOList the auth param do list
     * @return the int
     */
    int batchSave(@Param("authParamDOList") List<AuthParamDO> authParamDOList);

    /**
     * Update int.
     *
     * @param authParamDO the auth param do
     * @return the int
     */
    int update(AuthParamDO authParamDO);

    /**
     * Find by auth id list.
     *
     * @param authId the auth id
     * @return the list
     */
    List<AuthParamDO> findByAuthId(String authId);

    /**
     * Find all the {@link AuthParamDO} by authIdList.
     *
     * @param authIdList  batch auth id
     * @return the list
     */
    List<AuthParamDO> findByAuthIdList(@Param("authIdList") List<String> authIdList);

    /**
     * Find by auth id and app name auth param do.
     *
     * @param authId  the auth id
     * @param appName the app name
     * @return the auth param do
     */
    AuthParamDO findByAuthIdAndAppName(@Param("authId") String authId, @Param("appName") String appName);

    /**
     * Delete by auth id int.
     *
     * @param authId the auth id
     * @return the int
     */
    int deleteByAuthId(String authId);

    /**
     * Delete by auth id int.
     *
     * @param authIds the auth ids
     * @return the int
     */
    int deleteByAuthIds(List<String> authIds);
}
