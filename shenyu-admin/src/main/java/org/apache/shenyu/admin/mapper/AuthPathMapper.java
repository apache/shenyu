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
import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * The interface Auth path mapper.
 */
@Mapper
public interface AuthPathMapper extends ExistProvider {
    
    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);

    /**
     * existedByAuthId.
     *
     * @param authId authId
     * @return java.lang.Boolean
    */
    Boolean existedByAuthId(Serializable authId);

    /**
     * Save int.
     *
     * @param authPathDO the auth resource do
     * @return the int
     */
    int save(AuthPathDO authPathDO);
    
    /**
     * Batch save int.
     *
     * @param authPathDOList the auth path do list
     * @return the int
     */
    int batchSave(@Param("authPathDOList") List<AuthPathDO> authPathDOList);
    
    /**
     * Update int.
     *
     * @param authPathDO the auth resource do
     * @return the int
     */
    int update(AuthPathDO authPathDO);
    
    /**
     * Find by auth id list.
     *
     * @param authId the auth id
     * @return the list
     */
    List<AuthPathDO> findByAuthId(String authId);
    
    /**
     * find all the {@link AuthPathDO} by authIdList.
     *
     * @param authIdList batch auth id
     * @return the list
     */
    List<AuthPathDO> findByAuthIdList(@Param("authIdList") List<String> authIdList);
    
    /**
     * Find by auth id and app name list.
     *
     * @param authId  the auth id
     * @param appName the app name
     * @return the list
     */
    List<AuthPathDO> findByAuthIdAndAppName(@Param("authId") String authId, @Param("appName") String appName);
    
    
    /**
     * Delete by auth id and app name int.
     *
     * @param authId  the auth id
     * @param appName the app name
     * @return the int
     */
    int deleteByAuthIdAndAppName(@Param("authId") String authId, @Param("appName") String appName);
    
    /**
     * Delete by auth id int.
     *
     * @param authId the auth id
     * @return the int
     */
    int deleteByAuthId(@Param("authId") String authId);
    
    /**
     * Delete by auth id int.
     *
     * @param authIds the auth ids
     * @return the int
     */
    int deleteByAuthIds(List<String> authIds);
}
