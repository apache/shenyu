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

package org.apache.shenyu.admin.jpa.repository;

import org.apache.shenyu.admin.model.entity.AuthPathDO;
import org.apache.shenyu.admin.validation.ExistProvider;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.io.Serializable;
import java.util.List;

/**
 * The interface Auth path repository.
 */
@Repository
public interface AuthPathRepository extends JpaRepository<AuthPathDO, String>, ExistProvider {

    @Override
    default Boolean existed(Serializable key) {
        return existsById((String) key);
    }

    /**
     * Check if exists by auth id.
     *
     * @param authId the auth id
     * @return true if exists
     */
    boolean existsByAuthId(String authId);

    /**
     * Find by auth id.
     *
     * @param authId the auth id
     * @return the list
     */
    List<AuthPathDO> findByAuthId(String authId);

    /**
     * Find by auth id and app name.
     *
     * @param authId  the auth id
     * @param appName the app name
     * @return the list
     */
    List<AuthPathDO> findByAuthIdAndAppName(String authId, String appName);

    /**
     * Delete by auth id and app name.
     *
     * @param authId  the auth id
     * @param appName the app name
     * @return the deleted row count
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM AuthPathDO a WHERE a.authId = :authId AND a.appName = :appName")
    int deleteByAuthIdAndAppName(@Param("authId") String authId, @Param("appName") String appName);

    /**
     * Delete by auth id.
     *
     * @param authId the auth id
     * @return the deleted row count
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM AuthPathDO a WHERE a.authId = :authId")
    int deleteByAuthId(@Param("authId") String authId);

    /**
     * Delete by auth ids.
     *
     * @param authIds the auth ids
     * @return the deleted row count
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM AuthPathDO a WHERE a.authId IN :authIds")
    int deleteByAuthIds(@Param("authIds") List<String> authIds);

    /**
     * Find by auth id list.
     *
     * @param authIdList the auth id list
     * @return the list
     */
    List<AuthPathDO> findByAuthIdIn(@Param("authIdList") List<String> authIdList);
}
