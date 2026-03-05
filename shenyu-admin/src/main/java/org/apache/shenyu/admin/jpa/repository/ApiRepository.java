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

import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.validation.ExistProvider;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.io.Serializable;

@Repository
public interface ApiRepository extends JpaRepository<ApiDO, String>, ExistProvider {

    @Override
    default Boolean existed(Serializable key) {
        return existsById((String) key);
    }

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("""
            UPDATE ApiDO a SET
            a.contextPath = COALESCE(:#{#entity.contextPath}, a.contextPath),
            a.apiPath = COALESCE(:#{#entity.apiPath}, a.apiPath),
            a.httpMethod = COALESCE(:#{#entity.httpMethod}, a.httpMethod),
            a.consume = COALESCE(:#{#entity.consume}, a.consume),
            a.produce = COALESCE(:#{#entity.produce}, a.produce),
            a.version = COALESCE(:#{#entity.version}, a.version),
            a.rpcType = COALESCE(:#{#entity.rpcType}, a.rpcType),
            a.state = COALESCE(:#{#entity.state}, a.state),
            a.ext = COALESCE(:#{#entity.ext}, a.ext),
            a.apiOwner = COALESCE(:#{#entity.apiOwner}, a.apiOwner),
            a.apiDesc = COALESCE(:#{#entity.apiDesc}, a.apiDesc),
            a.apiSource = COALESCE(:#{#entity.apiSource}, a.apiSource),
            a.document = COALESCE(:#{#entity.document}, a.document),
            a.documentMd5 = COALESCE(:#{#entity.documentMd5}, a.documentMd5),
            a.dateUpdated = CURRENT_TIMESTAMP
            WHERE a.id = :#{#entity.id}
            """)
    int updateSelective(ApiDO entity);

    /**
     * updateOfflineByContextPath.
     * @param contextPath context path
     */
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("UPDATE ApiDO a SET a.state = 2, a.dateUpdated = CURRENT_TIMESTAMP WHERE a.contextPath = :contextPath")
    void updateOfflineByContextPath(@Param("contextPath") String contextPath);
}
