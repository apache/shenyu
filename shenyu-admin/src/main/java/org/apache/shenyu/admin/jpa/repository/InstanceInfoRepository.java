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

import org.apache.shenyu.admin.model.entity.InstanceInfoDO;
import org.apache.shenyu.admin.model.query.InstanceQuery;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * The interface Instance info repository.
 */
@Repository
public interface InstanceInfoRepository extends JpaRepository<InstanceInfoDO, String> {

    /**
     * Select one by query, mirrors MyBatis LIMIT 1 semantics via the caller's findFirst.
     *
     * @param query the {@linkplain InstanceQuery}
     * @return the matched instance info list
     */
    @Query("""
            SELECT i FROM InstanceInfoDO i
            WHERE (:#{#query.instanceId} IS NULL OR :#{#query.instanceId} = '' OR i.id = :#{#query.instanceId})
            AND (:#{#query.instanceIp} IS NULL OR :#{#query.instanceIp} = '' OR i.instanceIp = :#{#query.instanceIp})
            AND (:#{#query.namespaceId} IS NULL OR :#{#query.namespaceId} = '' OR i.namespaceId = :#{#query.namespaceId})
            AND (:#{#query.instancePort} IS NULL OR :#{#query.instancePort} = '' OR i.instancePort = :#{#query.instancePort})
            AND (:#{#query.instanceType} IS NULL OR :#{#query.instanceType} = '' OR i.instanceType = :#{#query.instanceType})
            """)
    List<InstanceInfoDO> selectOneByQuery(@Param("query") InstanceQuery query);

    /**
     * Select by query, namespaceId is a mandatory condition like the original SQL.
     *
     * @param query the {@linkplain InstanceQuery}
     * @return the instance info list
     */
    @Query("""
            SELECT i FROM InstanceInfoDO i
            WHERE i.namespaceId = :#{#query.namespaceId}
            AND (:#{#query.instanceIp} IS NULL OR :#{#query.instanceIp} = '' OR i.instanceIp LIKE CONCAT('%', :#{#query.instanceIp}, '%'))
            AND (:#{#query.instancePort} IS NULL OR :#{#query.instancePort} = '' OR i.instancePort = :#{#query.instancePort})
            AND (:#{#query.instanceType} IS NULL OR :#{#query.instanceType} = '' OR i.instanceType = :#{#query.instanceType})
            """)
    List<InstanceInfoDO> selectByQuery(@Param("query") InstanceQuery query);
}
