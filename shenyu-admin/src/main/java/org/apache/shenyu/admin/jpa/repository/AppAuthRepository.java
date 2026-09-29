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

import org.apache.shenyu.admin.model.entity.AppAuthDO;
import org.apache.shenyu.admin.model.query.AppAuthQuery;
import org.apache.shenyu.admin.validation.ExistProvider;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

@Repository
public interface AppAuthRepository extends JpaRepository<AppAuthDO, String>, ExistProvider {

    @Override
    default Boolean existed(Serializable key) {
        return existsById((String) key);
    }

    @Query("""
            SELECT a
            FROM AppAuthDO a
            WHERE (:#{#query.namespaceId} IS NULL OR :#{#query.namespaceId} = '' OR a.namespaceId = :#{#query.namespaceId})
            AND (:#{#query.appKey} IS NULL OR :#{#query.appKey} = '' OR a.appKey = :#{#query.appKey})
            AND (:#{#query.phone} IS NULL OR :#{#query.phone} = '' OR a.phone = :#{#query.phone})
            """)
    Page<AppAuthDO> pageByQuery(@Param("query") AppAuthQuery query, Pageable pageable);

    Optional<AppAuthDO> findByAppKey(String appKey);

    List<AppAuthDO> findByNamespaceId(String namespaceId);

    List<AppAuthDO> findByNamespaceIdIn(Collection<String> namespaceIds);

    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("""
            UPDATE AppAuthDO a SET a.enabled = :enabled, a.dateUpdated = CURRENT_TIMESTAMP WHERE a.id IN :ids
            """)
    void updateEnableBatch(@Param("ids") List<String> ids, @Param("enabled") Boolean enabled);

    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("""
            UPDATE AppAuthDO a SET a.open = :open, a.dateUpdated = CURRENT_TIMESTAMP WHERE a.id IN :ids
            """)
    void updateOpenBatch(@Param("ids") List<String> ids, @Param("open") Boolean open);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("""
            UPDATE AppAuthDO a SET a.appSecret = :appSecret, a.dateUpdated = CURRENT_TIMESTAMP WHERE a.appKey = :appKey
            """)
    int updateAppSecretByAppKey(@Param("appKey") String appKey, @Param("appSecret") String appSecret);

    /**
     * Find by id and namespace id, both must match like the original update WHERE clause.
     *
     * @param id          the id
     * @param namespaceId the namespace id
     * @return the optional
     */
    Optional<AppAuthDO> findByIdAndNamespaceId(String id, String namespaceId);

    /**
     * Delete by ids.
     *
     * @param ids the ids
     * @return the deleted row count
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM AppAuthDO a WHERE a.id IN :ids")
    int deleteByIds(@Param("ids") List<String> ids);
}
