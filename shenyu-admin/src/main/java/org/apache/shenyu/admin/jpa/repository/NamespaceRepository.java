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

import org.apache.shenyu.admin.model.entity.NamespaceDO;
import org.apache.shenyu.admin.validation.ExistProvider;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.io.Serializable;
import java.util.Optional;

/**
 * The interface Namespace repository.
 */
@Repository
public interface NamespaceRepository extends JpaRepository<NamespaceDO, String>, ExistProvider {

    @Override
    default Boolean existed(Serializable key) {
        return existsByNamespaceId((String) key);
    }

    /**
     * Check if exists by namespace id.
     *
     * @param namespaceId the namespace id
     * @return true if exists
     */
    boolean existsByNamespaceId(String namespaceId);

    /**
     * Find by namespace id.
     *
     * @param namespaceId the namespace id
     * @return the optional
     */
    Optional<NamespaceDO> findByNamespaceId(String namespaceId);


    /**
     * Update selective.
     *
     * @param entity the entity
     * @return the int
     */
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("""
            UPDATE NamespaceDO n SET
            n.namespaceId = COALESCE(:#{#entity.namespaceId}, n.namespaceId),
            n.name = COALESCE(:#{#entity.name}, n.name),
            n.description = COALESCE(:#{#entity.description}, n.description),
            n.dateUpdated = CURRENT_TIMESTAMP
            WHERE n.id = :#{#entity.id}
            """)
    int updateSelective(@Param("entity") NamespaceDO entity);
}
