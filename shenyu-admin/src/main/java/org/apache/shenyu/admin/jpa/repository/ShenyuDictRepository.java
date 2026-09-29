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

import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;
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

/**
 * ShenyuDictRepository.
 */
@Repository
public interface ShenyuDictRepository extends JpaRepository<ShenyuDictDO, String>, ExistProvider {

    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    default Boolean existed(Serializable id) {
        return existsById((String) id);
    }

    /**
     * Find by type.
     *
     * @param type the dict type
     * @return the list
     */
    List<ShenyuDictDO> findByType(String type);

    /**
     * Find by type batch, mirrors MyBatis findByTypeBatch.
     *
     * @param types the dict types
     * @return the list
     */
    List<ShenyuDictDO> findByTypeIn(Collection<String> types);

    /**
     * Find by dict code and dict name.
     *
     * @param dictCode the dict code
     * @param dictName the dict name
     * @return the optional {@linkplain ShenyuDictDO}
     */
    Optional<ShenyuDictDO> findByDictCodeAndDictName(String dictCode, String dictName);

    /**
     * Select by query with pagination.
     *
     * @param query    the {@linkplain ShenyuDictQuery}
     * @param pageable the pageable
     * @return the page of {@linkplain ShenyuDictDO}
     */
    @Query("""
            SELECT d FROM ShenyuDictDO d
            WHERE (:#{#query.type} IS NULL OR :#{#query.type} = '' OR d.type = :#{#query.type})
            AND (:#{#query.dictCode} IS NULL OR :#{#query.dictCode} = '' OR d.dictCode LIKE CONCAT('%', :#{#query.dictCode}, '%'))
            AND (:#{#query.dictName} IS NULL OR :#{#query.dictName} = '' OR d.dictName LIKE CONCAT('%', :#{#query.dictName}, '%'))
            ORDER BY d.type, d.sort, d.id
            """)
    Page<ShenyuDictDO> selectByQuery(@Param("query") ShenyuDictQuery query, Pageable pageable);

    /**
     * Update the enabled flag in batch, single-column hot toggle like the original SQL.
     *
     * @param ids     the ids
     * @param enabled the enabled flag
     * @return the updated row count
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("UPDATE ShenyuDictDO d SET d.enabled = :enabled WHERE d.id IN :ids")
    int enabled(@Param("ids") List<String> ids, @Param("enabled") Boolean enabled);
}
