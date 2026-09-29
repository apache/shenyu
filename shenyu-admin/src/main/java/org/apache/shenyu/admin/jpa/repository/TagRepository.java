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

import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.query.TagQuery;
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
 * TagRepository.
 */
@Repository
public interface TagRepository extends JpaRepository<TagDO, String>, ExistProvider {

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

    List<TagDO> findByParentTagIdIn(List<String> parentTagIds);


    List<TagDO> findByParentTagId(String parentTagId);

    /**
     * Select by query, null conditions are skipped like the original dynamic SQL.
     *
     * @param query the {@linkplain TagQuery}
     * @return the list
     */
    @Query("""
            SELECT t FROM TagDO t
            WHERE (:#{#query.tagName} IS NULL OR t.tagName = :#{#query.tagName})
            AND (:#{#query.parentTagId} IS NULL OR t.parentTagId = :#{#query.parentTagId})
            """)
    List<TagDO> selectByQuery(@Param("query") TagQuery query);

    /**
     * Delete by ids.
     *
     * @param ids the ids
     * @return the deleted row count
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM TagDO t WHERE t.id IN :ids")
    int deleteByIds(@Param("ids") List<String> ids);
}
