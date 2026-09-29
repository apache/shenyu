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

import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.model.query.ScaleRuleQuery;
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
import java.util.List;

/**
 * ScaleRuleRepository.
 */
@Repository
public interface ScaleRuleRepository extends JpaRepository<ScaleRuleDO, String>, ExistProvider {

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
     * Select by query with pagination.
     *
     * @param query    the {@linkplain ScaleRuleQuery}
     * @param pageable the pageable
     * @return the page of {@linkplain ScaleRuleDO}
     */
    @Query("""
            SELECT r FROM ScaleRuleDO r
            WHERE (:#{#query.metricName} IS NULL OR :#{#query.metricName} = '' OR r.metricName LIKE CONCAT('%', :#{#query.metricName}, '%'))
            AND (:#{#query.type} IS NULL OR r.type = :#{#query.type})
            AND (:#{#query.status} IS NULL OR r.status = :#{#query.status})
            ORDER BY r.sort, r.dateCreated
            """)
    Page<ScaleRuleDO> selectByQuery(@Param("query") ScaleRuleQuery query, Pageable pageable);

    /**
     * Delete by ids.
     *
     * @param ids the ids
     * @return the deleted row count
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM ScaleRuleDO r WHERE r.id IN :ids")
    int deleteByIds(@Param("ids") List<String> ids);
}
