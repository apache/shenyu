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

import org.apache.shenyu.admin.model.entity.MockRequestRecordDO;
import org.apache.shenyu.admin.model.query.MockRequestRecordQuery;
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
 * The interface Mock request record repository.
 */
@Repository
public interface MockRequestRecordRepository extends JpaRepository<MockRequestRecordDO, String>, ExistProvider {

    @Override
    default Boolean existed(Serializable key) {
        return existsById((String) key);
    }

    /**
     * Find by api id.
     *
     * @param apiId the api id
     * @return the list
     */
    List<MockRequestRecordDO> findByApiId(String apiId);

    /**
     * Select by query, conditions apply only when the value is non-null like the original SQL.
     *
     * @param query the {@linkplain MockRequestRecordQuery}
     * @return the list
     */
    @Query("""
            SELECT m FROM MockRequestRecordDO m
            WHERE (:#{#query.apiId} IS NULL OR m.apiId = :#{#query.apiId})
            AND (:#{#query.host} IS NULL OR m.host = :#{#query.host})
            AND (:#{#query.url} IS NULL OR m.url = :#{#query.url})
            AND (:#{#query.pathVariable} IS NULL OR m.pathVariable = :#{#query.pathVariable})
            AND (:#{#query.header} IS NULL OR m.header = :#{#query.header})
            """)
    List<MockRequestRecordDO> selectByQuery(@Param("query") MockRequestRecordQuery query);

    /**
     * Delete by ids.
     *
     * @param ids the ids
     * @return the deleted row count
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM MockRequestRecordDO m WHERE m.id IN :ids")
    int deleteByIds(@Param("ids") List<String> ids);
}
