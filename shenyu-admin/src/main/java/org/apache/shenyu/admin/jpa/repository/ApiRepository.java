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
import org.apache.shenyu.admin.model.query.ApiQuery;
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

@Repository
public interface ApiRepository extends JpaRepository<ApiDO, String>, ExistProvider {

    @Override
    default Boolean existed(Serializable key) {
        return existsById((String) key);
    }

    /**
     * updateOfflineByContextPath.
     * @param contextPath context path
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("UPDATE ApiDO a SET a.state = 2, a.dateUpdated = CURRENT_TIMESTAMP WHERE a.contextPath = :contextPath")
    void updateOfflineByContextPath(@Param("contextPath") String contextPath);

    /**
     * Page by query, a non-blank tagId narrows to APIs bound to that tag,
     * replacing the original inner join on tag_relation with a guarded EXISTS.
     *
     * @param query    the {@linkplain ApiQuery}
     * @param pageable the pageable
     * @return the page of {@linkplain ApiDO}
     */
    @Query("""
            SELECT a FROM ApiDO a
            WHERE (:#{#query.tagId} IS NULL OR :#{#query.tagId} = '' OR EXISTS (
                SELECT tr FROM TagRelationDO tr WHERE tr.apiId = a.id AND tr.tagId = :#{#query.tagId}))
            AND (:#{#query.apiPath} IS NULL OR :#{#query.apiPath} = '' OR a.apiPath LIKE CONCAT('%', :#{#query.apiPath}, '%'))
            AND (:#{#query.state} IS NULL OR a.state = :#{#query.state})
            ORDER BY a.dateCreated DESC, a.id DESC
            """)
    Page<ApiDO> pageByQuery(@Param("query") ApiQuery query, Pageable pageable);

    /**
     * Find by api path, http method and rpc type.
     *
     * @param apiPath    the api path
     * @param httpMethod the http method
     * @param rpcType    the rpc type
     * @return the list
     */
    List<ApiDO> findByApiPathAndHttpMethodAndRpcType(String apiPath, Integer httpMethod, String rpcType);

    /**
     * Delete by ids.
     *
     * @param ids the ids
     * @return the deleted row count
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("DELETE FROM ApiDO a WHERE a.id IN :ids")
    int deleteByIds(@Param("ids") List<String> ids);
}
