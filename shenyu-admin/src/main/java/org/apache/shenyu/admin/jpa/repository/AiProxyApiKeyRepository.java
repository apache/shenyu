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

import org.apache.shenyu.admin.model.entity.ProxyApiKeyDO;
import org.apache.shenyu.admin.model.query.ProxyApiKeyQuery;
import org.apache.shenyu.admin.validation.ExistProvider;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.io.Serializable;
import java.util.List;

/**
 * AiProxyApiKeyRepository.
 */
@Repository
public interface AiProxyApiKeyRepository extends JpaRepository<ProxyApiKeyDO, String>, ExistProvider {

    @Override
    default Boolean existed(Serializable key) {
        return existsById((String) key);
    }

    boolean existsBySelectorIdAndProxyApiKey(@Param("selectorId") String selectorId, @Param("proxyApiKey") String proxyApiKey);

    List<ProxyApiKeyDO> findBySelectorId(@Param("selectorId") String selectorId);

    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("""
            UPDATE ProxyApiKeyDO p SET p.enabled = :enabled, p.dateUpdated = CURRENT_TIMESTAMP WHERE p.id IN :ids
            """)
    int updateEnableBatch(@Param("ids") List<String> ids, @Param("enabled") Boolean enabled);


    @Query("""
            SELECT p
            FROM ProxyApiKeyDO p
            WHERE (:#{#query.proxyApiKey} IS NULL OR :#{#query.proxyApiKey} = '' OR p.proxyApiKey = :#{#query.proxyApiKey})
            AND coalesce(:#{#query.enabled}, p.enabled) = p.enabled
            AND (:#{#query.namespaceId} IS NULL OR :#{#query.namespaceId} = '' OR p.namespaceId = :#{#query.namespaceId})
            AND (:#{#query.selectorId} IS NULL OR :#{#query.selectorId} = '' OR p.selectorId = :#{#query.selectorId})
            ORDER BY p.dateUpdated DESC
            """)
    Page<ProxyApiKeyDO> pageByCondition(@Param("query") ProxyApiKeyQuery query, Pageable pageable);

    default List<ProxyApiKeyDO> selectByCondition(ProxyApiKeyQuery query) {
        return pageByCondition(query, Pageable.unpaged()).getContent();
    }
}
