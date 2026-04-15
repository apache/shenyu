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

import org.apache.shenyu.admin.model.entity.DashboardUserDO;
import org.apache.shenyu.admin.validation.ExistProvider;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.io.Serializable;
import java.util.Collection;
import java.util.List;
import java.util.Optional;

/**
 * The interface Dashboard user repository.
 */
@Repository
public interface DashboardUserRepository extends JpaRepository<DashboardUserDO, String>, ExistProvider {

    @Override
    default Boolean existed(Serializable key) {
        return existsById((String) key);
    }

    /**
     * Find by user name.
     *
     * @param userName the user name
     * @return the optional
     */
    Optional<DashboardUserDO> findByUserName(String userName);

    /**
     * Find by user name and password.
     *
     * @param userName the user name
     * @param password the password
     * @return the optional
     */
    Optional<DashboardUserDO> findByUserNameAndPassword(String userName, String password);

    /**
     * Find by id in.
     *
     * @param ids the ids
     * @return the list
     */
    List<DashboardUserDO> findByIdIn(Collection<String> ids);

    /**
     * Find by query.
     *
     * @param userName the user name
     * @param pageable the pageable
     * @return the page
     */
    @Query("""
            SELECT d
            FROM DashboardUserDO d
            WHERE (:#{#userName} IS NULL OR :#{#userName} = '' OR d.userName LIKE CONCAT('%', :#{#userName}, '%'))
            """)
    Page<DashboardUserDO> findByQuery(@Param("userName") String userName, Pageable pageable);

    /**
     * Update selective.
     *
     * @param entity the entity
     * @return the int
     */
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("""
            UPDATE DashboardUserDO d SET
            d.userName = COALESCE(:#{#entity.userName}, d.userName),
            d.password = COALESCE(:#{#entity.password}, d.password),
            d.role = COALESCE(:#{#entity.role}, d.role),
            d.enabled = COALESCE(:#{#entity.enabled}, d.enabled),
            d.clientId = COALESCE(:#{#entity.clientId}, d.clientId),
            d.dateUpdated = CURRENT_TIMESTAMP
            WHERE d.id = :#{#entity.id}
            """)
    int updateSelective(@Param("entity") DashboardUserDO entity);
}
