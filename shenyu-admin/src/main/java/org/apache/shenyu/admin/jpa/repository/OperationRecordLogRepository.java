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

import org.apache.shenyu.admin.model.entity.OperationRecordLog;
import org.apache.shenyu.admin.model.query.RecordLogQueryCondition;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;

/**
 * The interface Operation record log repository.
 */
@Repository
public interface OperationRecordLogRepository extends JpaRepository<OperationRecordLog, Long> {

    /**
     * Select limit by operator.
     *
     * @param operator the operator username
     * @param pageable the pageable
     * @return the list
     */
    List<OperationRecordLog> findByOperatorOrderByOperationTimeDesc(String operator, Pageable pageable);

    /**
     * Select limit order by operation time desc.
     *
     * @param pageable the pageable
     * @return the list
     */
    List<OperationRecordLog> findByOrderByOperationTimeDesc(Pageable pageable);

    /**
     * Select by condition.
     *
     * @param condition the {@linkplain RecordLogQueryCondition}
     * @return the list
     */
    default List<OperationRecordLog> selectByCondition(final RecordLogQueryCondition condition) {
        return pageByCondition(condition, Pageable.unpaged()).getContent();
    }

    /**
     * Page by condition.
     *
     * @param condition the condition
     * @param pageable  the pageable
     * @return page of {@linkplain OperationRecordLog}
     */
    @Query("""
            SELECT o FROM OperationRecordLog o WHERE
            o.operationTime BETWEEN :#{#condition.startTime} AND :#{#condition.endTime}
            AND (:#{#condition.keyword} IS NULL OR :#{#condition.keyword} = '' OR o.context LIKE CONCAT('%', :#{#condition.keyword}, '%'))
            AND (:#{#condition.excluded} IS NULL OR :#{#condition.excluded} = '' OR o.context NOT LIKE CONCAT('%', :#{#condition.excluded}, '%'))
            AND (:#{#condition.type} IS NULL OR o.operationType = :#{#condition.type})
            AND (:#{#condition.username} IS NULL OR o.operator = :#{#condition.username})
            ORDER BY o.operationTime DESC
            """)
    Page<OperationRecordLog> pageByCondition(@Param("condition") RecordLogQueryCondition condition, Pageable pageable);

    /**
     * Delete by before.
     *
     * @param time the time
     * @return the int
     */
    @Transactional
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("""
            DELETE FROM OperationRecordLog o WHERE o.operationTime < :time
            """)
    int deleteByBefore(@Param("time") Date time);
}
