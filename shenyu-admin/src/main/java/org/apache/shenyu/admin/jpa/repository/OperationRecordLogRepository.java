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
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

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
     * Delete by before.
     *
     * @param time the time
     * @return the int
     */
    @Modifying(clearAutomatically = true, flushAutomatically = true)
    @Query("""
            DELETE FROM OperationRecordLog o WHERE o.operationTime < :time
            """)
    int deleteByBefore(@Param("time") Date time);
}
