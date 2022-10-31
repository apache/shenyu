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

package org.apache.shenyu.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.OperationRecordLog;
import org.apache.shenyu.admin.model.query.RecordLogQueryCondition;
import org.springframework.lang.Nullable;

import java.util.Date;
import java.util.List;

/**
 * OperationRecordLogMapper.
 */
@Mapper
public interface OperationRecordLogMapper {
    
    /**
     * select limit.
     *
     * @param username username
     * @param limit    limit
     * @return list
     */
    List<OperationRecordLog> selectLimit(@Nullable @Param("username") String username, @Param("limit") Integer limit);
    
    /**
     * insert.
     *
     * @param recordLog log
     * @return count change
     */
    int insert(OperationRecordLog recordLog);
    
    /**
     * select by condition.
     *
     * @param condition condition
     * @return list
     */
    List<OperationRecordLog> selectByCondition(@Param("condition") RecordLogQueryCondition condition);
    
    
    /**
     * delete data before time.
     *
     * @param time time
     * @return time
     */
    int deleteByBefore(@Param("time") Date time);
}
