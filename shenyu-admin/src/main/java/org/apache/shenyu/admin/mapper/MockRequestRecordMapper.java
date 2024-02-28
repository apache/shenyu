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
import org.apache.shenyu.admin.model.entity.MockRequestRecordDO;
import org.apache.shenyu.admin.model.query.MockRequestRecordQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * This is Mock Request Record Mapper.
 */
@Mapper
public interface MockRequestRecordMapper extends ExistProvider {

    /**
     * exiated.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);

    /**
     * Select by primary key.
     *
     * @param id primary key
     * @return the value of {@linkplain MockRequestRecordDO}
     */
    MockRequestRecordDO queryById(String id);

    /**
     * Select by condition.
     *
     * @param mockRequestRecordDO condition
     * @return The value of {@linkplain List}
     */
    List<MockRequestRecordDO> queryAll(@Param("item") MockRequestRecordDO mockRequestRecordDO);

    /**
     * Count with condition.
     *
     * @param mockRequestRecordDO condition
     * @return The value of count result
     */
    long count(MockRequestRecordDO mockRequestRecordDO);

    /**
     * Insert record.
     *
     * @param mockRequestRecordDO record
     * @return The value of insert count
     */
    int insert(MockRequestRecordDO mockRequestRecordDO);

    /**
     * Insert selective.
     * @param mockRequestRecordDO record
     * @return Number of rows affected
     */
    int insertSelective(MockRequestRecordDO mockRequestRecordDO);

    /**
     * Insert batch.
     *
     * @param list data list
     * @return Number of rows affected
     */
    int insertBatch(@Param("list") List<MockRequestRecordDO> list);

    /**
     * Update.
     *
     * @param mockRequestRecordDO record
     * @return Number of rows affected
     */
    int update(MockRequestRecordDO mockRequestRecordDO);

    /**
     * Delete by primary key.
     *
     * @param id primary key
     * @return Number of rows affected
     */
    int deleteById(String id);

    /**
     * batchDelete.
     * @param ids ids
     * @return Number of rows deleted
     */
    int batchDelete(List<String> ids);

    /**
     * selectByQuery.
     * @param mockRequestRecordQuery mockRequestRecordQuery
     * @return list of MockRequestRecordDO
     */
    List<MockRequestRecordDO> selectByQuery(MockRequestRecordQuery mockRequestRecordQuery);

}

