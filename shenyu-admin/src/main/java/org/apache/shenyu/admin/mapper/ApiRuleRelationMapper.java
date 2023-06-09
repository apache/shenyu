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
import org.apache.shenyu.admin.model.entity.ApiRuleRelationDO;

/**
 * ApiRuleRelationMapper.
 */
@Mapper
public interface ApiRuleRelationMapper {
    /**
     * delete by primary key.
     *
     * @param id primaryKey
     * @return deleteCount
     */
    int deleteByPrimaryKey(String id);

    /**
     * insert record to table.
     *
     * @param record the record
     * @return insert count
     */
    int insert(ApiRuleRelationDO record);

    /**
     * insert record to table selective.
     *
     * @param record the record
     * @return insert count
     */
    int insertSelective(ApiRuleRelationDO record);

    /**
     * select by primary key.
     *
     * @param id primary key
     * @return object by primary key
     */
    ApiRuleRelationDO selectByPrimaryKey(String id);

    /**
     * update record selective.
     *
     * @param record the updated record
     * @return update count
     */
    int updateByPrimaryKeySelective(ApiRuleRelationDO record);

    /**
     * update record.
     *
     * @param record the updated record
     * @return update count
     */
    int updateByPrimaryKey(ApiRuleRelationDO record);
}
