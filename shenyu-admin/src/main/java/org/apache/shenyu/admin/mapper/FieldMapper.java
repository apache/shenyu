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
import org.apache.shenyu.admin.model.entity.FieldDO;
import org.apache.shenyu.admin.model.query.FieldQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * FieldMapper.
 */
@Mapper
public interface FieldMapper extends ExistProvider {

    /**
     * existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);

    /**
     * insert fieldDO to table.
     *
     * @param fieldDO the fieldDO
     * @return insert count
     */
    int insert(FieldDO fieldDO);

    /**
     * insert fieldDO to table selective.
     *
     * @param fieldDO the fieldDO
     * @return insert count
     */
    int insertSelective(FieldDO fieldDO);

    /**
     * select by primary key.
     *
     * @param id primary key
     * @return object by primary key
     */
    FieldDO selectByPrimaryKey(String id);

    /**
     * update fieldDO selective.
     *
     * @param fieldDO the updated fieldDO
     * @return update count
     */
    int updateByPrimaryKeySelective(FieldDO fieldDO);

    /**
     * update fieldDO.
     *
     * @param fieldDO the updated fieldDO
     * @return update count
     */
    int updateByPrimaryKey(FieldDO fieldDO);

    /**
     * delete by primary key.
     *
     * @param id primaryKey
     * @return deleteCount
     */
    int deleteByPrimaryKey(String id);


    /**
     * batchDelete.
     *
     * @param ids ids
     * @return int
     */
    int batchDelete(@Param("ids") List<String> ids);

    /**
     * selectByQuery.
     *
     * @param fieldQuery fieldQuery
     * @return FieldDOList
     */
    List<FieldDO> selectByQuery(FieldQuery fieldQuery);

}
