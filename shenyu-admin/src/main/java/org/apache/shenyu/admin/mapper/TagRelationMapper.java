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

import java.io.Serializable;
import java.util.List;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.admin.model.query.TagRelationQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

/**
 * this is User Tag Mapper.
 */
@Mapper
public interface TagRelationMapper extends ExistProvider {

    /**
     * tag_relation existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);

    /**
     * delete by primary key.
     *
     * @param id primaryKey
     * @return deleteCount
     */
    int deleteByPrimaryKey(String id);

    /**
     * delete tag relation.
     *
     * @param ids primary keys.
     * @return rows int
     */
    int deleteByIds(List<String> ids);

    /**
     * update record selective.
     *
     * @param record record the updated record
     * @return update conut
     */
    int insert(TagRelationDO record);

    /**
     * insert record selective.
     *
     * @param record the insert record
     * @return insert count
     */
    int insertSelective(TagRelationDO record);

    /**
     * select by key.
     *
     * @param id primarykey
     * @return tagRelationDO
     */
    TagRelationDO selectByPrimaryKey(String id);

    /**
     * select tag relation by query.
     *
     * @param tagRelationQuery {@linkplain org.apache.shenyu.admin.model.query.TagRelationQuery}
     * @return {@linkplain List}
     */
    List<TagRelationDO> selectByQuery(TagRelationQuery tagRelationQuery);

    /**
     * update record.
     *
     * @param record record
     * @return update count
     */
    int updateByPrimaryKeySelective(TagRelationDO record);

    /**
     * update .
     *
     * @param record update record
     * @return update count
     */
    int updateByPrimaryKey(TagRelationDO record);

    /**
     * batchInsert.
     * @param list list
     * @return insert rows
     */
    int batchInsert(@Param(value = "list") List<TagRelationDO> list);

    /**
     * deleteByApiId.
     * @param apiId apiId
     * @return delete rows
     */
    int deleteByApiId(@Param(value = "apiId") String apiId);

    /**
     * deleteByApiIds.
     * @param apiIds apiIds
     * @return delete rows
     */
    int deleteByApiIds(@Param(value = "apiIds") List<String> apiIds);
}

