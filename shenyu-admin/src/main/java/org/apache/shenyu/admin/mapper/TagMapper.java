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
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.query.TagQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.util.List;

/**
 * this is User Tag Mapper.
 */
@Mapper
public interface TagMapper extends ExistProvider {

    /**
     * tag existed.
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
     * delete tag.
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
    int insert(TagDO record);

    /**
     * insert record selective.
     *
     * @param record the insert record
     * @return insert count
     */
    int insertSelective(TagDO record);

    /**
     * select by key.
     *
     * @param id primaryKey
     * @return tagDO
     */
    TagDO selectByPrimaryKey(String id);

    /**
     * query by parenttagIds.
     * @param list parenttagIds
     * @return tagDos
     */
    List<TagDO> selectByParentTagIds(List<String> list);

    /**
     * select tag by query.
     *
     * @param tagQuery {@linkplain org.apache.shenyu.admin.model.query.TagQuery}
     * @return {@linkplain List}
     */
    List<TagDO> selectByQuery(TagQuery tagQuery);

    /**
     * update record.
     *
     * @param record record
     * @return update count
     */
    int updateByPrimaryKeySelective(TagDO record);

    /**
     * update .
     *
     * @param record update record
     * @return update count
     */
    int updateByPrimaryKey(TagDO record);

    /**
     * delete all data.
     * @return delete count
     */
    int deleteAllData();

    /**
     * selectByIds.
     * @param list ids
     * @return List
     */
    List<TagDO> selectByIds(List<String> list);
}
