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
import org.apache.shenyu.admin.model.entity.ModelDO;

/**
 * ModelMapper.
 */
@Mapper
public interface ModelMapper {

    /**
     * insert modelDO to table.
     *
     * @param modelDO the modelDO
     * @return insert count
     */
    int insert(ModelDO modelDO);

    /**
     * insert modelDO to table selective.
     *
     * @param modelDO the modelDO
     * @return insert count
     */
    int insertSelective(ModelDO modelDO);

    /**
     * select by primary key.
     *
     * @param id primary key
     * @return object by primary key
     */
    ModelDO selectByPrimaryKey(String id);

    /**
     * update modelDO selective.
     *
     * @param modelDO the updated modelDO
     * @return update count
     */
    int updateByPrimaryKeySelective(ModelDO modelDO);

    /**
     * update modelDO.
     *
     * @param modelDO the updated modelDO
     * @return update count
     */
    int updateByPrimaryKey(ModelDO modelDO);

    /**
     * delete by primary key.
     *
     * @param id primaryKey
     * @return deleteCount
     */
    int deleteByPrimaryKey(String id);

}
