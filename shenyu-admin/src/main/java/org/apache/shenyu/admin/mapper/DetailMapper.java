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
import org.apache.shenyu.admin.model.entity.DetailDO;

@Mapper
public interface DetailMapper {

    /**
     * insert detailDO to table.
     *
     * @param fieldDO the detailDO
     * @return insert count
     */
    int insert(DetailDO fieldDO);

    /**
     * insert detailDO to table selective.
     *
     * @param fieldDO the detailDO
     * @return insert count
     */
    int insertSelective(DetailDO fieldDO);

    /**
     * select by primary key.
     *
     * @param id primary key
     * @return object by primary key
     */
    DetailDO selectByPrimaryKey(String id);

    /**
     * update detailDO selective.
     *
     * @param fieldDO the updated detailDO
     * @return update count
     */
    int updateByPrimaryKeySelective(DetailDO fieldDO);

    /**
     * update detailDO.
     *
     * @param fieldDO the updated detailDO
     * @return update count
     */
    int updateByPrimaryKey(DetailDO fieldDO);

    /**
     * delete by primary key.
     *
     * @param id primaryKey
     * @return deleteCount
     */
    int deleteByPrimaryKey(String id);
}
