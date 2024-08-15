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
import org.apache.shenyu.admin.model.entity.ScaleHistoryDO;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * ScaleHistory Mapper.
 */
@Mapper
public interface ScaleHistoryMapper extends ExistProvider {

    /**
     * scale history existed.
     *
     * @param id id
     * @return Boolean
     */
    @Override
    Boolean existed(@Param("id") Serializable id);

    /**
     * deleteByPrimaryKey.
     *
     * @param id id
     * @return int
     */
    int deleteByPrimaryKey(String id);

    /**
     * insert.
     *
     * @param row row
     * @return int
     */
    int insert(ScaleHistoryDO row);

    /**
     * insertSelective.
     *
     * @param row row
     * @return int
     */
    int insertSelective(ScaleHistoryDO row);

    /**
     * selectByPrimaryKey.
     *
     * @param id id
     * @return ScaleHistoryDO
     */
    ScaleHistoryDO selectByPrimaryKey(String id);

    /**
     * updateByPrimaryKeySelective.
     *
     * @param row row
     * @return int
     */
    int updateByPrimaryKeySelective(ScaleHistoryDO row);

    /**
     * updateByPrimaryKeyWithBLOBs.
     *
     * @param row row
     * @return int
     */
    int updateByPrimaryKeyWithBLOBs(ScaleHistoryDO row);

    /**
     * updateByPrimaryKey.
     *
     * @param row row
     * @return int
     */
    int updateByPrimaryKey(ScaleHistoryDO row);


    /**
     * select all.
     *
     * @return List
     */
    List<ScaleHistoryDO> selectAll();
}
