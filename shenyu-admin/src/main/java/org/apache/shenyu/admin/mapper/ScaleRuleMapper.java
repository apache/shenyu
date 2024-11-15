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
import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.model.query.ScaleRuleQuery;
import org.apache.shenyu.admin.validation.ExistProvider;

@Mapper
public interface ScaleRuleMapper extends ExistProvider {

    /**
     * scale rule existed.
     *
     * @param id id
     * @return Boolean
     */
    @Override
    Boolean existed(@Param("id") Serializable id);

    /**
     * count by example.
     *
     * @param scaleRuleQuery scaleRuleQuery
     * @return long
     */
    long countByQuery(ScaleRuleQuery scaleRuleQuery);

    /**
     * delete by id.
     *
     * @param id id
     * @return int
     */
    int deleteByPrimaryKey(String id);

    /**
     * delete.
     *
     * @param ids ids
     * @return int
     */
    int delete(List<String> ids);

    /**
     * insert.
     *
     * @param row row
     * @return int
     */
    int insert(ScaleRuleDO row);

    /**
     * insertSelective.
     *
     * @param row row
     * @return int
     */
    int insertSelective(ScaleRuleDO row);

    /**
     * selectByQuery.
     *
     * @param scaleRuleQuery scaleRuleQuery
     * @return ScaleRuleDO>
     */
    List<ScaleRuleDO> selectByQuery(ScaleRuleQuery scaleRuleQuery);

    /**
     * selectByPrimaryKey.
     *
     * @param id id
     * @return ScaleRuleDO
     */
    ScaleRuleDO selectByPrimaryKey(String id);

    /**
     * updateByPrimaryKeySelective.
     *
     * @param row row
     * @return int
     */
    int updateByPrimaryKeySelective(ScaleRuleDO row);

    /**
     * updateByPrimaryKey.
     *
     * @param row row
     * @return int
     */
    int updateByPrimaryKey(ScaleRuleDO row);

    /**
     * select all.
     *
     * @return List
     */
    List<ScaleRuleDO> selectAll();
}
