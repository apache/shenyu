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
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.RuleQueryCondition;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.validation.ExistProvider;

import java.io.Serializable;
import java.util.List;

/**
 * Rule mapper.
 */
@Mapper
public interface RuleMapper extends ExistProvider {
    
    /**
     * rule existed.
     *
     * @param id id
     * @return existed
     */
    @Override
    Boolean existed(@Param("id") Serializable id);
    
    /**
     * select rule by id.
     *
     * @param id primary key.
     * @return {@linkplain RuleDO}
     */
    RuleDO selectById(String id);
    
    /**
     * select rule by query.
     *
     * @param ruleQuery {@linkplain RuleQuery}
     * @return {@linkplain List}
     */
    List<RuleDO> selectByQuery(RuleQuery ruleQuery);
    
    /**
     * Find by selector id list.
     *
     * @param selectorId the selector id
     * @return the list
     */
    List<RuleDO> findBySelectorId(String selectorId);
    
    /**
     * Find by selector id list.
     *
     * @param selectorIds the selector ids
     * @return the list
     */
    List<RuleDO> findBySelectorIds(List<String> selectorIds);
    
    /**
     * select rule by name.
     *
     * @param name the name
     * @return rule do
     */
    RuleDO findByName(String name);
    
    /**
     * Find by selector id and name rule do.
     *
     * @param selectorId the selector id
     * @param name       the name
     * @return the rule do
     */
    RuleDO findBySelectorIdAndName(@Param("selectorId") String selectorId, @Param("name") String name);
    
    /**
     * count rule by query.
     *
     * @param ruleQuery {@linkplain RuleQuery}
     * @return {@linkplain Integer}
     */
    Integer countByQuery(RuleQuery ruleQuery);
    
    /**
     * insert rule.
     *
     * @param ruleDO {@linkplain RuleDO}
     * @return rows int
     */
    int insert(RuleDO ruleDO);
    
    /**
     * insert selective rule.
     *
     * @param ruleDO {@linkplain RuleDO}
     * @return rows int
     */
    int insertSelective(RuleDO ruleDO);
    
    /**
     * update rule.
     *
     * @param ruleDO {@linkplain RuleDO}
     * @return rows int
     */
    int update(RuleDO ruleDO);
    
    /**
     * update selective rule.
     *
     * @param ruleDO {@linkplain RuleDO}
     * @return rows int
     */
    int updateSelective(RuleDO ruleDO);
    
    /**
     * delete rule.
     *
     * @param id primary key.
     * @return rows int
     */
    int delete(String id);
    
    /**
     * delete rule.
     *
     * @param ids primary keys.
     * @return rows int
     */
    int deleteByIds(List<String> ids);
    
    /**
     * list all.
     *
     * @return {@linkplain List}
     */
    List<RuleDO> selectAll();
    
    /**
     * search by condition.
     *
     * @param condition condition
     * @return list
     */
    List<RuleVO> selectByCondition(@Param("condition") RuleQueryCondition condition);
    
    /**
     * get plugin name by selectorId.
     *
     * @param selectorId selectorId
     * @return plugin name
     */
    String getPluginNameBySelectorId(@Param("selectorId") String selectorId);
    
    /**
     * select by rule ids.
     *
     * @param ids ids
     * @return rules
     */
    List<RuleDO> selectByIds(@Param("ids") List<String> ids);
    
}
