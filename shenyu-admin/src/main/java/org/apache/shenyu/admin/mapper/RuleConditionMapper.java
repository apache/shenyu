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
import org.apache.shenyu.admin.model.entity.RuleConditionDO;
import org.apache.shenyu.admin.model.query.RuleConditionQuery;

import java.util.List;
import java.util.Set;

/**
 * RuleConditionMapper.
 */
@Mapper
public interface RuleConditionMapper {
    
    /**
     * select rule condition by id.
     *
     * @param id primary key.
     * @return {@linkplain RuleConditionDO}
     */
    RuleConditionDO selectById(String id);
    
    /**
     * select rule condition by query.
     *
     * @param ruleConditionQuery {@linkplain RuleConditionQuery}
     * @return {@linkplain List}
     */
    List<RuleConditionDO> selectByQuery(RuleConditionQuery ruleConditionQuery);
    
    /**
     * select list of rule conditions by a set of ruleIds.
     *
     * @param ruleIdSet a set of ruleIds
     * @return a list of {@linkplain RuleConditionDO}
     */
    List<RuleConditionDO> selectByRuleIdSet(@Param("ruleIdSet") Set<String> ruleIdSet);
    
    /**
     * insert rule condition.
     *
     * @param ruleConditionDO {@linkplain RuleConditionDO}
     * @return rows
     */
    int insert(RuleConditionDO ruleConditionDO);
    
    /**
     * insert selective rule condition.
     *
     * @param ruleConditionDO {@linkplain RuleConditionDO}
     * @return rows
     */
    int insertSelective(RuleConditionDO ruleConditionDO);
    
    /**
     * update rule condition.
     *
     * @param ruleConditionDO {@linkplain RuleConditionDO}
     * @return rows
     */
    int update(RuleConditionDO ruleConditionDO);
    
    /**
     * update selective rule condition.
     *
     * @param ruleConditionDO {@linkplain RuleConditionDO}
     * @return rows
     */
    int updateSelective(RuleConditionDO ruleConditionDO);
    
    /**
     * delete rule condition.
     *
     * @param id primary key.
     * @return rows
     */
    int delete(String id);
    
    /**
     * delete rule condition.
     *
     * @param ruleIds the rule ids
     * @return rows
     */
    int deleteByRuleIds(List<String> ruleIds);
    
    /**
     * delete rule condition by query.
     *
     * @param ruleConditionQuery {@linkplain RuleConditionQuery}
     * @return rows
     */
    int deleteByQuery(RuleConditionQuery ruleConditionQuery);
}
