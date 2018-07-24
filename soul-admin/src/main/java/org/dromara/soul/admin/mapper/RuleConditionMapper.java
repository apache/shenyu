/*
 *   Licensed to the Apache Software Foundation (ASF) under one or more
 *   contributor license agreements.  See the NOTICE file distributed with
 *   this work for additional information regarding copyright ownership.
 *   The ASF licenses this file to You under the Apache License, Version 2.0
 *   (the "License"); you may not use this file except in compliance with
 *   the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 *
 */

package org.dromara.soul.admin.mapper;

import org.apache.ibatis.annotations.Mapper;
import org.dromara.soul.admin.entity.RuleConditionDO;
import org.dromara.soul.admin.entity.RuleDO;

/**
 * RuleConditionMapper.
 *
 * @author jiangxiaofeng(programgeek @ 163.com)
 */
@Mapper
public interface RuleConditionMapper {

    /**
     * select rule condition by id
     *
     * @param id
     * @return
     */
    RuleDO selectById(Long id);

    /**
     * insert rule condition
     *
     * @param ruleConditionDO
     * @return
     */
    int insert(RuleConditionDO ruleConditionDO);

    /**
     * insert selective rule condition
     *
     * @param ruleConditionDO
     * @return
     */
    int insertSelective(RuleConditionDO ruleConditionDO);

    /**
     * update rule condition
     *
     * @param ruleConditionDO
     * @return
     */
    int update(RuleConditionDO ruleConditionDO);

    /**
     * update selective rule condition
     *
     * @param ruleConditionDO
     * @return
     */
    int updateSelective(RuleConditionDO ruleConditionDO);
}