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

package org.apache.shenyu.admin.service;

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.RuleQueryCondition;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.common.dto.RuleData;

import java.util.List;

/**
 * this is rule service.
 */
public interface RuleService extends PageService<RuleQueryCondition, RuleVO> {
    
    /**
     * Register string.
     *
     * @param ruleDTO the rule dto
     * @return the string
     */
    String registerDefault(RuleDTO ruleDTO);
    
    /**
     * create or update rule.
     *
     * @param ruleDTO {@linkplain RuleDTO}
     * @return rows int
     */
    default int createOrUpdate(final RuleDTO ruleDTO) {
        return StringUtils.isBlank(ruleDTO.getId()) ? create(ruleDTO) : update(ruleDTO);
    }
    
    /**
     * create rule.
     *
     * @param ruleDTO {@linkplain RuleDTO}
     * @return rows int
     */
    int create(RuleDTO ruleDTO);
    
    /**
     * update rule.
     *
     * @param ruleDTO {@linkplain RuleDTO}
     * @return rows int
     */
    int update(RuleDTO ruleDTO);
    
    /**
     * delete rules.
     *
     * @param ids primary key.
     * @return rows int
     */
    int delete(List<String> ids);
    
    /**
     * find rule by id.
     *
     * @param id primary key.
     * @return {@linkplain RuleVO}
     */
    RuleVO findById(String id);
    
    /**
     * find page of rule by query.
     *
     * @param ruleQuery {@linkplain RuleQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<RuleVO> listByPage(RuleQuery ruleQuery);
    
    /**
     * List all list.
     *
     * @return the list
     */
    List<RuleData> listAll();
    
    /**
     * Find by selector id list.
     *
     * @param selectorId the selector id
     * @return the list
     */
    List<RuleData> findBySelectorId(String selectorId);
    
    /**
     * Find by a list of selector ids.
     *
     * @param selectorIdList a list of selector ids
     * @return the list of RuleDatas
     */
    List<RuleData> findBySelectorIdList(List<String> selectorIdList);
    
    /**
     * Find rule by name.
     *
     * @param name rule's name.
     * @return {@link RuleDO}
     */
    RuleDO findByName(String name);
}
