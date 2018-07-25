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

package org.dromara.soul.admin.service;

import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.RuleQuery;
import org.dromara.soul.admin.vo.RuleVO;

/**
 * this is rule service.
 *
 * @author jiangxiaofeng(programgeek @ 163.com)
 */
public interface RuleService {

    /**
     * save or update rule.
     *
     * @param ruleDTO {@linkplain RuleDTO}
     * @return rows
     */
    int saveOrUpdate(RuleDTO ruleDTO);

    /**
     * enabled or disabled rule.
     *
     * @param ruleDTO {@linkplain RuleDTO}
     * @return rows
     */
    int enabled(RuleDTO ruleDTO);

    /**
     * find rule by id.
     *
     * @param id pk.
     * @return {@linkplain RuleDO}
     */
    RuleDO findById(String id);

    /**
     * find page of rule by query.
     *
     * @param ruleQuery {@linkplain RuleQuery}
     * @return CommonPager<ruleVO>
     */
    CommonPager<RuleVO> listByPage(RuleQuery ruleQuery);
}