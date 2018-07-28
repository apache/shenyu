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

package org.dromara.soul.admin.service.impl;

import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.query.RuleQuery;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.vo.RuleVO;
import org.springframework.stereotype.Service;

/**
 * RuleServiceImpl.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Service("ruleService")
public class RuleServiceImpl implements RuleService {

    /**
     * create or update rule.
     *
     * @param ruleDTO {@linkplain RuleDTO}
     * @return rows
     */
    @Override
    public int createOrUpdate(final RuleDTO ruleDTO) {
        return 0;
    }

    /**
     * delete rule.
     *
     * @param id primary key.
     * @return rows
     */
    @Override
    public int delete(final String id) {
        return 0;
    }

    /**
     * find rule by id.
     *
     * @param id pk.
     * @return {@linkplain RuleDO}
     */
    @Override
    public RuleDO findById(final String id) {
        return null;
    }

    /**
     * find page of rule by query.
     *
     * @param ruleQuery {@linkplain RuleQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<RuleVO> listByPage(final RuleQuery ruleQuery) {
        return null;
    }
}
