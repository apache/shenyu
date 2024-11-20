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
import org.apache.shenyu.admin.model.dto.ScaleRuleDTO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.ScaleRuleQuery;
import org.apache.shenyu.admin.model.vo.ScaleRuleVO;

import java.util.List;

/**
 * this is scale rule service.
 */
public interface ScaleRuleService {

    /**
     * selectAll.
     *
     * @return java.util.List
     */
    List<ScaleRuleVO> selectAll();

    /**
     * find page of scale rule by query.
     *
     * @param scaleRuleQuery {@linkplain ScaleRuleQuery}
     * @return {@linkplain CommonPager}
     */
    CommonPager<ScaleRuleVO> listByPage(ScaleRuleQuery scaleRuleQuery);

    /**
     * find scale rule by id.
     *
     * @param id primary key
     * @return {@linkplain ScaleRuleVO}
     */
    ScaleRuleVO findById(String id);

    /**
     * create or update rule.
     *
     * @param scaleRuleDTO {@linkplain ScaleRuleDTO}
     * @return rows int
     */
    default int createOrUpdate(ScaleRuleDTO scaleRuleDTO) {
        return StringUtils.isBlank(scaleRuleDTO.getId()) ? create(scaleRuleDTO) : update(scaleRuleDTO);
    }

    /**
     * create or update rule.
     *
     * @param scaleRuleDTO {@linkplain ScaleRuleDTO}
     * @return rows int
     */
    int create(ScaleRuleDTO scaleRuleDTO);

    /**
     * create or update rule.
     *
     * @param scaleRuleDTO {@linkplain ScaleRuleDTO}
     * @return rows int
     */
    int update(ScaleRuleDTO scaleRuleDTO);

    /**
     * delete rules.
     *
     * @param ids primary key
     * @return rows int
     */
    int delete(List<String> ids);
}
