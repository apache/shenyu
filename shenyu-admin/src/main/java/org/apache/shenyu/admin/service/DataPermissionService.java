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

import org.apache.shenyu.admin.model.vo.DataPermissionPageVO;
import org.apache.shenyu.admin.model.dto.DataPermissionDTO;
import org.apache.shenyu.admin.model.entity.DataPermissionDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.SelectorQuery;

import java.util.List;

/**
 * data permission service.
 */
public interface DataPermissionService {

    /**
     * get user data permissions via  user_id and data_type.
     * @param userId user_id
     * @return List of {@linkplain DataPermissionDO}
     */
    List<DataPermissionDO> getUserDataPermissionList(String userId);

    /**
     * get user data permission.
     *
     * @param userId user id
     * @return List of {@link String}
     */
    List<String> getDataPermission(String userId);

    /**
     * Create selector data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return int
     */
    int createSelector(DataPermissionDTO dataPermissionDTO);


    /**
     * Delete selector data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return int
     */
    int deleteSelector(DataPermissionDTO dataPermissionDTO);


    /**
     * Query paginated selectors with data permission.
     * @param selectorQuery {@linkplain SelectorQuery}
     * @param userId user id
     * @return {@linkplain CommonPager}
     */
    CommonPager<DataPermissionPageVO> listSelectorsByPage(SelectorQuery selectorQuery, String userId);

    /**
     * Query paginated rules with data permission.
     * @param ruleQuery {@linkplain RuleQuery}
     * @param userId user id
     * @return {@linkplain CommonPager}
     */
    CommonPager<DataPermissionPageVO> listRulesByPage(RuleQuery ruleQuery, String userId);

    /**
     * create rule data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows
     */
    int createRule(DataPermissionDTO dataPermissionDTO);

    /**
     * delete rule data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return effect rows
     */
    int deleteRule(DataPermissionDTO dataPermissionDTO);
}
