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
import org.apache.shenyu.admin.exception.ShenyuAdminException;
import org.apache.shenyu.admin.model.dto.RuleConditionDTO;
import org.apache.shenyu.admin.model.dto.RuleDTO;
import org.apache.shenyu.admin.model.entity.RuleDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.query.RuleQuery;
import org.apache.shenyu.admin.model.query.RuleQueryCondition;
import org.apache.shenyu.admin.model.result.ConfigImportResult;
import org.apache.shenyu.admin.model.vo.RuleVO;
import org.apache.shenyu.admin.service.configs.ConfigsImportContext;
import org.apache.shenyu.common.dto.RuleData;
import org.apache.shenyu.common.enums.OperatorEnum;
import org.apache.shenyu.common.enums.ParamTypeEnum;
import org.springframework.web.util.pattern.PathPatternParser;

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

        // now, only check rule uri condition in pathPattern mode
        // todo check uri in other modes

        try {
            final List<RuleConditionDTO> ruleConditions = ruleDTO.getRuleConditions();
            ruleConditions.stream()
                    .filter(conditionData -> ParamTypeEnum.URI.getName().equals(conditionData.getParamType()))
                    .filter(conditionData -> OperatorEnum.PATH_PATTERN.getAlias().equals(conditionData.getOperator()))
                    .map(RuleConditionDTO::getParamValue)
                    .forEach(PathPatternParser.defaultInstance::parse);
        } catch (Exception e) {
            throw new ShenyuAdminException("uri validation of Condition failed, please check.", e);
        }
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
     * delete rules by ids and namespaceId.
     *
     * @param ids primary key.
     * @param namespaceId namespaceId.
     * @return rows int
     */
    int deleteByIdsAndNamespaceId(List<String> ids, String namespaceId);

    /**
     * find rule by id and namespaceId.
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
     * List all list by namespaceId.
     *
     * @param namespaceId the namespaceId
     * @return the list
     */
    List<RuleData> listAllByNamespaceId(String namespaceId);

    /**
     * List all rule vo list.
     *
     * @return the rule vo list
     */
    List<RuleVO> listAllData();

    /**
     * List all rule vo list.
     *
     * @param namespaceId the namespaceId
     * @return the rule vo list
     */
    List<RuleVO> listAllDataByNamespaceId(String namespaceId);

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

    /**
     * Find by selector id and name rule do.
     *
     * @param selectorId selector id
     * @param name rule name
     * @return {@link RuleDO}
     */
    RuleDO findBySelectorIdAndName(String selectorId, String name);

    /**
     * Import data.
     *
     * @param ruleList rule list
     * @return config import result
     */
    ConfigImportResult importData(List<RuleDTO> ruleList);

    /**
     * Import data.
     *
     * @param namespace namespace
     * @param ruleList rule list
     * @param context import context
     * @return config import result
     */
    ConfigImportResult importData(String namespace, List<RuleDTO> ruleList, ConfigsImportContext context);

    /**
     * Enabled string by ids and namespaceId.
     *
     * @param ids     the ids
     * @param enabled the enabled
     * @param namespaceId the namespaceId.
     * @return the result
     */
    Boolean enabledByIdsAndNamespaceId(List<String> ids, Boolean enabled, String namespaceId);
}
