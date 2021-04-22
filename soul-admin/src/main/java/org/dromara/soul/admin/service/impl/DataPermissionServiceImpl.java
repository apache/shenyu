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

package org.dromara.soul.admin.service.impl;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.mapper.DataPermissionMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.model.dto.DataPermissionDTO;
import org.dromara.soul.admin.model.entity.DataPermissionDO;
import org.dromara.soul.admin.model.entity.RuleDO;
import org.dromara.soul.admin.model.page.PageParameter;
import org.dromara.soul.admin.model.query.DataPermissionQuery;
import org.dromara.soul.admin.model.query.RuleQuery;
import org.dromara.soul.admin.model.vo.DataPermissionPageVO;
import org.dromara.soul.admin.service.DataPermissionService;
import org.dromara.soul.common.enums.AdminDataPermissionTypeEnum;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * data permission vo.
 *
 * @author kaitoshy(plutokaito)
 */
@Service("dataPermissionService")
public class DataPermissionServiceImpl implements DataPermissionService {

    private final DataPermissionMapper dataPermissionMapper;

    private final RuleMapper ruleMapper;

    public DataPermissionServiceImpl(final DataPermissionMapper dataPermissionMapper, final RuleMapper ruleMapper) {
        this.dataPermissionMapper = dataPermissionMapper;
        this.ruleMapper = ruleMapper;
    }

    /**
     * Get all data permissions by user id.
     * @param userId user id
     * @return list of {@linkplain DataPermissionDO}
     */
    @Override
    public List<DataPermissionDO> getUserDataPermissionList(final String userId) {
        if (StringUtils.isBlank(userId)) {
            return null;
        }
        return dataPermissionMapper.listByUserId(userId);
    }


    /**
     * Create data permissions.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return int
     */
    @Override
    public int create(final DataPermissionDTO dataPermissionDTO) {
        if (dataPermissionDTO.getDataType().equals(AdminDataPermissionTypeEnum.SELECTOR.ordinal())) {
            List<DataPermissionDO> allRuleDo = ruleMapper.findBySelectorId(dataPermissionDTO.getDataId())
                    .stream()
                    .filter(Objects::nonNull)
                    .map(ruleDO -> DataPermissionDO.buildPermissionDO(ruleDO, dataPermissionDTO.getUserId()))
                    .collect(Collectors.toList());

            if (CollectionUtils.isNotEmpty(allRuleDo)) {
                allRuleDo.add(DataPermissionDO.buildPermissionDO(dataPermissionDTO));

                allRuleDo.forEach(dataPermissionMapper::insertSelective);

                return allRuleDo.size();
            }

            return 0;
        } else {
            return dataPermissionMapper.insertSelective(DataPermissionDO.buildPermissionDO(dataPermissionDTO));
        }
    }


    /**
     * delete data permission.
     * @param dataPermissionDTO {@linkplain DataPermissionDTO}
     * @return int  effect rows
     */
    @Override
    public int delete(final DataPermissionDTO dataPermissionDTO) {
        if (dataPermissionDTO.getDataType().equals(AdminDataPermissionTypeEnum.SELECTOR.ordinal())) {
            List<String> allRuleIds = ruleMapper.findBySelectorId(dataPermissionDTO.getDataId())
                    .stream()
                    .filter(Objects::nonNull)
                    .map(RuleDO::getId)
                    .collect(Collectors.toList());
            if (CollectionUtils.isNotEmpty(allRuleIds)) {
                allRuleIds.add(dataPermissionDTO.getDataId());
                return dataPermissionMapper.deleteByDataIdsAndUserId(allRuleIds, dataPermissionDTO.getUserId());
            }

            return 0;
        } else {
            return dataPermissionMapper.deleteByDataIdAndUserId(dataPermissionDTO.getDataId(), dataPermissionDTO.getUserId());
        }
    }

    @Override
    public DataPermissionPageVO listByPage(final DataPermissionQuery dataPermissionQuery) {
        int count = ruleMapper.countByQuery(new RuleQuery());

        PageParameter pageParameter = dataPermissionQuery.getPageParameter();

        List<RuleDO> ruleVOList = ruleMapper.selectByQuery(new RuleQuery(null, null, pageParameter));

//
//        DataPermissionPageVO vo = PageResultUtils.result(pageParameter,
//                () -> ruleMapper.countByQuery(new RuleQuery()),
//                ruleMapper.selectByQuery(new RuleQuery(null, null, pageParameter)));

        return null;
    }

}
