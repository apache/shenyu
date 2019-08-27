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

import org.I0Itec.zkclient.ZkClient;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.RuleConditionDTO;
import org.dromara.soul.admin.dto.RuleDTO;
import org.dromara.soul.admin.entity.PluginDO;
import org.dromara.soul.admin.entity.RuleConditionDO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.mapper.RuleConditionMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.RuleConditionQuery;
import org.dromara.soul.admin.query.RuleQuery;
import org.dromara.soul.admin.service.RuleService;
import org.dromara.soul.admin.vo.RuleConditionVO;
import org.dromara.soul.admin.vo.RuleVO;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.zk.ConditionZkDTO;
import org.dromara.soul.common.dto.zk.RuleZkDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * RuleServiceImpl.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Service("ruleService")
public class RuleServiceImpl implements RuleService {

    private RuleMapper ruleMapper;

    private RuleConditionMapper ruleConditionMapper;

    private SelectorMapper selectorMapper;

    private PluginMapper pluginMapper;

    private final ZkClient zkClient;

    @Autowired(required = false)
    public RuleServiceImpl(final RuleMapper ruleMapper, final RuleConditionMapper ruleConditionMapper,
                           final SelectorMapper selectorMapper, final PluginMapper pluginMapper, final ZkClient zkClient) {
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
        this.selectorMapper = selectorMapper;
        this.pluginMapper = pluginMapper;
        this.zkClient = zkClient;
    }

    /**
     * create or update rule.
     *
     * @param ruleDTO {@linkplain RuleDTO}
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int createOrUpdate(final RuleDTO ruleDTO) {
        int ruleCount;
        RuleDO ruleDO = RuleDO.buildRuleDO(ruleDTO);
        List<RuleConditionDTO> ruleConditionDTOs = ruleDTO.getRuleConditions();
        if (StringUtils.isEmpty(ruleDTO.getId())) {
            ruleCount = ruleMapper.insertSelective(ruleDO);
            ruleConditionDTOs.forEach(ruleConditionDTO -> {
                ruleConditionDTO.setRuleId(ruleDO.getId());
                ruleConditionMapper.insertSelective(RuleConditionDO.buildRuleConditionDO(ruleConditionDTO));
            });
        } else {
            ruleCount = ruleMapper.updateSelective(ruleDO);
            //delete rule condition then add
            ruleConditionMapper.deleteByQuery(new RuleConditionQuery(ruleDO.getId()));
            ruleConditionDTOs.forEach(ruleConditionDTO -> {
                ruleConditionDTO.setRuleId(ruleDO.getId());
                RuleConditionDO ruleConditionDO = RuleConditionDO.buildRuleConditionDO(ruleConditionDTO);
                ruleConditionMapper.insertSelective(ruleConditionDO);
            });
        }

        SelectorDO selectorDO = selectorMapper.selectById(ruleDO.getSelectorId());
        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        String ruleParentPath = ZkPathConstants.buildRuleParentPath(pluginDO.getName());
        if (!zkClient.exists(ruleParentPath)) {
            zkClient.createPersistent(ruleParentPath, true);
        }

        String ruleRealPath = ZkPathConstants.buildRulePath(pluginDO.getName(), selectorDO.getId(), ruleDO.getId());
        if (!zkClient.exists(ruleRealPath)) {
            zkClient.createPersistent(ruleRealPath, true);
        }

        List<ConditionZkDTO> conditionZkDTOs = ruleConditionDTOs.stream()
                .map(this::buildConditionZkDTO).collect(Collectors.toList());
        zkClient.writeData(ruleRealPath, buildRuleZkDTO(ruleDO, pluginDO.getName(), conditionZkDTOs));
        return ruleCount;
    }

    /**
     * delete rules.
     *
     * @param ids primary key.
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int delete(final List<String> ids) {
        for (String id : ids) {
            RuleDO ruleDO = ruleMapper.selectById(id);
            SelectorDO selectorDO = selectorMapper.selectById(ruleDO.getSelectorId());
            PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
            ruleMapper.delete(id);
            ruleConditionMapper.deleteByQuery(new RuleConditionQuery(id));
            String ruleRealPath = ZkPathConstants.buildRulePath(pluginDO.getName(), selectorDO.getId(), ruleDO.getId());
            if (zkClient.exists(ruleRealPath)) {
                zkClient.delete(ruleRealPath);
            }
        }
        return ids.size();
    }

    /**
     * find rule by id.
     *
     * @param id primary key..
     * @return {@linkplain RuleVO}
     */
    @Override
    public RuleVO findById(final String id) {
        return RuleVO.buildRuleVO(ruleMapper.selectById(id),
                ruleConditionMapper.selectByQuery(
                        new RuleConditionQuery(id)).stream()
                        .map(RuleConditionVO::buildRuleConditionVO)
                        .collect(Collectors.toList()));
    }

    /**
     * find page of rule by query.
     *
     * @param ruleQuery {@linkplain RuleQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<RuleVO> listByPage(final RuleQuery ruleQuery) {
        PageParameter pageParameter = ruleQuery.getPageParameter();
        return new CommonPager<>(
                new PageParameter(pageParameter.getCurrentPage(),
                        pageParameter.getPageSize(), ruleMapper.countByQuery(ruleQuery)),
                ruleMapper.selectByQuery(ruleQuery).stream()
                        .map(RuleVO::buildRuleVO)
                        .collect(Collectors.toList()));
    }

    private ConditionZkDTO buildConditionZkDTO(final RuleConditionDTO ruleConditionDTO) {
        return new ConditionZkDTO(ruleConditionDTO.getParamType(), ruleConditionDTO.getOperator(),
                ruleConditionDTO.getParamName(), ruleConditionDTO.getParamValue());
    }

    private RuleZkDTO buildRuleZkDTO(final RuleDO ruleDO, final String pluginName,
                                     final List<ConditionZkDTO> conditionZkDTOList) {
        return new RuleZkDTO(ruleDO.getId(), pluginName, ruleDO.getSelectorId(),
                ruleDO.getMatchMode(), ruleDO.getSort(), ruleDO.getEnabled(), ruleDO.getLoged(), ruleDO.getHandle(),
                conditionZkDTOList);
    }
}
