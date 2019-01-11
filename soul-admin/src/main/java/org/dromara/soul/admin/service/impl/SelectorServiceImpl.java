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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.SelectorConditionDTO;
import org.dromara.soul.admin.dto.SelectorDTO;
import org.dromara.soul.admin.entity.PluginDO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.entity.SelectorConditionDO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.mapper.RuleConditionMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.mapper.SelectorConditionMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageParameter;
import org.dromara.soul.admin.query.RuleConditionQuery;
import org.dromara.soul.admin.query.RuleQuery;
import org.dromara.soul.admin.query.SelectorConditionQuery;
import org.dromara.soul.admin.query.SelectorQuery;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.vo.SelectorConditionVO;
import org.dromara.soul.admin.vo.SelectorVO;
import org.dromara.soul.common.constant.ZkPathConstants;
import org.dromara.soul.common.dto.zk.ConditionZkDTO;
import org.dromara.soul.common.dto.zk.SelectorZkDTO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * SelectorServiceImpl.
 *
 * @author jiangxiaofeng(Nicholas)
 */
@Service("selectorService")
public class SelectorServiceImpl implements SelectorService {

    private SelectorMapper selectorMapper;

    private SelectorConditionMapper selectorConditionMapper;

    private final RuleMapper ruleMapper;

    private final RuleConditionMapper ruleConditionMapper;

    private PluginMapper pluginMapper;

    private final ZkClient zkClient;

    @Autowired(required = false)
    public SelectorServiceImpl(final SelectorMapper selectorMapper, final SelectorConditionMapper selectorConditionMapper,
                               final PluginMapper pluginMapper, final ZkClient zkClient, RuleMapper ruleMapper, RuleConditionMapper ruleConditionMapper) {
        this.selectorMapper = selectorMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.pluginMapper = pluginMapper;
        this.zkClient = zkClient;
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
    }

    /**
     * create or update selector.
     *
     * @param selectorDTO {@linkplain SelectorDTO}
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = RuntimeException.class)
    public int createOrUpdate(final SelectorDTO selectorDTO) {
        int selectorCount;

        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        List<SelectorConditionDTO> selectorConditionDTOs = selectorDTO.getSelectorConditions();
        if (StringUtils.isEmpty(selectorDTO.getId())) {
            selectorCount = selectorMapper.insertSelective(selectorDO);
            selectorConditionDTOs.forEach(selectorConditionDTO -> {
                selectorConditionDTO.setSelectorId(selectorDO.getId());
                selectorConditionMapper.insertSelective(SelectorConditionDO.buildSelectorConditionDO(selectorConditionDTO));
            });
        } else {
            selectorCount = selectorMapper.updateSelective(selectorDO);
            List<SelectorConditionDO> selectorConditions = selectorConditionMapper.selectByQuery(
                    new SelectorConditionQuery(selectorDO.getId()));
            selectorConditionDTOs.forEach(selectorConditionDTO -> {
                selectorConditionDTO.setSelectorId(selectorDO.getId());
                SelectorConditionDO selectorConditionDO = SelectorConditionDO.buildSelectorConditionDO(selectorConditionDTO);
                if (StringUtils.isEmpty(selectorConditionDTO.getId())) {
                    selectorConditionMapper.insertSelective(selectorConditionDO);
                } else {
                    selectorConditionMapper.updateSelective(selectorConditionDO);
                }
            });
            selectorConditions.stream().filter(selectorConditionDO -> selectorConditionDTOs.stream()
                    .filter(selectorConditionDTO -> StringUtils.isNoneEmpty(selectorConditionDTO.getId()))
                    .anyMatch(selectorConditionDTO -> !selectorConditionDO.getId().equals(selectorConditionDTO.getId())))
                    .forEach(selectorConditionDO -> selectorConditionMapper.delete(selectorConditionDO.getId()));
        }

        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        String selectorParentPath = ZkPathConstants.buildSelectorParentPath(pluginDO.getName());
        if (!zkClient.exists(selectorParentPath)) {
            zkClient.createPersistent(selectorParentPath, true);
        }

        String selectorRealPath = ZkPathConstants.buildSelectorRealPath(pluginDO.getName(), selectorDO.getId());
        if (!zkClient.exists(selectorRealPath)) {
            zkClient.createPersistent(selectorRealPath, true);
        }

        List<ConditionZkDTO> conditionZkDTOs = selectorConditionDTOs.stream().map(selectorConditionDTO ->
                new ConditionZkDTO(selectorConditionDTO.getParamType(), selectorConditionDTO.getOperator(),
                        selectorConditionDTO.getParamName(), selectorConditionDTO.getParamValue())).collect(Collectors.toList());
        zkClient.writeData(selectorRealPath, new SelectorZkDTO(selectorDO.getId(), selectorDO.getPluginId(), pluginDO.getName(),
                selectorDO.getName(), selectorDO.getMatchMode(), selectorDO.getType(), selectorDO.getSort(), selectorDO.getEnabled(),
                selectorDO.getLoged(), selectorDO.getContinued(), selectorDO.getHandle(), conditionZkDTOs));
        return selectorCount;
    }

    /**
     * delete selectors.
     *
     * @param ids primary key.
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int delete(final List<String> ids) {
        for (String id : ids) {

            SelectorDO selectorDO = selectorMapper.selectById(id);
            PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());

            selectorMapper.delete(id);
            selectorConditionMapper.deleteByQuery(new SelectorConditionQuery(id));

            //清除zookeeper上的选择器
            String selectorRealPath = ZkPathConstants.buildSelectorRealPath(pluginDO.getName(), selectorDO.getId());
            if (zkClient.exists(selectorRealPath)) {
                zkClient.delete(selectorRealPath);
            }
            //清除规则与规则条件
            final List<RuleDO> ruleDOList = ruleMapper.selectByQuery(new RuleQuery(id, null));
            if (CollectionUtils.isNotEmpty(ruleDOList)) {
                for (RuleDO ruleDO : ruleDOList) {
                    ruleMapper.delete(ruleDO.getId());
                    ruleConditionMapper.deleteByQuery(new RuleConditionQuery(ruleDO.getId()));
                    //清除zookeeper上的规则
                    final String rulePath = ZkPathConstants.buildRulePath(pluginDO.getName(), id, ruleDO.getId());
                    if (zkClient.exists(rulePath)) {
                        zkClient.delete(rulePath);
                    }
                }
            }
        }
        return ids.size();
    }

    /**
     * find selector by id.
     *
     * @param id primary key.
     * @return {@linkplain SelectorVO}
     */
    @Override
    public SelectorVO findById(final String id) {
        return SelectorVO.buildSelectorVO(selectorMapper.selectById(id), selectorConditionMapper.selectByQuery(
                new SelectorConditionQuery(id)).stream().map(SelectorConditionVO::buildSelectorConditionVO).collect(Collectors.toList()));
    }

    /**
     * find page of selector by query.
     *
     * @param selectorQuery {@linkplain SelectorQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<SelectorVO> listByPage(final SelectorQuery selectorQuery) {
        PageParameter pageParameter = selectorQuery.getPageParameter();
        return new CommonPager<>(
                new PageParameter(pageParameter.getCurrentPage(), pageParameter.getPageSize(), selectorMapper.countByQuery(selectorQuery)),
                selectorMapper.selectByQuery(selectorQuery).stream()
                        .map(SelectorVO::buildSelectorVO)
                        .collect(Collectors.toList()));
    }
}
