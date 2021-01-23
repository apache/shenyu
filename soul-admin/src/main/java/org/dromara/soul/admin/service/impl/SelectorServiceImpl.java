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
import org.dromara.soul.admin.dto.SelectorConditionDTO;
import org.dromara.soul.admin.dto.SelectorDTO;
import org.dromara.soul.admin.entity.PluginDO;
import org.dromara.soul.admin.entity.RuleDO;
import org.dromara.soul.admin.entity.SelectorConditionDO;
import org.dromara.soul.admin.entity.SelectorDO;
import org.dromara.soul.admin.listener.DataChangedEvent;
import org.dromara.soul.admin.mapper.PluginMapper;
import org.dromara.soul.admin.mapper.RuleConditionMapper;
import org.dromara.soul.admin.mapper.RuleMapper;
import org.dromara.soul.admin.mapper.SelectorConditionMapper;
import org.dromara.soul.admin.mapper.SelectorMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageResultUtils;
import org.dromara.soul.admin.query.RuleConditionQuery;
import org.dromara.soul.admin.query.RuleQuery;
import org.dromara.soul.admin.query.SelectorConditionQuery;
import org.dromara.soul.admin.query.SelectorQuery;
import org.dromara.soul.admin.service.SelectorService;
import org.dromara.soul.admin.transfer.ConditionTransfer;
import org.dromara.soul.admin.vo.SelectorConditionVO;
import org.dromara.soul.admin.vo.SelectorVO;
import org.dromara.soul.common.dto.ConditionData;
import org.dromara.soul.common.dto.SelectorData;
import org.dromara.soul.common.dto.convert.DivideUpstream;
import org.dromara.soul.common.enums.ConfigGroupEnum;
import org.dromara.soul.common.enums.DataEventTypeEnum;
import org.dromara.soul.common.enums.PluginEnum;
import org.dromara.soul.common.utils.GsonUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * SelectorServiceImpl.
 *
 * @author jiangxiaofeng(Nicholas)
 * @author xiaoyu
 * @author  nuo-promise
 */
@Service("selectorService")
public class SelectorServiceImpl implements SelectorService {

    private final SelectorMapper selectorMapper;

    private final SelectorConditionMapper selectorConditionMapper;

    private final RuleMapper ruleMapper;

    private final RuleConditionMapper ruleConditionMapper;

    private final PluginMapper pluginMapper;

    private final ApplicationEventPublisher eventPublisher;

    private final UpstreamCheckService upstreamCheckService;

    @Autowired(required = false)
    public SelectorServiceImpl(final SelectorMapper selectorMapper,
                               final SelectorConditionMapper selectorConditionMapper,
                               final PluginMapper pluginMapper,
                               final RuleMapper ruleMapper,
                               final RuleConditionMapper ruleConditionMapper,
                               final ApplicationEventPublisher eventPublisher,
                               final UpstreamCheckService upstreamCheckService) {
        this.selectorMapper = selectorMapper;
        this.selectorConditionMapper = selectorConditionMapper;
        this.pluginMapper = pluginMapper;
        this.ruleMapper = ruleMapper;
        this.ruleConditionMapper = ruleConditionMapper;
        this.eventPublisher = eventPublisher;
        this.upstreamCheckService = upstreamCheckService;
    }

    @Override
    public String register(final SelectorDTO selectorDTO) {
        SelectorDO selectorDO = SelectorDO.buildSelectorDO(selectorDTO);
        List<SelectorConditionDTO> selectorConditionDTOs = selectorDTO.getSelectorConditions();
        if (StringUtils.isEmpty(selectorDTO.getId())) {
            selectorMapper.insertSelective(selectorDO);
            selectorConditionDTOs.forEach(selectorConditionDTO -> {
                selectorConditionDTO.setSelectorId(selectorDO.getId());
                selectorConditionMapper.insertSelective(SelectorConditionDO.buildSelectorConditionDO(selectorConditionDTO));
            });
        }
        publishEvent(selectorDO, selectorConditionDTOs);
        return selectorDO.getId();
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
            //delete rule condition then add
            selectorConditionMapper.deleteByQuery(new SelectorConditionQuery(selectorDO.getId()));
            selectorConditionDTOs.forEach(selectorConditionDTO -> {
                selectorConditionDTO.setSelectorId(selectorDO.getId());
                SelectorConditionDO selectorConditionDO = SelectorConditionDO.buildSelectorConditionDO(selectorConditionDTO);
                selectorConditionMapper.insertSelective(selectorConditionDO);
            });
        }
        publishEvent(selectorDO, selectorConditionDTOs);
        updateDivideUpstream(selectorDO);
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

            //if divide selector delete
            if (PluginEnum.DIVIDE.getName().equals(pluginDO.getName())) {
                UpstreamCheckService.removeByKey(selectorDO.getName());
            }

            // publish delete event of Selector
            eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.DELETE,
                    Collections.singletonList(SelectorDO.transFrom(selectorDO, pluginDO.getName(), null))));

            // delete rule and ruleCondition
            final List<RuleDO> ruleDOList = ruleMapper.selectByQuery(new RuleQuery(id, null));
            if (CollectionUtils.isNotEmpty(ruleDOList)) {
                for (RuleDO ruleDO : ruleDOList) {
                    ruleMapper.delete(ruleDO.getId());
                    ruleConditionMapper.deleteByQuery(new RuleConditionQuery(ruleDO.getId()));
                    //发送删除选择器事件
                    eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.RULE, DataEventTypeEnum.DELETE,
                            Collections.singletonList(RuleDO.transFrom(ruleDO, pluginDO.getName(), null))));

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
        return SelectorVO.buildSelectorVO(selectorMapper.selectById(id),
                selectorConditionMapper.selectByQuery(
                        new SelectorConditionQuery(id))
                        .stream()
                        .map(SelectorConditionVO::buildSelectorConditionVO)
                        .collect(Collectors.toList()));
    }

    @Override
    public SelectorDO findByName(final String name) {
        return selectorMapper.selectByName(name);
    }

    @Override
    public SelectorData buildByName(final String name) {
        SelectorDO selectorDO = selectorMapper.selectByName(name);
        return buildSelectorData(selectorDO);
    }

    /**
     * find page of selector by query.
     *
     * @param selectorQuery {@linkplain SelectorQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<SelectorVO> listByPage(final SelectorQuery selectorQuery) {
        return PageResultUtils.result(selectorQuery.getPageParameter(),
            () -> selectorMapper.countByQuery(selectorQuery),
            () -> selectorMapper.selectByQuery(selectorQuery)
                        .stream()
                        .map(SelectorVO::buildSelectorVO)
                        .collect(Collectors.toList()));
    }

    @Override
    public List<SelectorData> findByPluginId(final String pluginId) {
        return selectorMapper.findByPluginId(pluginId)
                .stream()
                .map(this::buildSelectorData)
                .collect(Collectors.toList());
    }

    @Override
    public List<SelectorData> listAll() {
        return selectorMapper.selectAll()
                .stream()
                .filter(Objects::nonNull)
                .map(this::buildSelectorData)
                .collect(Collectors.toList());
    }

    private void publishEvent(final SelectorDO selectorDO, final List<SelectorConditionDTO> selectorConditionDTOs) {
        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        List<ConditionData> conditionDataList =
                selectorConditionDTOs.stream().map(ConditionTransfer.INSTANCE::mapToSelectorDTO).collect(Collectors.toList());
        // publish change event.
        eventPublisher.publishEvent(new DataChangedEvent(ConfigGroupEnum.SELECTOR, DataEventTypeEnum.UPDATE,
                Collections.singletonList(SelectorDO.transFrom(selectorDO, pluginDO.getName(), conditionDataList))));
    }

    private SelectorData buildSelectorData(final SelectorDO selectorDO) {
        // find conditions
        List<ConditionData> conditionDataList = ConditionTransfer.INSTANCE.mapToSelectorDOS(
                selectorConditionMapper.selectByQuery(new SelectorConditionQuery(selectorDO.getId())));
        PluginDO pluginDO = pluginMapper.selectById(selectorDO.getPluginId());
        if (Objects.isNull(pluginDO)) {
            return null;
        }
        return SelectorDO.transFrom(selectorDO, pluginDO.getName(), conditionDataList);
    }

    private void updateDivideUpstream(final SelectorDO selectorDO) {
        PluginDO pluginDO = pluginMapper.selectByName(PluginEnum.DIVIDE.getName());
        if (Objects.nonNull(pluginDO) && pluginDO.getId().equals(selectorDO.getPluginId())) {
            String selectorName = selectorDO.getName();
            String handle = selectorDO.getHandle();
            if (StringUtils.isNotBlank(handle)) {
                List<DivideUpstream> existDivideUpstreams = GsonUtils.getInstance().fromList(handle, DivideUpstream.class);
                upstreamCheckService.replace(selectorName, existDivideUpstreams);
            }
        }
    }
}
