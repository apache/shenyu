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

package org.apache.shenyu.admin.service.impl;

import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.mapper.ScaleRuleMapper;
import org.apache.shenyu.admin.model.dto.ScaleRuleDTO;
import org.apache.shenyu.admin.model.entity.ScaleRuleDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ScaleRuleQuery;
import org.apache.shenyu.admin.model.vo.ScaleRuleVO;
import org.apache.shenyu.admin.scale.monitor.subject.cache.ScaleRuleCache;
import org.apache.shenyu.admin.service.ScaleRuleService;
import org.apache.shenyu.common.utils.ListUtil;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of the ScaleRuleService.
 */
@Service
public class ScaleRuleServiceImpl implements ScaleRuleService {

    private final ScaleRuleMapper scaleRuleMapper;

    private final ScaleRuleCache scaleRuleCache;

    public ScaleRuleServiceImpl(final ScaleRuleMapper scaleRuleMapper, final ScaleRuleCache scaleRuleCache) {
        this.scaleRuleMapper = scaleRuleMapper;
        this.scaleRuleCache = scaleRuleCache;
    }


    /**
     * selectAll.
     *
     * @return java.util.List
     */
    @Override
    public List<ScaleRuleVO> selectAll() {
        return ListUtil.map(scaleRuleMapper.selectAll(), ScaleRuleVO::buildScaleRuleVO);
    }

    /**
     * find page of scale rule by query.
     *
     * @param scaleRuleQuery {@linkplain ScaleRuleQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    @Pageable
    public CommonPager<ScaleRuleVO> listByPage(final ScaleRuleQuery scaleRuleQuery) {
        return PageResultUtils.result(scaleRuleQuery.getPageParameter(), () -> scaleRuleMapper.selectByQuery(scaleRuleQuery)
                .stream()
                .map(ScaleRuleVO::buildScaleRuleVO)
                .collect(Collectors.toList()));
    }

    /**
     * find scale rule by id.
     *
     * @param id primary key
     * @return {@linkplain ScaleRuleVO}
     */
    @Override
    public ScaleRuleVO findById(final String id) {
        return ScaleRuleVO.buildScaleRuleVO(scaleRuleMapper.selectByPrimaryKey(id));
    }

    /**
     * create or update rule info.
     *
     * @param scaleRuleDTO {@linkplain ScaleRuleDTO}
     * @return rows
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int createOrUpdate(final ScaleRuleDTO scaleRuleDTO) {
        return ScaleRuleService.super.createOrUpdate(scaleRuleDTO);
    }

    /**
     * create or update rule.
     *
     * @param scaleRuleDTO {@linkplain ScaleRuleDTO}
     * @return rows int
     */
    @Override
    public int create(final ScaleRuleDTO scaleRuleDTO) {
        final ScaleRuleDO scaleRuleDO = ScaleRuleDO.buildScaleRuleDO(scaleRuleDTO);
        int rows = scaleRuleMapper.insertSelective(scaleRuleDO);
        if (rows > 0) {
            scaleRuleCache.addOrUpdateRuleToCache(ScaleRuleDO.buildScaleRuleDO(scaleRuleDTO));
        }
        return rows;
    }

    /**
     * create or update rule.
     *
     * @param scaleRuleDTO {@linkplain ScaleRuleDTO}
     * @return rows int
     */
    @Override
    public int update(final ScaleRuleDTO scaleRuleDTO) {
        final ScaleRuleDO after = ScaleRuleDO.buildScaleRuleDO(scaleRuleDTO);
        int rows = scaleRuleMapper.updateByPrimaryKey(after);
        if (rows > 0) {
            scaleRuleCache.addOrUpdateRuleToCache(ScaleRuleDO.buildScaleRuleDO(scaleRuleDTO));
        }
        return rows;
    }

    /**
     * delete rules.
     *
     * @param ids primary key
     * @return rows int
     */
    @Override
    public int delete(final List<String> ids) {
        int rows = scaleRuleMapper.delete(ids);
        if (rows > 0) {
            scaleRuleCache.removeRulesFromCache(ids);
        }
        return rows;
    }
}
