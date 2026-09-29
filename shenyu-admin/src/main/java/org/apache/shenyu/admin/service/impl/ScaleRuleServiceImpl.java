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

import org.apache.shenyu.admin.jpa.repository.ScaleRuleRepository;
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
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.List;
import java.util.Objects;

/**
 * Implementation of the ScaleRuleService.
 */
@Service
public class ScaleRuleServiceImpl implements ScaleRuleService {

    private final ScaleRuleRepository scaleRuleRepository;

    private final ScaleRuleCache scaleRuleCache;

    public ScaleRuleServiceImpl(final ScaleRuleRepository scaleRuleRepository, final ScaleRuleCache scaleRuleCache) {
        this.scaleRuleRepository = scaleRuleRepository;
        this.scaleRuleCache = scaleRuleCache;
    }


    /**
     * selectAll.
     *
     * @return java.util.List
     */
    @Override
    public List<ScaleRuleVO> selectAll() {
        return ListUtil.map(scaleRuleRepository.findAll(), ScaleRuleVO::buildScaleRuleVO);
    }

    /**
     * find page of scale rule by query.
     *
     * @param scaleRuleQuery {@linkplain ScaleRuleQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<ScaleRuleVO> listByPage(final ScaleRuleQuery scaleRuleQuery) {
        return PageResultUtils.result(scaleRuleQuery.getPageParameter(),
                scaleRuleRepository.selectByQuery(scaleRuleQuery, PageResultUtils.of(scaleRuleQuery.getPageParameter())),
                ScaleRuleVO::buildScaleRuleVO);
    }

    /**
     * find scale rule by id.
     *
     * @param id primary key
     * @return {@linkplain ScaleRuleVO}
     */
    @Override
    public ScaleRuleVO findById(final String id) {
        return ScaleRuleVO.buildScaleRuleVO(scaleRuleRepository.findById(id).orElse(null));
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
        scaleRuleRepository.save(scaleRuleDO);
        runAfterCommit(() -> scaleRuleCache.addOrUpdateRuleToCache(scaleRuleDO));
        return 1;
    }

    /**
     * create or update rule.
     *
     * @param scaleRuleDTO {@linkplain ScaleRuleDTO}
     * @return rows int
     */
    @Override
    public int update(final ScaleRuleDTO scaleRuleDTO) {
        final ScaleRuleDO persisted = scaleRuleRepository.findById(scaleRuleDTO.getId()).orElse(null);
        if (Objects.isNull(persisted)) {
            return 0;
        }
        final ScaleRuleDO after = ScaleRuleDO.buildScaleRuleDO(scaleRuleDTO);
        final String beforeMetricName = persisted.getMetricName();
        if (Objects.nonNull(after.getMetricName())) {
            persisted.setMetricName(after.getMetricName());
        }
        if (Objects.nonNull(after.getType())) {
            persisted.setType(after.getType());
        }
        if (Objects.nonNull(after.getSort())) {
            persisted.setSort(after.getSort());
        }
        if (Objects.nonNull(after.getStatus())) {
            persisted.setStatus(after.getStatus());
        }
        if (Objects.nonNull(after.getMinimum())) {
            persisted.setMinimum(after.getMinimum());
        }
        if (Objects.nonNull(after.getMaximum())) {
            persisted.setMaximum(after.getMaximum());
        }
        persisted.setDateUpdated(after.getDateUpdated());
        scaleRuleRepository.save(persisted);
        runAfterCommit(() -> {
            if (!Objects.equals(beforeMetricName, after.getMetricName())) {
                scaleRuleCache.removeRulesFromCache(List.of(beforeMetricName));
            }
            scaleRuleCache.addOrUpdateRuleToCache(persisted);
        });
        return 1;
    }

    /**
     * delete rules.
     *
     * @param ids primary key
     * @return rows int
     */
    @Override
    public int delete(final List<String> ids) {
        int rows = scaleRuleRepository.deleteByIds(ids);
        if (rows > 0) {
            runAfterCommit(() -> scaleRuleCache.removeRulesByIdsFromCache(ids));
        }
        return rows;
    }

    private void runAfterCommit(final Runnable action) {
        if (!TransactionSynchronizationManager.isSynchronizationActive()) {
            action.run();
            return;
        }
        TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
            @Override
            public void afterCommit() {
                action.run();
            }
        });
    }
}
