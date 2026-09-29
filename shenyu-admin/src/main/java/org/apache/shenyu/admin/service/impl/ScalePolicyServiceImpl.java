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

import org.apache.shenyu.admin.jpa.repository.ScalePolicyRepository;
import org.apache.shenyu.admin.model.dto.ScalePolicyDTO;
import org.apache.shenyu.admin.model.entity.ScalePolicyDO;
import org.apache.shenyu.admin.model.vo.ScalePolicyVO;
import org.apache.shenyu.admin.scale.scaler.ScaleService;
import org.apache.shenyu.admin.scale.scaler.cache.ScalePolicyCache;
import org.apache.shenyu.admin.service.ScalePolicyService;
import org.apache.shenyu.common.utils.ListUtil;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionSynchronization;
import org.springframework.transaction.support.TransactionSynchronizationManager;

import java.util.List;
import java.util.Objects;

/**
 * Implementation of ScalePolicyService.
 */
@Service
public class ScalePolicyServiceImpl implements ScalePolicyService {

    private final ScalePolicyRepository scalePolicyRepository;

    private final ScalePolicyCache scalePolicyCache;

    private final ScaleService scaleService;

    public ScalePolicyServiceImpl(final ScalePolicyRepository scalePolicyRepository,
                                  final ScalePolicyCache scalePolicyCache,
                                  final ScaleService scaleService) {
        this.scalePolicyRepository = scalePolicyRepository;
        this.scalePolicyCache = scalePolicyCache;
        this.scaleService = scaleService;
    }

    /**
     * select all.
     *
     * @return List
     */
    @Override
    public List<ScalePolicyVO> selectAll() {
        return ListUtil.map(scalePolicyRepository.findAll(), ScalePolicyVO::buildScalePolicyVO);
    }

    /**
     * find scale policy by id.
     *
     * @param id primary key
     * @return {@linkplain ScalePolicyVO}
     */
    @Override
    public ScalePolicyVO findById(final String id) {
        return ScalePolicyVO.buildScalePolicyVO(scalePolicyRepository.findById(id).orElse(null));
    }

    /**
     * create or update scale policy.
     *
     * @param scalePolicyDTO {@linkplain ScalePolicyDTO}
     * @return rows int
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public int update(final ScalePolicyDTO scalePolicyDTO) {
        final ScalePolicyDO scalePolicy = ScalePolicyDO.buildScalePolicyDO(scalePolicyDTO);
        if (Objects.isNull(scalePolicy)) {
            return 0;
        }
        return scalePolicyRepository.findById(scalePolicy.getId())
                .map(persisted -> {
                    if (Objects.nonNull(scalePolicy.getSort())) {
                        persisted.setSort(scalePolicy.getSort());
                    }
                    if (Objects.nonNull(scalePolicy.getStatus())) {
                        persisted.setStatus(scalePolicy.getStatus());
                    }
                    if (Objects.nonNull(scalePolicy.getNum())) {
                        persisted.setNum(scalePolicy.getNum());
                    }
                    if (Objects.nonNull(scalePolicy.getBeginTime())) {
                        persisted.setBeginTime(scalePolicy.getBeginTime());
                    }
                    if (Objects.nonNull(scalePolicy.getEndTime())) {
                        persisted.setEndTime(scalePolicy.getEndTime());
                    }
                    persisted.setDateUpdated(scalePolicy.getDateUpdated());
                    scalePolicyRepository.save(persisted);
                    if (TransactionSynchronizationManager.isSynchronizationActive()) {
                        TransactionSynchronizationManager.registerSynchronization(new TransactionSynchronization() {
                            @Override
                            public void afterCommit() {
                                applyPolicy(persisted);
                            }
                        });
                    } else {
                        applyPolicy(persisted);
                    }
                    return 1;
                })
                .orElse(0);
    }

    private void applyPolicy(final ScalePolicyDO scalePolicy) {
        scalePolicyCache.updatePolicy(scalePolicy);
        scaleService.executeScaling();
    }
}
