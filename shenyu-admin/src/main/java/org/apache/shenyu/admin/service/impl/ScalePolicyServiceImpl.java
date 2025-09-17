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

import org.apache.shenyu.admin.mapper.ScalePolicyMapper;
import org.apache.shenyu.admin.model.dto.ScalePolicyDTO;
import org.apache.shenyu.admin.model.entity.ScalePolicyDO;
import org.apache.shenyu.admin.model.vo.ScalePolicyVO;
import org.apache.shenyu.admin.scale.scaler.ScaleService;
import org.apache.shenyu.admin.scale.scaler.cache.ScalePolicyCache;
import org.apache.shenyu.admin.service.ScalePolicyService;
import org.apache.shenyu.common.utils.ListUtil;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Implementation of ScalePolicyService.
 */
@Service
public class ScalePolicyServiceImpl implements ScalePolicyService {

    private final ScalePolicyMapper scalePolicyMapper;

    private final ScalePolicyCache scalePolicyCache;

    private final ScaleService scaleService;

    public ScalePolicyServiceImpl(final ScalePolicyMapper scalePolicyMapper,
                                  final ScalePolicyCache scalePolicyCache,
                                  final ScaleService scaleService) {
        this.scalePolicyMapper = scalePolicyMapper;
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
        return ListUtil.map(scalePolicyMapper.selectAll(), ScalePolicyVO::buildScalePolicyVO);
    }

    /**
     * find scale policy by id.
     *
     * @param id primary key
     * @return {@linkplain ScalePolicyVO}
     */
    @Override
    public ScalePolicyVO findById(final String id) {
        return ScalePolicyVO.buildScalePolicyVO(scalePolicyMapper.selectByPrimaryKey(id));
    }

    /**
     * create or update scale policy.
     *
     * @param scalePolicyDTO {@linkplain ScalePolicyDTO}
     * @return rows int
     */
    @Override
    public int update(final ScalePolicyDTO scalePolicyDTO) {
        final ScalePolicyDO scalePolicy = ScalePolicyDO.buildScalePolicyDO(scalePolicyDTO);
        int rows = scalePolicyMapper.updateByPrimaryKeySelective(scalePolicy);
        if (rows > 0) {
            scalePolicyCache.updatePolicy(scalePolicy);
            scaleService.executeScaling();
        }
        return rows;
    }
}
