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

import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.aspect.annotation.Pageable;
import org.apache.shenyu.admin.mapper.PluginHandleMapper;
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.dto.PluginHandleDTO;
import org.apache.shenyu.admin.model.entity.PluginHandleDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.PluginHandleQuery;
import org.apache.shenyu.admin.model.vo.PluginHandleVO;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.PluginHandleService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.PluginHandleService}.
 */
@Service
public class PluginHandleServiceImpl implements PluginHandleService {

    private static final int SELECT_BOX_DATA_TYPE = 3;

    private final PluginHandleMapper pluginHandleMapper;

    private final ShenyuDictMapper shenyuDictMapper;

    public PluginHandleServiceImpl(final PluginHandleMapper pluginHandleMapper, final ShenyuDictMapper shenyuDictMapper) {
        this.pluginHandleMapper = pluginHandleMapper;
        this.shenyuDictMapper = shenyuDictMapper;
    }

    @Override
    @Pageable
    public CommonPager<PluginHandleVO> listByPage(final PluginHandleQuery pluginHandleQuery) {
        return PageResultUtils.result(pluginHandleQuery.getPageParameter(),
            () -> pluginHandleMapper.selectByQuery(pluginHandleQuery)
                        .stream()
                        .map(this::buildPluginHandleVO)
                        .collect(Collectors.toList()));
    }

    @Override
    public Integer createOrUpdate(final PluginHandleDTO pluginHandleDTO) {
        int pluginHandleCount;
        PluginHandleDO pluginHandleDO = PluginHandleDO.buildPluginHandleDO(pluginHandleDTO);
        if (StringUtils.isEmpty(pluginHandleDTO.getId())) {
            pluginHandleCount = pluginHandleMapper.insertSelective(pluginHandleDO);
        } else {
            pluginHandleCount = pluginHandleMapper.updateByPrimaryKeySelective(pluginHandleDO);
        }
        return pluginHandleCount;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer deletePluginHandles(final List<String> ids) {
        int affectedRows = 0;
        for (String id : ids) {
            affectedRows += pluginHandleMapper.delete(id);
        }
        return affectedRows;
    }

    @Override
    public PluginHandleVO findById(final String id) {
        return buildPluginHandleVO(pluginHandleMapper.selectById(id));
    }

    @Override
    public List<PluginHandleVO> list(final String pluginId, final Integer type) {
        PluginHandleQuery pluginHandleQuery = PluginHandleQuery.builder()
                .pluginId(pluginId)
                .type(type)
                .build();
        return pluginHandleMapper.selectByQuery(pluginHandleQuery)
                .stream()
                .map(this::buildPluginHandleVO)
                .collect(Collectors.toList());
    }

    private PluginHandleVO buildPluginHandleVO(final PluginHandleDO pluginHandleDO) {
        List<ShenyuDictVO> dictOptions = null;

        if (pluginHandleDO.getDataType() == SELECT_BOX_DATA_TYPE) {
            dictOptions = shenyuDictMapper.findByType(pluginHandleDO.getField())
                    .stream()
                    .map(ShenyuDictVO::buildShenyuDictVO)
                    .collect(Collectors.toList());
        }
        return PluginHandleVO.buildPluginHandleVO(pluginHandleDO, dictOptions);
    }
}
