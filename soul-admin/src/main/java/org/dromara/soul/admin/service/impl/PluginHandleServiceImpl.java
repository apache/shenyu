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

import org.apache.commons.lang3.StringUtils;
import org.dromara.soul.admin.dto.PluginHandleDTO;
import org.dromara.soul.admin.entity.PluginHandleDO;
import org.dromara.soul.admin.mapper.PluginHandleMapper;
import org.dromara.soul.admin.mapper.SoulDictMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageResultUtils;
import org.dromara.soul.admin.query.PluginHandleQuery;
import org.dromara.soul.admin.service.PluginHandleService;
import org.dromara.soul.admin.vo.PluginHandleVO;
import org.dromara.soul.admin.vo.SoulDictVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * PluginHandleServiceImpl.
 *
 * @author liangziqiang.
 * @author dengliming
 * @author nuo-promise
 */
@Service("pluginHandleService")
public class PluginHandleServiceImpl implements PluginHandleService {

    private static final int SELECT_BOX_DATA_TYPE = 3;

    private final PluginHandleMapper pluginHandleMapper;

    private final SoulDictMapper soulDictMapper;

    @Autowired(required = false)
    public PluginHandleServiceImpl(final PluginHandleMapper pluginHandleMapper, final SoulDictMapper soulDictMapper) {
        this.pluginHandleMapper = pluginHandleMapper;
        this.soulDictMapper = soulDictMapper;
    }

    @Override
    public CommonPager<PluginHandleVO> listByPage(final PluginHandleQuery pluginHandleQuery) {
        return PageResultUtils.result(pluginHandleQuery.getPageParameter(),
            () -> pluginHandleMapper.countByQuery(pluginHandleQuery),
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
        List<SoulDictVO> dictOptions = null;

        if (pluginHandleDO.getDataType() == SELECT_BOX_DATA_TYPE) {
            dictOptions = soulDictMapper.findByType(pluginHandleDO.getField())
                    .stream()
                    .map(SoulDictVO::buildSoulDictVO)
                    .collect(Collectors.toList());
        }
        return PluginHandleVO.buildPluginHandleVO(pluginHandleDO, dictOptions);
    }
}
