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
import org.dromara.soul.admin.model.dto.SoulDictDTO;
import org.dromara.soul.admin.model.entity.SoulDictDO;
import org.dromara.soul.admin.mapper.SoulDictMapper;
import org.dromara.soul.admin.model.page.CommonPager;
import org.dromara.soul.admin.model.page.PageResultUtils;
import org.dromara.soul.admin.model.query.SoulDictQuery;
import org.dromara.soul.admin.service.SoulDictService;
import org.dromara.soul.admin.model.vo.SoulDictVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * SoulDictServiceImpl.
 *
 * @author dengliming
 * @author nuo-promise
 */
@Service("soulDictService")
public class SoulDictServiceImpl implements SoulDictService {

    private final SoulDictMapper soulDictMapper;

    @Autowired(required = false)
    public SoulDictServiceImpl(final SoulDictMapper soulDictMapper) {
        this.soulDictMapper = soulDictMapper;
    }

    @Override
    public CommonPager<SoulDictVO> listByPage(final SoulDictQuery soulDictQuery) {
        return PageResultUtils.result(soulDictQuery.getPageParameter(),
            () -> soulDictMapper.countByQuery(soulDictQuery),
            () -> soulDictMapper.selectByQuery(soulDictQuery)
                        .stream()
                        .map(SoulDictVO::buildSoulDictVO)
                        .collect(Collectors.toList()));
    }

    @Override
    public Integer createOrUpdate(final SoulDictDTO soulDictDTO) {
        int count;
        SoulDictDO soulDictDO = SoulDictDO.buildSoulDictDO(soulDictDTO);
        if (StringUtils.isEmpty(soulDictDTO.getId())) {
            count = soulDictMapper.insertSelective(soulDictDO);
        } else {
            count = soulDictMapper.updateByPrimaryKeySelective(soulDictDO);
        }
        return count;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer deleteSoulDicts(final List<String> ids) {
        int affectedRows = 0;
        for (String id : ids) {
            affectedRows += soulDictMapper.delete(id);
        }
        return affectedRows;
    }

    @Override
    public Integer enabled(final List<String> ids, final Boolean enabled) {
        if (ids == null || ids.isEmpty()) {
            return 0;
        }
        return soulDictMapper.enabled(ids, enabled);
    }

    @Override
    public SoulDictVO findById(final String id) {
        return SoulDictVO.buildSoulDictVO(soulDictMapper.selectById(id));
    }

    @Override
    public List<SoulDictVO> list(final String type) {
        SoulDictQuery soulDictQuery = new SoulDictQuery();
        soulDictQuery.setType(type);
        return soulDictMapper.selectByQuery(soulDictQuery).stream()
                .map(SoulDictVO::buildSoulDictVO).collect(Collectors.toList());
    }

}
