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

import lombok.RequiredArgsConstructor;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ShenyuDictService}.
 */
@RequiredArgsConstructor
@Service
public class ShenyuDictServiceImpl implements ShenyuDictService {

    private final ShenyuDictMapper shenyuDictMapper;

    @Override
    public CommonPager<ShenyuDictVO> listByPage(final ShenyuDictQuery shenyuDictQuery) {
        return PageResultUtils.result(shenyuDictQuery.getPageParameter(),
            () -> shenyuDictMapper.countByQuery(shenyuDictQuery),
            () -> shenyuDictMapper.selectByQuery(shenyuDictQuery)
                        .stream()
                        .map(ShenyuDictVO::buildShenyuDictVO)
                        .collect(Collectors.toList()));
    }

    @Override
    public Integer createOrUpdate(final ShenyuDictDTO shenyuDictDTO) {
        int count;
        ShenyuDictDO shenyuDictDO = ShenyuDictDO.buildShenyuDictDO(shenyuDictDTO);
        if (StringUtils.isEmpty(shenyuDictDTO.getId())) {
            count = shenyuDictMapper.insertSelective(shenyuDictDO);
        } else {
            count = shenyuDictMapper.updateByPrimaryKeySelective(shenyuDictDO);
        }
        return count;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer deleteShenyuDicts(final List<String> ids) {
        int affectedRows = 0;
        for (String id : ids) {
            affectedRows += shenyuDictMapper.delete(id);
        }
        return affectedRows;
    }

    @Override
    public Integer enabled(final List<String> ids, final Boolean enabled) {
        if (ids == null || ids.isEmpty()) {
            return 0;
        }
        return shenyuDictMapper.enabled(ids, enabled);
    }

    @Override
    public ShenyuDictVO findById(final String id) {
        return ShenyuDictVO.buildShenyuDictVO(shenyuDictMapper.selectById(id));
    }

    @Override
    public List<ShenyuDictVO> list(final String type) {
        ShenyuDictQuery shenyuDictQuery = new ShenyuDictQuery();
        shenyuDictQuery.setType(type);
        return shenyuDictMapper.selectByQuery(shenyuDictQuery).stream()
                .map(ShenyuDictVO::buildShenyuDictVO).collect(Collectors.toList());
    }

}
