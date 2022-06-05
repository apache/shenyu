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
import org.apache.shenyu.admin.mapper.ShenyuDictMapper;
import org.apache.shenyu.admin.model.dto.ShenyuDictDTO;
import org.apache.shenyu.admin.model.entity.ShenyuDictDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ShenyuDictQuery;
import org.apache.shenyu.admin.model.vo.ShenyuDictVO;
import org.apache.shenyu.admin.service.ShenyuDictService;
import org.apache.shenyu.admin.service.publish.DictEventPublisher;
import org.apache.shenyu.admin.utils.Assert;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ShenyuDictService}.
 */
@Service
public class ShenyuDictServiceImpl implements ShenyuDictService {
    
    private final ShenyuDictMapper shenyuDictMapper;
    
    private final DictEventPublisher publisher;
    
    public ShenyuDictServiceImpl(final ShenyuDictMapper shenyuDictMapper, final DictEventPublisher publisher) {
        this.shenyuDictMapper = shenyuDictMapper;
        this.publisher = publisher;
    }
    
    @Override
    @Pageable
    public CommonPager<ShenyuDictVO> listByPage(final ShenyuDictQuery shenyuDictQuery) {
        return PageResultUtils.result(shenyuDictQuery.getPageParameter(), () -> shenyuDictMapper.selectByQuery(shenyuDictQuery)
                .stream()
                .map(ShenyuDictVO::buildShenyuDictVO)
                .collect(Collectors.toList()));
    }
    
    @Override
    public Integer createOrUpdate(final ShenyuDictDTO shenyuDictDTO) {
        return StringUtils.isBlank(shenyuDictDTO.getId()) ? create(shenyuDictDTO) : update(shenyuDictDTO);
    }
    
    private int update(final ShenyuDictDTO shenyuDictDTO) {
        final ShenyuDictDO before = shenyuDictMapper.selectById(shenyuDictDTO.getId());
        Assert.notNull(before, "the dict is not existed");
        final ShenyuDictDO dict = ShenyuDictDO.buildShenyuDictDO(shenyuDictDTO);
        final int changeCount = shenyuDictMapper.updateByPrimaryKeySelective(dict);
        if (changeCount > 0) {
            publisher.onUpdated(dict, before);
        }
        return changeCount;
    }
    
    private int create(final ShenyuDictDTO shenyuDictDTO) {
        final ShenyuDictDO dict = ShenyuDictDO.buildShenyuDictDO(shenyuDictDTO);
        final int insertCount = shenyuDictMapper.insertSelective(dict);
        if (insertCount > 0) {
            publisher.onCreated(dict);
        }
        return insertCount;
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer deleteShenyuDicts(final List<String> ids) {
        final List<ShenyuDictDO> dictList = shenyuDictMapper.selectByIds(ids);
        final int deleteCount = shenyuDictMapper.deleteByIdList(ids);
        if (deleteCount > 0) {
            publisher.onDeleted(dictList);
        }
        return deleteCount;
    }
    
    @Override
    @Transactional(rollbackFor = Exception.class)
    public Integer enabled(final List<String> ids, final Boolean enabled) {
        return shenyuDictMapper.enabled(ids, enabled);
    }
    
    @Override
    public ShenyuDictVO findById(final String id) {
        return ShenyuDictVO.buildShenyuDictVO(shenyuDictMapper.selectById(id));
    }
    
    @Override
    public ShenyuDictVO findByDictCodeName(final String dictCode, final String dictName) {
        return ShenyuDictVO.buildShenyuDictVO(shenyuDictMapper.selectByDictCodeAndDictName(dictCode, dictName));
    }
    
    @Override
    public List<ShenyuDictVO> list(final String type) {
        ShenyuDictQuery shenyuDictQuery = new ShenyuDictQuery();
        shenyuDictQuery.setType(type);
        return shenyuDictMapper.selectByQuery(shenyuDictQuery).stream()
                .map(ShenyuDictVO::buildShenyuDictVO)
                .collect(Collectors.toList());
    }
    
}
