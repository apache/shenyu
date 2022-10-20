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

import com.google.common.collect.Lists;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import org.apache.shenyu.admin.mapper.TagMapper;
import org.apache.shenyu.admin.model.dto.TagDTO;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.query.TagQuery;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.admin.utils.Assert;
import org.springframework.stereotype.Service;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.TagService}.
 */
@Service
public class TagServiceImpl implements TagService {

    private TagMapper tagMapper;

    public TagServiceImpl(final TagMapper tagMapper) {
        this.tagMapper = tagMapper;
    }

    @Override
    public int create(final TagDTO tagDTO) {
        TagDO tagRelationDO = TagDO.buildTagDO(tagDTO);
        return tagMapper.insert(tagRelationDO);
    }

    @Override
    public int update(final TagDTO tagDTO) {
        TagDO before = tagMapper.selectByPrimaryKey(tagDTO.getId());
        Assert.notNull(before, "the updated rule is not found");
        TagDO tagDO = TagDO.buildTagDO(tagDTO);
        return tagMapper.updateByPrimaryKeySelective(tagDO);
    }

    @Override
    public int delete(final List<String> ids) {
        return tagMapper.deleteByIds(ids);
    }

    @Override
    public TagVO findById(final String id) {
        TagDO tagDO = tagMapper.selectByPrimaryKey(id);
        return TagVO.buildTagVO(tagDO);
    }

    @Override
    public TagVO findByQuery(final String tagName) {
        TagVO tagVO = new TagVO();
        TagQuery tagQuery = new TagQuery();
        List<TagDO> tagDOS = Optional.ofNullable(tagMapper.selectByQuery(tagQuery)).orElse(Lists.newArrayList());
        if (tagDOS.size() > 0) {
            tagVO = TagVO.buildTagVO(tagDOS.get(0));
        }
        return tagVO;
    }

    @Override
    public List<TagVO> findByParentTagId(final String parentTagId) {
        TagQuery tagQuery = new TagQuery();
        tagQuery.setParentTagId(parentTagId);
        List<TagDO> tagRelationDOS = Optional.ofNullable(tagMapper.selectByQuery(tagQuery)).orElse(Lists.newArrayList());
        return tagRelationDOS.stream().map(TagVO::buildTagVO).collect(Collectors.toList());
    }
}
