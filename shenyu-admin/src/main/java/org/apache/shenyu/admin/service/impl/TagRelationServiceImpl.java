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
import org.apache.shenyu.admin.jpa.repository.TagRelationRepository;
import org.apache.shenyu.admin.mapper.TagRelationMapper;
import org.apache.shenyu.admin.model.dto.TagRelationDTO;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.admin.service.TagRelationService;
import org.apache.shenyu.admin.utils.Assert;
import org.springframework.stereotype.Service;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.TagRelationService}.
 */
@Service
public class TagRelationServiceImpl implements TagRelationService {

    private final TagRelationMapper tagRelationMapper;

    private final TagRelationRepository tagRelationRepository;

    public TagRelationServiceImpl(final TagRelationMapper tagRelationMapper, final TagRelationRepository tagRelationRepository) {
        this.tagRelationMapper = tagRelationMapper;
        this.tagRelationRepository = tagRelationRepository;
    }

    @Override
    public int create(final TagRelationDTO tagRelationDTO) {
        TagRelationDO tagRelationDO = TagRelationDO.buildTagRelationDO(tagRelationDTO);
        return tagRelationMapper.insert(tagRelationDO);
    }

    @Override
    public int update(final TagRelationDTO tagRelationDTO) {
        TagRelationDO before = tagRelationMapper.selectByPrimaryKey(tagRelationDTO.getId());
        Assert.notNull(before, "the updated rule is not found");
        TagRelationDO tagRelationDO = TagRelationDO.buildTagRelationDO(tagRelationDTO);
        return tagRelationMapper.updateByPrimaryKeySelective(tagRelationDO);
    }

    @Override
    public int delete(final List<String> ids) {
        return tagRelationMapper.deleteByIds(ids);
    }

    @Override
    public TagRelationDO findById(final String id) {
        return tagRelationRepository.findById(id).orElse(null);
    }

    @Override
    public List<TagRelationDO> findByTagId(final String tagId) {
        return Optional.ofNullable(tagRelationRepository.findByTagId(tagId)).orElse(Lists.newArrayList());
    }

    @Override
    public List<TagRelationDO> findApiId(final String apiId) {
        return Optional.ofNullable(tagRelationRepository.findByApiId(apiId)).orElse(Lists.newArrayList());
    }
}
