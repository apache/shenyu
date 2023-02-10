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
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.shenyu.admin.mapper.TagMapper;
import org.apache.shenyu.admin.model.dto.TagDTO;
import org.apache.shenyu.admin.model.entity.BaseDO;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.query.TagQuery;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.TagService;
import org.apache.shenyu.admin.utils.Assert;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.GsonUtils;
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
        Assert.notNull(tagDTO, "tagDTO is not allowed null");
        Assert.notNull(tagDTO.getParentTagId(), "parent tag id is not allowed null");
        String ext = "";
        if (!tagDTO.getParentTagId().equals(AdminConstants.TAG_ROOT_PARENT_ID)) {
            TagDO tagDO = tagMapper.selectByPrimaryKey(tagDTO.getParentTagId());
            ext = buildExtParamByParentTag(tagDO);
        }
        TagDO tagDO = TagDO.buildTagDO(tagDTO);
        tagDO.setExt(ext);
        return tagMapper.insert(tagDO);
    }

    @Override
    public int update(final TagDTO tagDTO) {
        TagDO before = tagMapper.selectByPrimaryKey(tagDTO.getId());
        Assert.notNull(before, "the updated tag is not found");
        TagDO tagDO = TagDO.buildTagDO(tagDTO);
        updateSubTags(tagDTO);
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
    public List<TagVO> findByQuery(final String tagName) {
        TagQuery tagQuery = new TagQuery();
        tagQuery.setName(tagName);
        List<TagDO> tagDOS = Optional.ofNullable(tagMapper.selectByQuery(tagQuery)).orElse(Lists.newArrayList());
        return tagDOS.stream().map(TagVO::buildTagVO).collect(Collectors.toList());
    }

    @Override
    public List<TagVO> findByParentTagId(final String parentTagId) {
        TagQuery tagQuery = new TagQuery();
        tagQuery.setParentTagId(parentTagId);
        List<TagDO> tagDOS = tagMapper.selectByQuery(tagQuery);
        if (CollectionUtils.isEmpty(tagDOS)) {
            return Lists.newArrayList();
        }
        List<String> rootIds = tagDOS.stream().map(TagDO::getId).collect(Collectors.toList());
        List<TagDO> tagDOList = tagMapper.selectByParentTagIds(rootIds);
        Map<String, Boolean> map = tagDOList.stream().collect(
            Collectors.toMap(TagDO::getParentTagId, tagDO -> true, (a, b) -> b, ConcurrentHashMap::new));
        return tagDOS.stream().map(tag -> {
            TagVO tagVO = TagVO.buildTagVO(tag);
            if (map.get(tag.getId()) != null) {
                tagVO.setHasChildren(map.get(tag.getId()));
            }
            return tagVO;
        }).collect(Collectors.toList());
    }

    /**
     * update sub tags.
     * @param tagDTO tagDTO
     */
    private void updateSubTags(final TagDTO tagDTO) {
        List<TagDO> allData = tagMapper.selectByQuery(new TagQuery());
        Map<String, TagDO> allDataMap = allData.stream().collect(
            Collectors.toMap(BaseDO::getId, Function.identity(), (a, b) -> b, ConcurrentHashMap::new));
        TagDO update = TagDO.buildTagDO(tagDTO);
        allDataMap.put(update.getId(), update);
        Map<String, List<String>> relationMap = new ConcurrentHashMap<>();
        allDataMap.keySet().stream().map(allDataMap::get).forEach(tagDO -> {
            if (CollectionUtils.isEmpty(relationMap.get(tagDO.getParentTagId()))) {
                relationMap.put(tagDO.getParentTagId(), Lists.newArrayList(tagDO.getId()));
            } else {
                List<String> list = relationMap.get(tagDO.getParentTagId());
                list.add(tagDO.getId());
                relationMap.put(tagDO.getParentTagId(), list);
            }
        });
        recurseUpdateTag(allDataMap, relationMap, tagDTO.getId());
    }

    /**
     * recurseUpdateTag.
     * @param allData allData
     * @param relationMap relationMap
     * @param id id
     */
    private void recurseUpdateTag(final Map<String, TagDO> allData, final Map<String, List<String>> relationMap, final String id) {
        if (CollectionUtils.isEmpty(relationMap.get(id))) {
            return;
        }
        List<String> subTagIds = relationMap.get(id);
        subTagIds.stream().forEach(tagId -> {
            TagDO tagDO = allData.get(tagId);
            tagDO.setExt(buildExtParamByParentTag(allData.get(id)));
            tagMapper.updateByPrimaryKey(tagDO);
            recurseUpdateTag(allData, relationMap, tagId);
        });
    }

    /**
     * buildExtParam.
     * @param parentTagDO parentTagDO
     * @return ext
     */
    private String buildExtParamByParentTag(final TagDO parentTagDO) {
        String ext = "";
        if (parentTagDO.getId().equals(AdminConstants.TAG_ROOT_PARENT_ID)) {
            final TagDO.TagExt parent = new TagDO.TagExt();
            TagDO.TagExt tagExt = new TagDO.TagExt();
            tagExt.setDesc(parentTagDO.getTagDesc());
            tagExt.setName(parentTagDO.getName());
            tagExt.setId(parentTagDO.getId());
            parent.setParent(tagExt);
            ext = GsonUtils.getInstance().toJson(parent);
        } else {
            TagDO.TagExt parentTagExt = Optional.ofNullable(GsonUtils.getInstance().fromJson(parentTagDO.getExt(), TagDO.TagExt.class)).orElse(new TagDO.TagExt());
            final TagDO.TagExt tagExt = new TagDO.TagExt();
            parentTagExt.setDesc(parentTagDO.getTagDesc());
            parentTagExt.setName(parentTagDO.getName());
            parentTagExt.setId(parentTagDO.getId());
            tagExt.setParent(parentTagExt);
            ext = GsonUtils.getInstance().toJson(tagExt);
        }
        return ext;
    }
}
