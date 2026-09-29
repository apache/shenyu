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
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.shenyu.admin.jpa.repository.TagRelationRepository;
import org.apache.shenyu.admin.jpa.repository.TagRepository;
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
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.List;
import java.util.HashSet;
import java.util.Set;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.TagService}.
 */
@Service
public class TagServiceImpl implements TagService {

    private final TagRepository tagRepository;

    private final TagRelationRepository tagRelationRepository;

    public TagServiceImpl(final TagRepository tagRepository, final TagRelationRepository tagRelationRepository) {
        this.tagRepository = tagRepository;
        this.tagRelationRepository = tagRelationRepository;
    }

    @Override
    public int create(final TagDTO tagDTO) {
        tagDTO.setParentTagId(StringUtils.isNotEmpty(tagDTO.getParentTagId()) ? tagDTO.getParentTagId() : AdminConstants.TAG_ROOT_PARENT_ID);
        return createInner(tagDTO, null);
    }

    @Override
    public int createRootTag(final TagDTO tagDTO, final TagDO.TagExt tagExt) {
        Assert.notNull(tagDTO, "tagDTO is not allowed null");
        tagDTO.setParentTagId(StringUtils.isNotEmpty(tagDTO.getParentTagId()) ? tagDTO.getParentTagId() : AdminConstants.TAG_ROOT_PARENT_ID);
        return createInner(tagDTO, tagExt);
    }

    private int createInner(final TagDTO tagDTO, final TagDO.TagExt tagExt) {
        Assert.notNull(tagDTO, "tagDTO is not allowed null");
        Assert.notNull(tagDTO.getParentTagId(), "parent tag id is not allowed null");
        String ext = "";
        if (!tagDTO.getParentTagId().equals(AdminConstants.TAG_ROOT_PARENT_ID)) {
            TagDO tagDO = tagRepository.findById(tagDTO.getParentTagId()).orElse(null);
            Assert.notNull(tagDO, "parent tag is not found");
            ext = buildExtParamByParentTag(tagDO);
        } else {
            ext = GsonUtils.getInstance().toJson(Optional.ofNullable(tagExt).orElse(new TagDO.TagExt()));
        }
        TagDO tagDO = TagDO.buildTagDO(tagDTO);
        tagDO.setExt(ext);
        tagDTO.setId(tagDO.getId());
        tagRepository.save(tagDO);
        return 1;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int update(final TagDTO tagDTO) {
        TagDO before = tagRepository.findById(tagDTO.getId()).orElse(null);
        Assert.notNull(before, "the updated tag is not found");
        TagDO tagDO = TagDO.buildTagDO(tagDTO);
        updateSubTags(tagDTO);
        return tagRepository.findById(tagDTO.getId())
                .map(persisted -> {
                    if (Objects.nonNull(tagDO.getTagName())) {
                        persisted.setName(tagDO.getTagName());
                    }
                    if (Objects.nonNull(tagDO.getTagDesc())) {
                        persisted.setTagDesc(tagDO.getTagDesc());
                    }
                    if (Objects.nonNull(tagDO.getParentTagId())) {
                        persisted.setParentTagId(tagDO.getParentTagId());
                    }
                    if (Objects.nonNull(tagDO.getExt())) {
                        persisted.setExt(tagDO.getExt());
                    }
                    persisted.setDateUpdated(tagDO.getDateUpdated());
                    tagRepository.save(persisted);
                    return 1;
                })
                .orElse(0);
    }

    @Override
    public int updateTagExt(final String tagId, final TagDO.TagExt tagExt) {
        Assert.notNull(tagId, "tagId is not null");
        Assert.notNull(tagExt, "tagDO is not null");
        return tagRepository.findById(tagId)
                .map(persisted -> {
                    persisted.setExt(GsonUtils.getInstance().toJson(tagExt));
                    persisted.setDateUpdated(new Timestamp(System.currentTimeMillis()));
                    tagRepository.save(persisted);
                    return 1;
                })
                .orElse(0);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int delete(final List<String> ids) {
        if (CollectionUtils.isEmpty(ids)) {
            return 0;
        }
        Assert.isTrue(tagRepository.findByParentTagIdIn(ids).stream().allMatch(tag -> ids.contains(tag.getId())),
                "cannot delete tags with remaining children");
        tagRelationRepository.deleteByTagIds(ids);
        return tagRepository.deleteByIds(ids);
    }

    @Override
    public TagVO findById(final String id) {
        return TagVO.buildTagVO(tagRepository.findById(id).orElse(null));
    }

    @Override
    public List<TagVO> findByQuery(final String tagName) {
        return this.findByQuery(tagName, null);
    }

    @Override
    public List<TagVO> findByQuery(final String tagName, final String parentTagId) {
        TagQuery tagQuery = new TagQuery();
        tagQuery.setTagName(tagName);
        tagQuery.setParentTagId(parentTagId);
        List<TagDO> tagDOS = Optional.ofNullable(tagRepository.selectByQuery(tagQuery)).orElse(Lists.newArrayList());
        return tagDOS.stream().map(TagVO::buildTagVO).collect(Collectors.toList());
    }

    @Override
    public List<TagVO> findByParentTagId(final String parentTagId) {
        List<TagDO> tagDOS = tagRepository.findByParentTagId(parentTagId);
        if (CollectionUtils.isEmpty(tagDOS)) {
            return Lists.newArrayList();
        }
        List<String> rootIds = tagDOS.stream().map(TagDO::getId).collect(Collectors.toList());
        List<TagDO> tagDOList = tagRepository.findByParentTagIdIn(rootIds);
        Map<String, Boolean> map = tagDOList.stream().collect(
                Collectors.toMap(TagDO::getParentTagId, tagDO -> true, (a, b) -> b, ConcurrentHashMap::new));
        return tagDOS.stream().map(tag -> {
            TagVO tagVO = TagVO.buildTagVO(tag);
            if (Objects.nonNull(map.get(tag.getId()))) {
                tagVO.setHasChildren(map.get(tag.getId()));
            }
            return tagVO;
        }).collect(Collectors.toList());
    }

    /**
     * update sub tags.
     *
     * @param tagDTO tagDTO
     */
    private void updateSubTags(final TagDTO tagDTO) {
        List<TagDO> allData = tagRepository.findAll();
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
        recurseUpdateTag(allDataMap, relationMap, tagDTO.getId(), new HashSet<>());
    }

    /**
     * recurseUpdateTag.
     *
     * @param allData     allData
     * @param relationMap relationMap
     * @param id          id
     * @param visited     tags already visited during this update
     */
    private void recurseUpdateTag(final Map<String, TagDO> allData, final Map<String, List<String>> relationMap, final String id, final Set<String> visited) {
        Assert.isTrue(visited.add(id), "Cyclic tag hierarchy detected at tag: " + id);
        if (CollectionUtils.isEmpty(relationMap.get(id))) {
            return;
        }
        List<String> subTagIds = relationMap.get(id);
        subTagIds.forEach(tagId -> {
            TagDO tagDO = allData.get(tagId);
            tagDO.setExt(buildExtParamByParentTag(allData.get(id)));
            tagRepository.save(tagDO);
            recurseUpdateTag(allData, relationMap, tagId, visited);
        });
    }

    /**
     * buildExtParam.
     *
     * @param parentTagDO parentTagDO
     * @return ext
     */
    private String buildExtParamByParentTag(final TagDO parentTagDO) {
        String ext = "";
        if (parentTagDO.getId().equals(AdminConstants.TAG_ROOT_PARENT_ID)) {
            final TagDO.TagExt parent = new TagDO.TagExt();
            TagDO.TagExt tagExt = new TagDO.TagExt();
            tagExt.setDesc(parentTagDO.getTagDesc());
            tagExt.setName(parentTagDO.getTagName());
            tagExt.setId(parentTagDO.getId());
            tagExt.setRefreshTime(parent.getRefreshTime());
            tagExt.setApiDocMd5(parent.getApiDocMd5());
            parent.setParent(tagExt);
            ext = GsonUtils.getInstance().toJson(parent);
        } else {
            TagDO.TagExt parentTagExt = Optional.ofNullable(GsonUtils.getInstance().fromJson(parentTagDO.getExt(), TagDO.TagExt.class)).orElse(new TagDO.TagExt());
            final TagDO.TagExt tagExt = new TagDO.TagExt();
            parentTagExt.setDesc(parentTagDO.getTagDesc());
            parentTagExt.setName(parentTagDO.getTagName());
            parentTagExt.setId(parentTagDO.getId());
            tagExt.setParent(parentTagExt);
            ext = GsonUtils.getInstance().toJson(tagExt);
        }
        return ext;
    }
}
