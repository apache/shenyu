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
import org.apache.shenyu.admin.mapper.ApiMapper;
import org.apache.shenyu.admin.mapper.TagMapper;
import org.apache.shenyu.admin.mapper.TagRelationMapper;
import org.apache.shenyu.admin.model.dto.ApiDTO;
import org.apache.shenyu.admin.model.entity.ApiDO;
import org.apache.shenyu.admin.model.entity.TagDO;
import org.apache.shenyu.admin.model.entity.TagRelationDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.ApiQuery;
import org.apache.shenyu.admin.model.query.TagRelationQuery;
import org.apache.shenyu.admin.model.vo.ApiVO;
import org.apache.shenyu.admin.model.vo.TagVO;
import org.apache.shenyu.admin.service.ApiService;
import org.apache.shenyu.admin.utils.ListUtil;
import org.apache.shenyu.admin.utils.ShenyuResultMessage;
import org.apache.shenyu.common.constant.AdminConstants;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Implementation of the {@link org.apache.shenyu.admin.service.ApiService}.
 */
@Service
public class ApiServiceImpl implements ApiService {

    private final ApiMapper apiMapper;

    private final TagRelationMapper tagRelationMapper;

    private final TagMapper tagMapper;

    public ApiServiceImpl(final ApiMapper apiMapper, final TagRelationMapper tagRelationMapper,
                          final TagMapper tagMapper) {
        this.apiMapper = apiMapper;
        this.tagRelationMapper = tagRelationMapper;
        this.tagMapper = tagMapper;

    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String createOrUpdate(final ApiDTO apiDTO) {
        return StringUtils.isBlank(apiDTO.getId()) ? this.create(apiDTO) : this.update(apiDTO);
    }

    /**
     * update.
     *
     * @param apiDTO apiDTO
     * @return update message
     */
    private String update(final ApiDTO apiDTO) {
        ApiDO apiDO = ApiDO.buildApiDO(apiDTO);
        final int updateRows = apiMapper.updateByPrimaryKeySelective(apiDO);
        if (CollectionUtils.isNotEmpty(apiDTO.getTagIds()) && updateRows > 0) {
            List<String> tagIds = apiDTO.getTagIds();
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            List<TagRelationDO> tags = tagIds.stream().map(tagId -> TagRelationDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .apiId(apiDO.getId())
                .tagId(tagId)
                .dateUpdated(currentTime)
                .build()).collect(Collectors.toList());
            tagRelationMapper.deleteByApiId(apiDO.getId());
            tagRelationMapper.batchInsert(tags);
        }
        return ShenyuResultMessage.UPDATE_SUCCESS;
    }

    /**
     * create.
     *
     * @param apiDTO apiDTO
     * @return create message
     */
    private String create(final ApiDTO apiDTO) {
        ApiDO apiDO = ApiDO.buildApiDO(apiDTO);
        final int insertRows = apiMapper.insertSelective(apiDO);
        if (CollectionUtils.isNotEmpty(apiDTO.getTagIds()) && insertRows > 0) {
            List<String> tagIds = apiDTO.getTagIds();
            Timestamp currentTime = new Timestamp(System.currentTimeMillis());
            List<TagRelationDO> tags = tagIds.stream().map(tagId -> TagRelationDO.builder()
                .id(UUIDUtils.getInstance().generateShortUuid())
                .apiId(apiDO.getId())
                .tagId(tagId)
                .dateCreated(currentTime)
                .dateUpdated(currentTime)
                .build()).collect(Collectors.toList());
            tagRelationMapper.batchInsert(tags);
        }
        return ShenyuResultMessage.CREATE_SUCCESS;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public String delete(final List<String> ids) {
        // select api id.
        List<ApiDO> apis = this.apiMapper.selectByIds(ids);
        if (CollectionUtils.isEmpty(apis)) {
            return AdminConstants.SYS_API_ID_NOT_EXIST;
        }
        // delete apis.
        final List<String> apiIds = ListUtil.map(apis, ApiDO::getId);
        final int deleteRows = this.apiMapper.deleteByIds(apiIds);
        if (deleteRows > 0) {
            tagRelationMapper.deleteByApiIds(apiIds);
        }
        return StringUtils.EMPTY;
    }

    @Override
    public ApiVO findById(final String id) {
        return Optional.ofNullable(apiMapper.selectByPrimaryKey(id)).map(item -> {
            List<TagRelationDO> tagRelations = tagRelationMapper.selectByQuery(TagRelationQuery.builder().apiId(item.getId()).build());
            List<String> tagIds = tagRelations.stream().map(TagRelationDO::getTagId).collect(Collectors.toList());
            List<TagVO> tagVOS = Lists.newArrayList();
            if (CollectionUtils.isNotEmpty(tagIds)) {
                List<TagDO> tagDOS = tagMapper.selectByIds(tagIds);
                tagVOS = tagDOS.stream().map(TagVO::buildTagVO).collect(Collectors.toList());
            }
            return ApiVO.buildApiVO(item, tagVOS);
        }).orElse(null);
    }

    @Override
    public CommonPager<ApiVO> listByPage(final ApiQuery apiQuery) {
        return PageResultUtils.result(apiQuery.getPageParameter(), () -> apiMapper.selectByQuery(apiQuery)
                .stream().map(item -> {
                    List<TagRelationDO> tagRelations = tagRelationMapper.selectByQuery(TagRelationQuery.builder().apiId(item.getId()).build());
                    List<String> tagIds = tagRelations.stream().map(TagRelationDO::getTagId).collect(Collectors.toList());
                    List<TagVO> tagVOS = Lists.newArrayList();
                    if (CollectionUtils.isNotEmpty(tagIds)) {
                        List<TagDO> tagDOS = tagMapper.selectByIds(tagIds);
                        tagVOS = tagDOS.stream().map(TagVO::buildTagVO).collect(Collectors.toList());
                    }
                    return ApiVO.buildApiVO(item, tagVOS);
                }).collect(Collectors.toList()));
    }

    @Override
    public int deleteByApiPathHttpMethodRpcType(final String apiPath, final Integer httpMethod, final String rpcType) {
        List<ApiDO> apiDOs = apiMapper.selectByApiPathHttpMethodRpcType(apiPath, httpMethod, rpcType);
        // delete apis.
        if (CollectionUtils.isNotEmpty(apiDOs)) {
            final List<String> apiIds = ListUtil.map(apiDOs, ApiDO::getId);
            final int deleteRows = this.apiMapper.deleteByIds(apiIds);
            if (deleteRows > 0) {
                tagRelationMapper.deleteByApiIds(apiIds);
            }
            return deleteRows;
        }
        return 0;
    }

    @Override
    public String offlineByContextPath(final String contextPath) {
        apiMapper.updateOfflineByContextPath(contextPath);
        return ShenyuResultMessage.SUCCESS;
    }
}
