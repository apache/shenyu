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
import org.apache.shenyu.admin.mapper.FieldMapper;
import org.apache.shenyu.admin.model.dto.FieldDTO;
import org.apache.shenyu.admin.model.entity.FieldDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.FieldQuery;
import org.apache.shenyu.admin.model.vo.FieldVO;
import org.apache.shenyu.admin.service.FieldService;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class FieldServiceImpl implements FieldService {

    private final FieldMapper fieldMapper;

    public FieldServiceImpl(final FieldMapper fieldMapper) {
        this.fieldMapper = fieldMapper;
    }

    @Override
    public int createOrUpdate(final FieldDTO fieldDTO) {
        return StringUtils.isBlank(fieldDTO.getId()) ? this.create(fieldDTO) : this.update(fieldDTO);
    }

    @Override
    public int delete(final String id) {
        return fieldMapper.deleteByPrimaryKey(id);
    }

    @Override
    public int deleteBatch(final List<String> ids) {
        return fieldMapper.batchDelete(ids);
    }

    @Override
    public FieldVO findById(final String id) {
        FieldDO fieldDO = fieldMapper.selectByPrimaryKey(id);
        FieldVO.FieldVOBuilder builder = FieldVO.builder();
        if (fieldDO != null) {
            builder.id(fieldDO.getId())
                    .ext(fieldDO.getExt())
                    .fieldDesc(fieldDO.getFieldDesc())
                    .name(fieldDO.getName())
                    .modelId(fieldDO.getModelId())
                    .required(fieldDO.getRequired())
                    .selfModelId(fieldDO.getSelfModelId())
                    .dateUpdated(fieldDO.getDateUpdated())
                    .dateCreated(fieldDO.getDateCreated());
        }
        return builder.build();
    }

    @Override
    @Pageable
    public CommonPager<FieldVO> listByPage(final FieldQuery fieldQuery) {
        List<FieldDO> list = fieldMapper.selectByQuery(fieldQuery);
        return PageResultUtils.result(fieldQuery.getPageParameter(), () -> list.stream().map(FieldVO::buildFieldVO).collect(Collectors.toList()));
    }

    private int create(final FieldDTO fieldDTO) {
        if (fieldDTO == null) {
            return 0;
        }
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        FieldDO fieldDO = FieldDO.builder()
                .id(fieldDTO.getId())
                .ext(fieldDTO.getExt())
                .fieldDesc(fieldDTO.getFieldDesc())
                .name(fieldDTO.getName())
                .modelId(fieldDTO.getModelId())
                .required(fieldDTO.getRequired())
                .selfModelId(fieldDTO.getSelfModelId())
                .dateUpdated(currentTime)
                .dateCreated(currentTime)
                .build();
        if (StringUtils.isEmpty(fieldDO.getId())) {
            fieldDO.setId(UUIDUtils.getInstance().generateShortUuid());
        }
        return fieldMapper.insert(fieldDO);
    }

    private int update(final FieldDTO fieldDTO) {
        if (fieldDTO == null || fieldDTO.getId() == null) {
            return 0;
        }
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        FieldDO fieldDO = FieldDO.builder()
                .id(fieldDTO.getId())
                .ext(fieldDTO.getExt())
                .fieldDesc(fieldDTO.getFieldDesc())
                .name(fieldDTO.getName())
                .modelId(fieldDTO.getModelId())
                .required(fieldDTO.getRequired())
                .selfModelId(fieldDTO.getSelfModelId())
                .dateUpdated(currentTime)
                .build();
        return fieldMapper.updateByPrimaryKeySelective(fieldDO);
    }
}
