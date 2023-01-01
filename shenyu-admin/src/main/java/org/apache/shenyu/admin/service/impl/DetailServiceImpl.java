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
import org.apache.shenyu.admin.mapper.DetailMapper;
import org.apache.shenyu.admin.model.dto.DetailDTO;
import org.apache.shenyu.admin.model.entity.DetailDO;
import org.apache.shenyu.admin.model.page.CommonPager;
import org.apache.shenyu.admin.model.page.PageResultUtils;
import org.apache.shenyu.admin.model.query.DetailQuery;
import org.apache.shenyu.admin.model.vo.DetailVO;
import org.apache.shenyu.admin.service.DetailService;
import org.apache.shenyu.common.utils.UUIDUtils;
import org.springframework.stereotype.Service;

import java.sql.Timestamp;
import java.util.List;
import java.util.stream.Collectors;

@Service
public class DetailServiceImpl implements DetailService {

    private final DetailMapper detailMapper;

    public DetailServiceImpl(final DetailMapper detailMapper) {
        this.detailMapper = detailMapper;
    }

    @Override
    public int createOrUpdate(final DetailDTO detailDTO) {
        return StringUtils.isBlank(detailDTO.getId()) ? this.create(detailDTO) : this.update(detailDTO);
    }

    @Override
    public int delete(final String id) {
        return detailMapper.deleteByPrimaryKey(id);
    }

    @Override
    public int deleteBatch(final List<String> ids) {
        return detailMapper.batchDelete(ids);
    }

    @Override
    public DetailVO findById(final String id) {
        DetailDO detailDO = detailMapper.selectByPrimaryKey(id);
        DetailVO.DetailVOBuilder builder = DetailVO.builder();
        if (detailDO != null) {
            builder.id(detailDO.getId())
                    .example(detailDO.getExample())
                    .valueDesc(detailDO.getValueDesc())
                    .fieldValue(detailDO.getFieldValue())
                    .fieldId(detailDO.getFieldId())
                    .dateUpdated(detailDO.getDateUpdated())
                    .dateCreated(detailDO.getDateCreated());
        }
        return builder.build();
    }

    @Override
    @Pageable
    public CommonPager<DetailVO> listByPage(final DetailQuery detailQuery) {
        return PageResultUtils.result(detailQuery.getPageParameter(), () -> detailMapper.selectByQuery(detailQuery)
                .stream()
                .map(DetailVO::buildDetailVO)
                .collect(Collectors.toList()));
    }

    private int create(final DetailDTO detailDTO) {
        if (detailDTO == null) {
            return 0;
        }
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        DetailDO detailDO = DetailDO.builder()
                .id(detailDTO.getId())
                .example(detailDTO.getExample())
                .valueDesc(detailDTO.getValueDesc())
                .fieldValue(detailDTO.getFieldValue())
                .fieldId(detailDTO.getFieldId())
                .dateUpdated(currentTime)
                .dateCreated(currentTime)
                .build();
        if (StringUtils.isEmpty(detailDO.getId())) {
            detailDO.setId(UUIDUtils.getInstance().generateShortUuid());
        }
        return detailMapper.insert(detailDO);
    }

    private int update(final DetailDTO detailDTO) {
        if (detailDTO == null || detailDTO.getId() == null) {
            return 0;
        }
        Timestamp currentTime = new Timestamp(System.currentTimeMillis());
        DetailDO detailDO = DetailDO.builder()
                .id(detailDTO.getId())
                .example(detailDTO.getExample())
                .valueDesc(detailDTO.getValueDesc())
                .fieldValue(detailDTO.getFieldValue())
                .fieldId(detailDTO.getFieldId())
                .dateUpdated(currentTime)
                .dateUpdated(currentTime)
                .build();
        return detailMapper.updateByPrimaryKeySelective(detailDO);
    }
}
