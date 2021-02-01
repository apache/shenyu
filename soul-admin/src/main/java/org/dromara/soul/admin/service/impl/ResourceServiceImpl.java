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

import org.dromara.soul.admin.dto.ResourceDTO;
import org.dromara.soul.admin.entity.ResourceDO;
import org.dromara.soul.admin.mapper.ResourceMapper;
import org.dromara.soul.admin.page.CommonPager;
import org.dromara.soul.admin.page.PageResultUtils;
import org.dromara.soul.admin.query.ResourceQuery;
import org.dromara.soul.admin.service.ResourceService;
import org.dromara.soul.admin.vo.ResourceVO;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import reactor.util.StringUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * this Resource Service Impl.
 *
 * @author nuo-promise
 */
@Service("resourceService")
public class ResourceServiceImpl implements ResourceService {

    private final ResourceMapper resourceMapper;

    @Autowired(required = false)
    public ResourceServiceImpl(final ResourceMapper resourceMapper) {
        this.resourceMapper = resourceMapper;
    }

    /**
     *  create or update resource.
     *
     * @param resourceDTO {@linkplain ResourceDTO}
     * @return rows int
     */
    @Override
    public int createOrUpdate(final ResourceDTO resourceDTO) {
        ResourceDO resourceDO = ResourceDO.buildResourceDO(resourceDTO);
        if (StringUtils.isEmpty(resourceDTO.getId())) {
            return resourceMapper.insertSelective(resourceDO);
        } else {
            return resourceMapper.updateSelective(resourceDO);
        }
    }

    /**
     * delete resource info.
     *
     * @param ids {@linkplain List}
     * @return rows int
     */
    @Override
    public int delete(final List<String> ids) {
        return resourceMapper.delete(ids);
    }

    /**
     * find resource info by id.
     *
     * @param id resource id
     * @return {@linkplain ResourceVO}
     */
    @Override
    public ResourceVO findById(final String id) {
        return ResourceVO.buildResourceVO(resourceMapper.selectById(id));
    }

    /**
     * find page of resource by query.
     *
     * @param resourceQuery {@linkplain ResourceQuery}
     * @return {@linkplain CommonPager}
     */
    @Override
    public CommonPager<ResourceVO> listByPage(final ResourceQuery resourceQuery) {
        return PageResultUtils.result(resourceQuery.getPageParameter(),
            () -> resourceMapper.countByQuery(resourceQuery),
            () -> resourceMapper.selectByQuery(resourceQuery)
                            .stream()
                            .map(ResourceVO::buildResourceVO)
                            .collect(Collectors.toList()));
    }

}
